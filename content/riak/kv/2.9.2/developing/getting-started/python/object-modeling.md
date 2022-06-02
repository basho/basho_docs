---
title_supertext: "Getting Started:"
title: "Object Modeling with Python"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Object Modeling"
    identifier: "getting_started_python_object"
    weight: 102
    parent: "getting_started_python"
toc: true
aliases:
  - /riak/2.9.2/dev/taste-of-riak/object-modeling-python
  - /riak/kv/2.9.2/dev/taste-of-riak/object-modeling-python
---

To get started, let's create the data structures that we'll be using.

```python
from datetime import datetime
import string
import riak


marleen = {'user_name': 'marleenmgr',
           'full_name': 'Marleen Manager',
           'email': 'marleen.manager@basho.com'}

joe = {'user_name': 'joeuser',
       'full_name': 'Joe User',
       'email': 'joe.user@basho.com'}

msg = {'sender': marleen['user_name'],
       'recipient': joe['user_name'],
       'created': datetime.utcnow().isoformat(),
       'text': 'Welcome to the company!'}
```

As you can see, we first create a user, and then we can use that user to
create a message. To send this message we can append it to one or more
`Timeline`s. If it's a private message, we'll append it to the
Recipient's `Inbox` timeline and the User's own `Sent` timeline. If it's
a group message, we'll append it to the Group's timeline, as well as to
the User's `Sent` timeline.

#### Buckets and Keys Revisited

Now that we've worked out how we will differentiate data in the system,
let's figure out our bucket and key names.

The bucket names are straightforward. We can use `Users`, `Msgs`, and
`Timelines`. The key names, however, are a little more tricky. In past
examples we've used sequential integers, but this presents a problem: we
would need a secondary service to hand out these IDs. This service could
easily be a future bottleneck in the system, so let's use a natural key.
Natural keys are a great fit for key/value systems because both humans
and computers can easily construct them when needed, and most of the
time they can be made unique enough for a KV store.


Bucket | Key Pattern | Example Key
:------|:------------|:-----------
`Users` | `<user_name>` | `joeuser`
`Msgs` | `<username>_<datetime>` | `joeuser_2014-03-06T02:05:13.223556Z`
`Timelines` | `<username>_<type>_<date>` | `joeuser_Sent_2014-03-06`<br /> `marketing_group_Inbox_2014-03-06` |

For the `Users` bucket, we can be certain that we will want each
username to be unique, so let's use the `username` as the key.  For the
`Msgs` bucket, let's use a combination of the username and the posting
datetime in an [ISO 8601 Long](http://en.wikipedia.org/wiki/ISO_8601)
format. This combination gives us the pattern `<username>_<datetime>`,
which produces keys like `joeuser_2014-03-05T23:20:28`.

Now for `Timelines`, we need to differentiate between `Inbox` and `Sent`
timelines, so we can simply add that type into the key name. We will
also want to partition each collection object into some time period,
that way the object doesn't grow too large (see note below).

For `Timelines`, let's use the pattern `<username>_<type>_<date>` for
users, and `<groupname>_<type>_<date>` for groups, which will look like
`joeuser_Sent_2014-03-06` or `marketing_group_Inbox_2014-03-06`,
respectively.

{{% note title="Note" %}}
Riak performs best with objects under 1-2MB. Objects larger than that can hurt
performance, especially if many siblings are being created. We will cover
siblings, sibling resolution, and sibling explosions in the next chapter.
{{% /note %}}

#### Keeping our story straight with repositories

Now that we've figured out our object model, let's write some
repositories to help create and work with these objects in Riak:

```python
class UserRepository:
    BUCKET = 'Users'

    def __init__(self, client):
        self.client = client

    def save(self, user):
        riak_obj = self.client.bucket(self.BUCKET).get(user['user_name'])
        riak_obj.data = user
        return riak_obj.store()

    def get(self, user_name):
        riak_obj = self.client.bucket(self.BUCKET).get(user_name)
        return riak_obj.data


class MsgRepository:
    BUCKET = 'Msgs'

    def __init__(self, client):
        self.client = client

    def save(self, msg):
        msgs = self.client.bucket(self.BUCKET)
        key = self._generate_key(msg)

        riak_obj = msgs.get(key)

        if not riak_obj.exists:
            riak_obj.data = msg
            riak_obj.store(if_none_match=True)

        return riak_obj

    def get(self, key):
        riak_obj = self.client.bucket(self.BUCKET).get(key)
        return riak_obj.data

    def _generate_key(self, msg):
        return msg['sender'] + '_' + msg['created']


class TimelineRepository:
    BUCKET = 'Timelines'
    SENT = 'Sent'
    INBOX = 'Inbox'

    def __init__(self, client):
        self.client = client
        self.msg_repo = MsgRepository(client)

    def post_message(self, msg):
        # Save the canonical copy
        saved_message = self.msg_repo.save(msg)
        msg_key = saved_message.key

        # Post to sender's Sent timeline
        self._add_to_timeline(msg, self.SENT, msg_key)

        # Post to recipient's Inbox timeline
        self._add_to_timeline(msg, self.INBOX, msg_key)

    def get_timeline(self, owner, msg_type, date):
        key = self._generate_key(owner, msg_type, date)
        riak_obj = self.client.bucket(self.BUCKET).get(key)
        return riak_obj.data

    def _add_to_timeline(self, msg, msg_type, msg_key):
        timeline_key = self._generate_key_from_msg(msg, msg_type)
        riak_obj = self.client.bucket(self.BUCKET).get(timeline_key)

        if riak_obj.exists:
            riak_obj = self._add_to_existing_timeline(riak_obj,
                                                      msg_key)
        else:
            riak_obj = self._create_new_timeline(riak_obj,
                                                 msg, msg_type,
                                                 msg_key)

        return riak_obj.store()

    def _create_new_timeline(self, riak_obj, msg, msg_type, msg_key):
        owner = self._get_owner(msg, msg_type)
        new_timeline = {'owner': owner,
                        'msg_type': msg_type,
                        'msgs': [msg_key]}

        riak_obj.data = new_timeline
        return riak_obj

    def _add_to_existing_timeline(self, riak_obj, msg_key):
        riak_obj.data['msgs'].append(msg_key)
        return riak_obj

    def _get_owner(self, msg, msg_type):
        if msg_type == self.INBOX:
            return msg['recipient']
        else:
            return msg['sender']

    def _generate_key_from_msg(self, msg, msg_type):
        owner = self._get_owner(msg, msg_type)
        return self._generate_key(owner, msg_type, msg['created'])

    def _generate_key(self, owner, msg_type, datetimestr):
        dateString = string.split(datetimestr, 'T', 1)[0]
        return owner + '_' + msg_type + '_' + dateString

```

Finally, let's test them:

```python
# Setup our repositories
client = riak.RiakClient(pb_port=10017, protocol='pbc')
userRepo = UserRepository(client)
msgsRepo = MsgRepository(client)
timelineRepo = TimelineRepository(client)

# Save users
userRepo.save(marleen)
userRepo.save(joe)

# Post msg to timelines
timelineRepo.post_message(msg)

# Get Joe's inbox for today, get first message
joes_inbox_today = timelineRepo.get_timeline(
    joe['user_name'],
    TimelineRepository.INBOX,
    datetime.utcnow().isoformat())

joes_first_message = msgsRepo.get(joes_inbox_today['msgs'][0])

print 'From: {0}\nMsg : {1}\n\n'.format(
    joes_first_message['sender'],
    joes_first_message['text'])

```

As you can see, the repository pattern helps us with a few things:

* It helps us to see if an object exists before creating a new one
* It keeps our buckets and key names consistent
* It provides us with a consistent interface to work with.

While this set of repositories solves many of our problems, it is very
minimal and doesn't cover all the edge cases. For instance, what happens
if two different people try to create a user with the same username?

We can also easily "compute" key names now, but how do we quickly look
up the last 10 messages a user sent? Many of these answers will be
application dependent. If your application shows the last 10 messages in
reverse order, for example, you may want to store that set of data in
another collection object to make lookup faster. There are drawbacks to
every solution, but we recommend seeking out the key/value-based
solution first, as it will likely be the quickest.

So to recap, in this chapter we learned:

* How to choose bucket names
* How to choose natural keys based on how we want to partition our data.

