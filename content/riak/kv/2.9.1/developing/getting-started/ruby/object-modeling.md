---
title_supertext: "Getting Started:"
title: "Object Modeling with Ruby"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Object Modeling"
    identifier: "getting_started_ruby_object"
    weight: 102
    parent: "getting_started_ruby"
toc: true
aliases:
  - /riak/2.9.1/dev/taste-of-riak/object-modeling-ruby
  - /riak/kv/2.9.1/dev/taste-of-riak/object-modeling-ruby
---

To get started, let's create the models that we'll be using. Since the
[Ruby Riak Client](https://github.com/basho/riak-ruby-client) uses
hashes when converting to and from JSON, we'll use the library
[Hashie](http://rdoc.info/github/intridea/hashie) to help automatically
coerce class properties to and from hashes. You can install this library
with `gem install hashie`.

```ruby
# Encoding: utf-8

require 'riak'
require 'hashie'
require 'time'

class User < Hashie::Dash
  property :user_name
  property :full_name
  property :email
end

class Msg < Hashie::Dash
  property :from
  property :to
  property :created
  property :text
end

class Timeline < Hashie::Dash
  property :owner
  property :type
  property :msgs
end
```

To use these classes to store data, we will first have to create a user.
Then, when a user creates a message, we will append that message to one
or more timelines. If it's a private message, we'll append it to the
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
`Timelines` | `<username>_<type>_<date>` | `joeuser_Sent_2014-03-06Z`<br /> `marketing_group_Inbox_2014-03-06Z` |

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
users, and `<groupname>_Inbox_<date>` for groups, which will look like
`joeuser_Sent_2014-03-06Z` or `marketing_group_Inbox_2014-03-05Z`,
respectively.

{{% note title="Note" %}}
Riak performs best with objects under 1-2MB. Objects larger than that can hurt
performance, especially many siblings are being created. We will cover
siblings, sibling resolution, and sibling explosions in the next chapter.
{{% /note %}}

#### Keeping our story straight with repositories

Now that we've figured out our object models, let's write some
repositories to help create and work with these objects in Riak:

```ruby
class UserRepository
  BUCKET = 'Users'

  def initialize(client)
    @client = client
  end

  def save(user)
    users = @client.bucket(BUCKET)
    key = user.user_name

    riak_obj = users.get_or_new(key)
    riak_obj.data = user
    riak_obj.content_type = 'application/json'
    riak_obj.store
  end

  def get(user_name)
    riak_obj = @client.bucket(BUCKET)[user_name]
    User.new(riak_obj.data)
  end
end

class MsgRepository
  BUCKET = 'Msgs'

  def initialize(client)
    @client = client
  end

  def save(msg)
    msgs = @client.bucket(BUCKET)
    key = generate_key(msg)

    return msgs.get(key) if msgs.exists?(key)
    riak_obj = msgs.new(key)
    riak_obj.data = msg
    riak_obj.content_type = 'application/json'
    riak_obj.prevent_stale_writes = true
    riak_obj.store(returnbody: true)
  end

  def get(key)
    riak_obj = @client.bucket(BUCKET).get(key)
    Msg.new(riak_obj.data)
  end

  def generate_key(msg)
    msg.from + '_' + msg.created.utc.iso8601(6)
  end
end

class TimelineRepository
  BUCKET = 'Timelines'
  SENT = 'Sent'
  INBOX = 'Inbox'

  def initialize(client)
    @client = client
    @msg_repo = MsgRepository.new(client)
  end

  def post_message(msg)
    # Save the canonical copy
    saved_message = @msg_repo.save(msg)
    # Post to sender's Sent timeline
    add_to_timeline(msg, SENT, saved_message.key)
    # Post to recipient's Inbox timeline
    add_to_timeline(msg, INBOX, saved_message.key)
  end

  def get_timeline(owner, type, date)
    riak_obj = @client.bucket(BUCKET).get(generate_key(owner, type, date))
    Timeline.new(riak_obj.data)
  end

  private

  def add_to_timeline(msg, type, msg_key)
    timeline_key = generate_key_from_msg(msg, type)
    riak_obj = nil

    if @client.bucket(BUCKET).exists?(timeline_key)
      riak_obj = add_to_existing_timeline(timeline_key, msg_key)
    else
      riak_obj = create_new_timeline(timeline_key, msg, type, msg_key)
    end

    riak_obj.store
  end

  def create_new_timeline(key, msg, type, msg_key)
    owner = get_owner(msg, type)
    riak_obj = @client.bucket(BUCKET).new(key)
    riak_obj.data = Timeline.new(owner: owner,
                                 type: type,
                                 msgs: [msg_key])
    riak_obj.content_type = 'application/json'
    riak_obj
  end

  def add_to_existing_timeline(key, msg_key)
    riak_obj = @client.bucket(BUCKET).get(key)
    timeline = Timeline.new(riak_obj.data)
    timeline.msgs << msg_key
    riak_obj.data = timeline
    riak_obj
  end

  def get_owner(msg, type)
    type == INBOX ? msg.to : msg.from
  end

  def generate_key_from_msg(msg, type)
    owner = get_owner(msg, type)
    generate_key(owner, type, msg.created)
  end

  def generate_key(owner, type, date)
    owner + '_' + type + '_' + date.utc.strftime('%F')
  end
end
```

Finally, let's test them:

```ruby
# Setup our repositories
client = Riak::Client.new(protocol: 'pbc', pb_port: 10017)
user_repo = UserRepository.new(client)
msgs_repo = MsgRepository.new(client)
timeline_repo = TimelineRepository.new(client)

# Create and save users
marleen = User.new(user_name: 'marleenmgr',
                   full_name: 'Marleen Manager',
                   email: 'marleen.manager@basho.com')

joe = User.new(user_name: 'joeuser',
               full_name: 'Joe User',
               email: 'joe.user@basho.com')

user_repo.save(marleen)
user_repo.save(joe)

# Create new Msg, post to timelines
msg = Msg.new(from: marleen.user_name,
              to: joe.user_name,
              created: Time.now,
              text: 'Welcome to the company!')

timeline_repo.post_message(msg)

# Get Joe's inbox for today, get first message
joes_inbox_today = timeline_repo.get_timeline(joe.user_name, 'Inbox', Time.now)
joes_first_message = msgs_repo.get(joes_inbox_today.msgs.first)

puts "From: #{joes_first_message.from}\nMsg : #{joes_first_message.text}"
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

