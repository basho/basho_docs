---
title_supertext: "Getting Started:"
title: "Object Modeling with Java"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Object Modeling"
    identifier: "getting_started_java_object"
    weight: 102
    parent: "getting_started_java"
toc: true
aliases:
  - /riak/2.9.8/dev/taste-of-riak/object-modeling-java
  - /riak/kv/2.9.8/dev/taste-of-riak/object-modeling-java
---

To get started, let's create the models that we'll be using.

```java
package com.basho.msgy.Models;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

public class Msg {
    public String Sender;
    public String Recipient;
    public String Created;
    public String Text;

    public static Msg createNew(String sender, String recipient, String text) {
        Msg msg = new Msg();
        msg.Sender = sender;
        msg.Recipient = recipient;
        msg.Text = text;
        msg.Created = GetCurrentISO8601Timestamp();
        return msg;
    }

    private static String GetCurrentISO8601Timestamp() {
        TimeZone tz = TimeZone.getTimeZone("UTC");
        // Java Dates don't have microsecond resolution :(
        // Pad out to microseconds to match other examples.
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'000'");
        df.setTimeZone(tz);
        return df.format(new Date());
    }
}

// ----------------------------------------------------------------------------

import java.util.ArrayList;

public class Timeline {

    public enum TimelineType
    {
        Inbox,
        Sent;

        @Override
        public String toString() {
            if(this == Inbox)
                return "Inbox";
            else
                return "Sent";
        }
    }

    public Timeline() {
        Msgs = new ArrayList<String>();
    }

    public String Owner;
    public String Type;
    public ArrayList<String> Msgs;
}

// ----------------------------------------------------------------------------

package com.basho.msgy.Models;

import com.basho.riak.client.convert.RiakKey;

public class User {
    @RiakKey
    public String UserName;

    @RiakBucketName
    final String bucketName = "msgs";

    public String FullName;
    public String Email;

    public User() {}

    public User(String userName, String fullName, String email) {
        this.UserName = userName;
        this.FullName = fullName;
        this.Email = email;
    }
}
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


| Bucket | Key Pattern | Example Key
|:-------|:------------|:-----------
| `Users` | `<user_name>` | `joeuser`
| `Msgs` | `<username>_<datetime>` | `joeuser_2014-03-06T02:05:13.223556Z`
| `Timelines` | `<username>_<type>_<date>` | `joeuser_Sent_2014-03-06Z`<br /> `marketing_group_Inbox_2014-03-06Z` |

For the `Users` bucket, we can be certain that we will want each
username to be unique, so let's use the `username` as the key.  With the
Java client, we can use the `@RiakKey` annotation to tell the client
that we want to use the `UserName` member as the key. It will
automatically use that value in the future, instead of having to pass the
key in as another parameter when storing a value.

For the `Msgs` bucket, let's use a combination of the username and the
posting datetime in an [ISO 8601
Long](http://en.wikipedia.org/wiki/ISO_8601) format. This combination
gives us the pattern `<username>_<datetime>`, which produces keys like
`joeuser_2014-03-05T23:20:28Z`.

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

Now that we've figured out our object model, let's write some
repositories to help create and work with these objects in Riak:

```java
package com.basho.msgy.Repositories;

import com.basho.msgy.Models.Msg;
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakRetryFailedException;
import com.basho.riak.client.bucket.Bucket;

public class MsgRepository {

    static final String BUCKET_NAME = "Msgs";
    protected RiakClient client;

    public MsgRepository(RiakClient client) {
        this.client = client;
    }

    public Msg get(String msgKey) throws Exception {
        Location key = new Location(new Namespace(BUCKET_NAME), msgKey);
        FetchValue fetch = new FetchValue.Builder(key).build();
        FetchValue.Response response = client.execute(fetch);
        return response.getValue(Msg.class);
    }

    public String save(Msg msg) throws Exception {
        StoreValue store = new StoreValue.Builder(msg).build();
        client.execute(store);
        return generateKey(msg);
    }

    private String generateKey(Msg msg) {
        return msg.Sender + "_" + msg.Created;
    }
}

// ----------------------------------------------------------------------------

package com.basho.msgy.Repositories;

import com.basho.msgy.Models.Msg;
import com.basho.msgy.Models.Timeline;
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakRetryFailedException;
import com.basho.riak.client.bucket.Bucket;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

public class TimelineRepository {

    static final String BUCKET_NAME = "Timelines";
    protected RiakClient client;
    protected MsgRepository msgRepo;

    public TimelineRepository(RiakClient client) {
        this.client = client;
        this.msgRepo = new MsgRepository(this.client);
    }

    public void postMsg(Msg msg) throws Exception {
        String msgKey = msgRepo.save(msg);

        // Post to recipient's Inbox timeline
        addToTimeline(msg, Timeline.TimelineType.Inbox, msgKey);

        // Post to sender's Sent timeline
        addToTimeline(msg, Timeline.TimelineType.Sent, msgKey);
    }


    private void addToTimeline(Msg msg, Timeline.TimelineType type, String msgKey) throws Exception {
        String timelineKey = generateKeyFromMsg(msg, type);

        Location loc = new Location(new Namespace(BUCKET_NAME), timelineKey);
        FetchValue fetch = new FetchValue.Builder(loc).build();
        Timeline timeline = client.execute(fetch).getValue(Timeline.class);

        if (timeline != null) {
            timeline = addToExistingTimeline(timeline,msgKey);
        } else {
            timeline = createNewTimeline(msg, type, msgKey);
        }

        StoreValue store = new StoreValue.Builder(timeline).build();
        client.execute(store);
    }

    public Timeline createNewTimeline(Msg msg, Timeline.TimelineType type, String msgKey) {
        String owner = getOwner(msg, type);

        Timeline newTimeline = new Timeline();
        newTimeline.Owner = owner;
        newTimeline.Type = type.toString();
        newTimeline.Msgs.add(msgKey);

        return newTimeline;
    }

    public Timeline addToExistingTimeline(Timeline timeline, String msgKey) {
        timeline.Msgs.add(msgKey);
        return timeline;
    }

    public Timeline getTimeline(String ownerUsername, Timeline.TimelineType type, Date date) throws RiakRetryFailedException {
        String timelineKey = generateKey(ownerUsername, type, date);
        Bucket bucket = client.fetchBucket(BUCKET_NAME).execute();
        return bucket.fetch(timelineKey, Timeline.class).execute();
    }

    private String generateKeyFromMsg(Msg msg, Timeline.TimelineType type) {
        String owner = getOwner(msg, type);
        String dateString = msg.Created.substring(0, 10);
        return generateKey(owner, type, dateString);
    }

    private String getOwner(Msg msg, Timeline.TimelineType type) {
        if(type == Timeline.TimelineType.Inbox)
            return msg.Recipient;
        else
            return msg.Sender;
    }

    private String generateKey(String ownerUsername, Timeline.TimelineType type, Date date) {
        String dateString = getIso8601DateStringFromDate(date);
        return generateKey(ownerUsername, type, dateString);
    }

    private String generateKey(String ownerUsername, Timeline.TimelineType type, String dateString) {
        return ownerUsername + "_" + type.toString() + "_" + dateString;
    }

    private String getIso8601DateStringFromDate(Date date) {
        TimeZone tz = TimeZone.getTimeZone("UTC");
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
        df.setTimeZone(tz);
        return df.format(date);
    }


}

// ----------------------------------------------------------------------------

package com.basho.msgy.Repositories;

import com.basho.msgy.Models.User;
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakRetryFailedException;
import com.basho.riak.client.bucket.Bucket;

public class UserRepository {
    static final String BUCKET_NAME = "Users";
    protected IRiakClient client;

    public UserRepository(IRiakClient client) {
        this.client = client;
    }

    public void save(User user) throws RiakRetryFailedException {
        Bucket bucket = client.fetchBucket(BUCKET_NAME).execute();
        bucket.store(user).execute();
    }

    public User get(String UserName) throws RiakRetryFailedException {
        Bucket bucket = client.fetchBucket(BUCKET_NAME).execute();
        return bucket.fetch(UserName, User.class).execute();
    }
}

```

Finally, let's test them:

```java
package com.basho.msgy;

import com.basho.msgy.Models.Msg;
import com.basho.msgy.Models.Timeline;
import com.basho.msgy.Models.User;
import com.basho.msgy.Repositories.MsgRepository;
import com.basho.msgy.Repositories.TimelineRepository;
import com.basho.msgy.Repositories.UserRepository;
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;

import java.util.Date;

public class MsgyMain {

    public static void main(String[] args) throws RiakException {
        // Setup our repositories
        IRiakClient client = RiakFactory.pbcClient("127.0.0.1", 10017);

        UserRepository userRepo = new UserRepository(client);
        MsgRepository msgRepo = new MsgRepository(client);
        TimelineRepository timelineRepo = new TimelineRepository(client);

        // Create and save users
        User marleen = new User("marleenmgr",
                "Marleen Manager",
                "marleen.manager@basho.com");

        User joe = new User("joeuser",
                "Joe User",
                "joe.user@basho.com");

        userRepo.save(marleen);
        userRepo.save(joe);

        // Create new Msg, post to timelines
        Msg msg = Msg.createNew(marleen.UserName,
                joe.UserName,
                "Welcome to the company!");

        timelineRepo.postMsg(msg);


        // Get Joe's inbox for today, get first message
        Timeline joesInboxToday = timelineRepo.getTimeline(joe.UserName,
                                                           Timeline.TimelineType.Inbox,
                                                           new Date());

        Msg joesFirstMsg = msgRepo.get(joesInboxToday.Msgs.get(0));

        System.out.println("From: " + joesFirstMsg.Sender);
        System.out.println("Msg : " + joesFirstMsg.Text);
        System.out.println("");

        client.shutdown();
    }
}
```

As you can see, the repository pattern helps us with a few things:

 - It helps us to see if an object exists before creating a new one
 - It keeps our buckets and key names consistent
 - It provides us with a consistent interface to work with.

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
* How to choose natural keys based on how we want to partition our data




