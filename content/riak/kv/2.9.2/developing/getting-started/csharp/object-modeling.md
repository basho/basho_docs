---
title_supertext: "Getting Started:"
title: "Object Modeling with C Sharp"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Object Modeling"
    identifier: "getting_started_csharp_object"
    weight: 102
    parent: "getting_started_csharp"
toc: true
aliases:
  - /riak/2.9.2/dev/taste-of-riak/object-modeling-csharp
  - /riak/kv/2.9.2/dev/taste-of-riak/object-modeling-csharp
---

To get started, refer to [this source code][1] for the models that we'll
be using.

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
| `Msgs` | `<username>_<datetime>` | `joeuser_2014-03-06T02:05:13`
| `Timelines` | `<username>_<type>_<date>` | `joeuser_Sent_2014-03-06`<br /> `marketing_group_Inbox_2014-03-06` |

For the `Users` bucket, we can be certain that we will want each
username to be unique, so let's use the `username` as the key.

For the `Msgs` bucket, let's use a combination of the username and the
posting UTC datetime in an [ISO 8601][iso_8601]
format. This combination gives us the pattern `<username>_<datetime>`,
which produces keys like `joeuser_2014-03-05T23:20:28`.

Now for `Timelines`, we need to differentiate between `Inbox` and `Sent`
timelines, so we can simply add that type into the key name. We will
also want to partition each collection object into some time period,
that way the object doesn't grow too large (see note below).

For `Timelines`, let's use the pattern `<username>_<type>_<date>` for
users, and `<groupname>_Inbox_<date>` for groups, which will look like
`joeuser_Sent_2014-03-06` or `marketing_group_Inbox_2014-03-05`,
respectively.

{{% note title="Note" %}}
Riak performs best with objects under 1-2MB. Objects larger than that can hurt
performance, especially when many siblings are being created. We will cover
siblings, sibling resolution, and sibling explosions in the next chapter.
{{% /note %}}

#### Keeping our story straight with repositories

Now that we've figured out our object model, please refer to
[this source code][2] for the repositories that we'll be using.

[This console application][3] exercises the code that we've written.

The repository pattern and `TimelineManager` help with a few things:

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

[1]: https://github.com/basho/taste-of-riak/tree/master/csharp/Ch03-Msgy-Schema/Models
[2]: https://github.com/basho/taste-of-riak/tree/master/csharp/Ch03-Msgy-Schema/Repositories
[3]: https://github.com/basho/taste-of-riak/blob/master/csharp/Ch03-Msgy-Schema/Program.cs
[iso_8601]: http://en.wikipedia.org/wiki/ISO_8601

