---
title: Use Cases
project: riak
version: 1.2.1+
document: cookbook
index: true
toc: false
audience: intermediate
keywords: [use-cases]
moved: {
  '1.4.0-': '/cookbooks/use-cases'
}
---

Riak is a flexible data storage technology capable of addressing
a wide variety of problems in a scalable way. In this guide, we'll list
a number of use cases and data models that are a good fit for Riak. All
of these use cases are already being used in production for projects
large and small. We'll also suggest possibilities for implementation and
provide links to videos and documentation for further exploration.

How you structure your application to run on Riak should take into
account the unique needs of your use case, including access patterns
such as read/write distribution, latency differences between various
operations, use of Riak features including [[Data Types]],
[[MapReduce|Using MapReduce]], [[Search|Using Search]],
[[secondary indexes (2i)|Using Secondary Indexes]] and more. This guide
is intended to be illustrative only.

## High Read/Write, Simple Applications

The following are examples of Riak use cases that require high
read/write performance without necessarily utilizing complex data
structures:

* [[Session Storage|Use Cases#Session-Storage]]
* [[Serving Advertisements|Use Cases#Serving-Advertisements]]
* [[Log Data|Use Cases#Log-Data]]
* [[Sensor Data|Use Cases#Sensor-Data]]

## Content Management, Social Applications

The following application types require more subtle relationships
between objects, e.g. one-to-many and many-to-many relationships.

* [[User Accounts|Use Cases#User-Accounts]]
* [[User Settings and Preferences|Use
  Cases#User-Settings-and-Preferences]]
* [[User Events and Timelines|Use Cases#User-Events-and-Timelines]]
* [[Articles, Blog Posts, and Other
  Content|Use Cases#Articles-Blog-Posts-and-Other-Content]]

## Session Storage

Riak was originally created to serve as a highly scalable session store.
This is an ideal use case for Riak, which is always most performant and
predictable when used as a key/value store. Since user and session IDs
are usually stored in cookies or otherwise known at lookup time, Riak is
able to serve these requests with predictably low latency. Riak's
content-type agnosticism also imposes no restrictions on the value, so
session data can be encoded in many ways and can evolve without
administrative changes to schemas.

### Complex Session Storage Case

Riak has features that allow for more complex session storage use cases.
The [[Bitcask]] storage backend, for example, supports automatic expiry
of keys, which frees application developers from implementing manual
session expiry. Riak's [[MapReduce|Using MapReduce]] system can also be
used to perform batch processing analysis on large bodies of session
data, for example to compute the average number of active users. If
sessions must be retrieved using multiple keys (e.g. a UUID or email
address), [[using secondary indexes]] can provide an easy solution.

### Session Storage Community Examples

<table class="links">
    <tr>
        <td><a href="https://player.vimeo.com/video/42744689" target="_blank" title="Scaling Riak at Kiip">
           <img src="http://b.vimeocdn.com/ts/296/624/296624215_960.jpg"/>
         </a></td>
        <td><a href="https://player.vimeo.com/video/42744689" target="_blank" title="Riak at OpenX">Scaling Riak at Kiip</a>
        <br>
    In this talk, recorded at the May 2012 San Francisco Riak Meetup, Armon Dadgar and Mitchell Hashimoto of Kiip give an overview of how and why they are using Riak in production, and the road they took to get there. One of the first subsystems they switched over to Riak was Sessions. You can also read the blog post and catch the slides <a href="http://basho.com/blog/technical/2012/05/25/Scaling-Riak-At-Kiip/" class="riak" target="_blank">here.</a>
        </td>
    </tr>
</table>

## Serving Advertisements

Riak is often a good choice for serving advertising content to many
different web and mobile users simultaneously with low latency. Content
of this sort, e.g. images or text, can be stored in Riak using unique
generated either by the application or by Riak. Keys can be created
based on, for example, a campaign or company ID for easy retrieval.

### Serving Advertisements Complex Case

In the advertising industry, being able to serve ads quickly to many
users and platforms is often the most important factor in selecting and
tuning a database. Riak's tunable [[Replication Properties]] can be set
to favor fast read performance. By setting R to 1, only one of N
replicas will need to be returned to complete a read operation, yielding
lower read latency than an R value equal to the number of replicas
(i.e. R=N). This is ideal for advertising traffic, which primarily
involves serving reads.

### Serving Advertisements Community Examples

<table class="links">
  <tr>
    <td><a href="http://player.vimeo.com/video/49775483" target="_blank" title="Riak at OpenX"><img src="http://b.vimeocdn.com/ts/343/417/343417336_960.jpg"/></a>
    </td>
    <td><a href="http://player.vimeo.com/video/49775483" target="_blank" title="Riak at OpenX">Riak at OpenX</a>
    <br>
    Los Angeles-based OpenX will serves trillions of ads a year. In this
    talk, Anthony Molinaro, Engineer at OpenX, goes in depth on their
    architecture, how they've built their system, and why/how they're
    switching to Riak for data storage after using databases like
    CouchDB and Cassandra in production.
    </td>
  </tr>
</table>

## Log Data

A common use case for Riak is storing large amounts of log data, either
for analysis [[using MapReduce]] or as a storage system used in
conjunction with a secondary analytics cluster used to perform more
advanced analytics tasks. To store log data, you can use a bucket called
`logs` (just to give an example) and use a unique value, such as a date,
for the key. Log files would then be the values associated with each
unique key.

For storing log data from different systems, you could use unique
buckets for each system (e.g. `system1_log_data`, `system2_log_data`,
etc.) and write associated logs to the corresponding buckets. To
analyze that data, you could use Riak's MapReduce system for aggregation
tasks, such as summing the counts of records for a date or Riak Search
for a more robust, text-based queries.

### Log Data Complex Case

For storing a large amount of log data that is frequently written to
Riak, some users might consider doing primary storage of logs in a
Riak cluster and then replicating data to a secondary cluster to run
heavy analytics jobs, either over another Riak cluster or another
solution such as Hadoop. Because the access patterns of reading and
writing data to Riak is very different from the access pattern of
something like a MapReduce job, which iterates over many keys,
separating the write workload from the analytics workload will let you
maintain higher performance and yield more predictable latency.

### Log Data Community Examples

<table class="links">
  <tr>
    <td><a href="http://www.simonbuckle.com/2011/08/27/analyzing-apache-logs-with-riak/" target="_blank" title="Riak at OpenX"><img src="/images/simon-analyzing-logs.png"/></a>
    </td>
    <td>Simon Buckle on <a href="http://www.simonbuckle.com/2011/08/27/analyzing-apache-logs-with-riak/" target="_blank">analyzing Apache logs with Riak.</a>
    </td>
  </tr>
</table>

## Sensor Data

Riak's scalable design makes it useful for data sets, like sensor data,
that scale rapidly and are subject to heavy read/write loads. Many
sensors collect and send data at a given interval. One way to model
this in Riak is to use a bucket for each sensor device and use the time
interval as a unique key (i.e. a date or combination of date and time),
and then store update data as the value.

That data could then be queried on the basis of the interval.
Alternatively, a timestamp could be attached to each object as a
[[secondary index|Using Secondary Indexes]], which would allow you to
perform queries on specific time interval ranges or to perform
[[MapReduce|Using MapReduce]] queries against the indexes.

### Sensor Data Complex Case

If you are dealing with thousands or millions of sensors yet with very
small data sets, storing all of a single device's updates as unique keys
may be cumbersome when it comes to reading that device's data.
Retrieving it all would mean calling a number of keys.

Instead, you could store all of a device's updates in a document with a
unique key to identify the device. Stored as a JSON document, you could
read and parse all of those updates on the client side. Riak, however,
doesn't allow you to append data to a document without reading the
object and writing it back to the key. This strategy would mean more
simplicity and performance on the read side as a tradeoff for slightly
more work at write time and on the client side.

It's also important to keep an eye out for the total size of documents
as they grow, as we tend to recommend that Riak objects stay smaller
than 1-2 MB and preferably below 100 KB. Otherwise, performance problems
in the cluster are likely.

## User Accounts

User accounts tend to rely on fairly straightforward data models. One
way of storing user account data in Riak would be store each user's data
as a JSON object in a bucket called `users` (or whatever you wish). Keys
for user data objects could be constructed using application-specific
considerations. If your application involves user logins, for example,
the simplest and most read-efficient strategy would be to use the login
username as the object key. The username could be extracted upon login,
and a read request could be performed on the corresponding key.

There are, however, several drawbacks to this approach. What happens if
a user wants to change their username later on? The most common solution
would be to use a UUID-type key for the user and store the user's
username as a [[secondary index|Using Secondary Indexes]] for efficient
lookup.

### User Accounts Complex Case

For simple retrieval of a specific account, a user ID (plus perhaps a
secondary index on a username or email) is enough. If you foresee the
need to make queries on additional user attributes (e.g. creation time,
user type, or region), plan ahead and either set up additional secondary
indexes or consider using [[Riak Search|Using Search]] to index the JSON
contents of the user account.

### User Accounts Community Examples

<table class="links">
  <tr>
    <td><a href="https://player.vimeo.com/video/47535803" target="_blank" title="Riak at Braintree"><img class="vid_img"src="http://b.vimeocdn.com/ts/329/711/329711886_640.jpg"/></a>
    </td>
    <td><a href="https://player.vimeo.com/video/47535803" target="_blank" title="Riak at Braintree">Riak at Braintree</a>
    <br>
    Ben Mills, a developer at Braintree, discusses how their backend team came to find and begin to integrate Riak into their production environment. They also cover their model and repository framework for Ruby, Curator. Check out more details and slides on the <a href="http://basho.com/blog/technical/2012/08/14/riak-at-braintree/" target="_blank">Riak blog.</a>
    </td>
  </tr>
</table>

## User Settings and Preferences

For user account-related data that is simple and frequently read but
rarely changed (such as a privacy setting or theme preference), consider
storing it in the user object itself. Another common pattern is to
create a companion user settings-type of object, with keys based on
user ID for easy one-read retrieval.

### User Settings and Preferences Complex Case

If you find your application frequently writing to the user account or
have dynamically growing user-related data such as bookmarks,
subscriptions, or multiple notifications, then a more advanced data
model may be called for.

## User Events and Timelines

Sometimes you may want to do more complex or specific kinds of modeling
user data. A common example would be storing data for assembling a
social network timeline. To create a user timeline, you could use a
`timeline` bucket in Riak and form keys on the basis of a unique user
ID. You would store timeline information as the value, e.g. a list of
status update IDs which could then be used to retrieve the full
information from another bucket, or perhaps containing the full status
update. If you want to store additional data, such as a timestamp,
category or list of properties, you can turn the list into an array of
hashes containing this additional information.

Note than in Riak you cannot append information to an object, so adding
events in the timeline would necessarily involve reading the full object,
modifying it, and writing back the new value.

### User Events and Timelines Community Examples

<table class="links">
  <tr>
    <td>
      <a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak at Yammer">
      <img src="http://b.vimeocdn.com/ts/139/033/139033664_640.jpg"/>
</a></td>
        <td><a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak at Yammer">Riak at Yammer</a>
        <br>
    This video was recorded at the March 2012 San Francisco Riak Meetup and is worth every minute of your time. Coda Hale and Ryan Kennedy of Yammer give an excellent and in depth look into how they built “Streamie”, user notifications, why Riak was the right choice, and the lessons learned in the process. Read more and get the slides in the Riak blog <a href="http://basho.com/blog/technical/2011/03/28/Riak-and-Scala-at-Yammer/" target="_blank">here.</a>
        </td>
    </tr>

    <tr>
        <td><a href="http://player.vimeo.com/video/44498491" target="_blank" title="Riak at Voxer">
           <img src="http://b.vimeocdn.com/ts/309/154/309154350_960.jpg"/>
         </a></td>
        <td><a href="http://player.vimeo.com/video/44498491" target="_blank" title="Riak at Voxer">Riak at Voxer</a>
        <br>
    The team at Voxer has long relied on Riak as their primary data store for various production services. They have put Riak through its paces and have served as one of our more exciting customers and use cases: Riak was in place when they shot to the top of the App Store at the end of 2011. We also love them because they open-sourced their Node.js client. Read more and get the slides in the Riak blog <a href="http://basho.com/blog/technical/2012/06/27/Riak-at-Voxer/" target="_blank">here.</a>
        </td>
    </tr>
</table>

## Articles, Blog Posts, and Other Content

The simplest way to model blog posts, articles, or similar content is
to use a bucket in Riak with some unique attribute for logical division
of content, such as `blogs` or `articles`. Keys could be constructed out
of unique identifiers for posts, perhaps the title of each article, a
combination of the title and data/time, an integer that can be used as
part of a URL string, etc.

In Riak, you can store content of any kind, from HTML files to plain
text to JSON or XML or another document type entirely. Keep in mind that
data in Riak is opaque, with the exception of [[Riak Data Types|Data Types]],
and so Riak won't "know" about the object unless it is indexed
[[using Riak Search|Using Search]] or [[using secondary indexes]].

### Articles et al Complex Case

Setting up a data model for content becomes more complex based on the
querying and search requirements of your application. For example, you
may have different kinds of content that you want to generate in a view,
e.g. not just a post but also comments, user profile information, etc.

For many Riak developers, it will make sense to divide content into
different buckets, e.g. a bucket for comments that would be stored in
the Riak cluster along with the posts bucket. Comments for a given post
could be stored as a document with the same key as the content post,
though with a different bucket/key combination. Another possibility
would be to store each comment with its own ID. Loading the full view
with comments would require your application to call from the posts
and comments buckets to assemble the view.

Other possible cases may involve performing operations on content beyond
key/value pairs. [[Riak Search|Using Search]] is recommended for use cases
involving full-text search. For lighter-weight querying,
[[using secondary indexes]] \(2i) enables you to add metadata to objects to
either query for exact matches or to perform range queries. 2i also
enables you to tag posts with dates, timestamps, topic areas, or other
pieces of information useful for later retrieval.

### Articles et al Community Examples

<table class="links">
  <tr>
    <td><a href="http://blog.clipboard.com/2012/03/18/0-Milking-Performance-From-Riak-Search" class="vid_img" target="_blank"><img src="/images/milking-perf-from-riak.png" title="Milking Performance"></a>
    </td>
    <td>Clipboard on <a href="http://blog.clipboard.com/2012/03/18/0-Milking-Performance-From-Riak-Search" target="_blank">storing and searching data in Riak.</a>
  </tr>
  <tr>
    <td><a href="http://media.basho.com/pdf/Linkfluence-Case-Study-v2-1.pdf" class="vid_img" link target="_blank"><img src="/images/linkfluence-case-study.png" title="Milking Performance"></a>
    </td>
    <td>Linkfluence case study on using Riak to <a href="http://media.basho.com/pdf/Linkfluence-Case-Study-v2-1.pdf" target="_blank">store social web content</a>.
  </tr>
  <tr>
    <td><a href="http://basho.com/assets/Basho-Case-Study-ideeli.pdf" class="vid_img" link target="_blank"><img src="/images/ideeli-case-study.png" title="Milking Performance"></a>
    </td>
    <td>ideeli case study on <a href="http://basho.com/assets/Basho-Case-Study-ideeli.pdf" target="_blank">serving web pages with Riak</a>.
  </tr>
</table>
