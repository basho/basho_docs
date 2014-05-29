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

Riak is a deeply flexible data storage technology capable of addressing
a wide variety of problems in a scalable way. In this guide, we'll list
a number of potential use cases and data models for Riak, some of which
are already being used in production for projects large and small. We'll
also suggest possibilities for implementation and provide links to
videos and documentation for further exploration.

How you structure your application to run on Riak should take into
account the unique needs of your use case, including access patterns
such as read/write distribution, latency differences between various
operations, use of Riak features including [[Data Types]], [[MapReduce]],
[[Search|Using Search]], and [[secondary indexes (2i)|Using Secondary Indexes]]
and more. This guide is intended to be illustrative only.

## High Read/Write, Simple Applications

The following application types require high read/write performance
without necessarily utilizing complex data structures:

* [[Session Storage|Use Cases#session-storage]]
* [[Serving Advertisements|Use Cases#serving-advertisements]]
* [[Log Data|Use Cases#log-data]]
* [[Sensor Data|Use Cases#sensor-data]]

## Content Management, Social Applications

The following application types require more subtle relationships
between objects, e.g. one-to-many and many-to-many relationships.

* [[User Accounts|Use Cases#user-accounts]]
* [[User Settings and Preferences|Use Cases#user-settings-and-preferences]]
* [[User Events and Timelines|Use Cases#user-events-and-timelines]]
* [[Articles, Blog Posts, and Other Content|Use Cases#articles-blog-posts-and-other-content]]

## Session Storage

#### Simple Case

Riak was originally created to serve as a highly scalable session store.
This is an ideal use case for Riak, which is always most performant and
predictable when used as a key/value store. Since user and session IDs
are usually stored in cookies or otherwise known at lookup time, Riak is
able to serve these requests with predictably low latency. Riak's
content-type agnosticism also imposes no restrictions on the value, so
session data can be encoded in many ways and can evolve without
administrative changes to schemas.

#### Complex Case

Riak has other features that enable more complex session storage use
cases. The [[Bitcask]] storage backend supports automatic expiry of
keys, which frees application developers from implementing manual
session expiry. Riak's [[MapReduce]] system can also be used to perform
batch processing analysis on large bodies of session data, for example
to compute the average number of active users. If sessions must be
retrieved using multiple keys (e.g. a UUID or email address), 
[[using secondary indexes]] can provide an easy solution.

#### Community Examples

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

#### Simple Case

A common use case for Riak is using it as a data store for serving ad
content to many different web and mobile users with low latency. In
Riak, advertising content---e.g. images or text---can be stored with
unique keys or with Riak-generated keys. Users will often make keys
based on a campaign ID for easy retrieval.

#### Complex Case

In the advertising industry, being able to serve ads quickly to many
users and platforms is often the most important factor in selecting and
tuning a database. Riak's tunable [[Replication Properties]] can be set
to favor fast read performance. By setting R to 1, only one of N
replicas will need to be returned to complete a read operation, yielding
lower read latency than an R value equal to the number of replicas
(i.e. R=N). This is ideal for advertising traffic which is primarily
serving reads.

#### Community Examples

<table class="links">
  <tr>
    <td><a href="http://player.vimeo.com/video/49775483" target="_blank" title="Riak at OpenX"><img src="http://b.vimeocdn.com/ts/343/417/343417336_960.jpg"/></a>
    </td>
    <td><a href="http://player.vimeo.com/video/49775483" target="_blank" title="Riak at OpenX">Riak at OpenX</a>
    <br>
    Los Angeles-based OpenX will serve around four trillion ads this
    year. In this talk, Anthony Molinaro, Engineer at OpenX, goes
    in depth on their architecture, how they've built their system, and
    why/how they are switching to Riak for data storage after using
    databases like CouchDB and Cassandra in production.
    </td>
  </tr>
</table>

## Log Data

#### Simple Case

A common use case for Riak is storing large amounts of log data for
analysis with [[MapReduce]] or as the primary storage for log data with
a secondary analytics cluster used to perform more advanced analytics
tasks. For this, you can create a bucket called `logs` (or whatever
you'd like) and use a unique value, such as a date, for the key. Log
files would be the values associated with the unique keys. For storing
log data from different systems, you could create a unique bucket for
each system and write associated logs to that bucket. In terms of
analyzing log data, you could then use Riak's MapReduce for aggregation
tasks such as summing the counts of records for a date, or Riak Search
for more robust, text-based queries.

#### Complex Case

For storing a large amount of log data that is frequently writing data
to Riak, some users might consider doing primary storage of logs in a
Riak cluster and then replicating data to a secondary cluster to run
heavy analytics jobs, either over another Riak cluster or another
solution such as Hadoop. Because the access patterns of reading and
writing data to Riak is very different from the access pattern of
something like a MapReduce job which is iterating over many keys,
separating the write workload from the analytics workload will let you
maintain higher performance and yield more predictable latency.

#### Community Examples

<table class="links">
  <tr>
    <td><a href="http://www.simonbuckle.com/2011/08/27/analyzing-apache-logs-with-riak/" target="_blank" title="Riak at OpenX"><img src="/images/simon-analyzing-logs.png"/></a>
    </td>
    <td>Simon Buckle on <a href="http://www.simonbuckle.com/2011/08/27/analyzing-apache-logs-with-riak/" target="_blank">analyzing Apache logs with Riak.</a>
    </td>
  </tr>
</table>

## Sensor Data

#### Simple Case

 Riak's scalable design makes it useful for data sets, like sensor data,
 that scale rapidly and are subject to heavy read/write loads. Many
 sensors collect and send data at a given interval. One way to model
 this in Riak is to create a bucket for each sensor device, and use the
 interval as a unique key (i.e., a date or combination of date/time),
 then store update data as the value. You could then query on the
 interval; or alternatively store a timestamp as a secondary index
 (piece of queryable metadata attached to the key/value pair) that would
 allow you to perform queries on specific ranges or perform MapReduce
 queries against the indexes.

#### Complex Case

If you are dealing with thousands or millions of sensors, yet very small
data sets, storing all of a single device's updates as unique keys may
be overly cumbersome when it comes to reading that device's data.
Retrieving it all would mean calling a number of keys. Instead, you
could store all of a device's updates in a document with a unique key to
identify the device. Stored as a JSON document, you can read and parse
all of those updates on the client side. Riak doesn't allow you to
append data to a document without reading the object and writing it back
to the key, however. This strategy would mean more simplicity and
performance on the read side in tradeoff for slightly more work at write
time and on the client side. You also have to keep an eye out for the
total size of the document as it grows - we tend to recommend a sensible
object size of about 1-2MB in order to avoid performance problems in the
cluster.

## User Accounts

#### Simple Case

User accounts are pretty straight-forward -- the usual practice is to
store JSON objects in a 'users' bucket. As far as what to use for a key
value, usual app-specific considerations apply. For example, if your
application involves user logins, the simplest and most read-efficient
way is to use the login username as the object key. Get the username off
the login, perform a GET on the user account object and go. There are
several drawbacks, however - what if they'll want to change their
username or email, later? The most common solution is - use a UUID-type
key for the user, and store their username or email as a secondary index
for efficient lookup.

#### Complex Case

For simple retrieval of a specific account, a user id (plus perhaps a
secondary index on a username/email) is enough. If you foresee the need
to make queries on additional user attributes (creation time, user type,
region), plan ahead and either set up additional Secondary Indexes, or
consider using Riak Search to index the JSON contents of the user
account.

#### Community Examples

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

#### Simple Case

For user account-related data that is simple, frequently read but rarely
changed (such as a privacy setting or theme preference), consider
storing it in the user object itself. Another common pattern is to
create a companion User Settings type of object, also keyed off of the
user ID for easy one-read retrieval.

#### Complex Case

 If you find your application frequently writing to the user account, or
 have dynamically growing user related data such as bookmarks,
 subscriptions or multiple notifications, then a more advanced data
 model is called for (see the section on social events/subscriptions)

## User Events and Timelines

#### Simple Case

Sometimes you may want to do more complex or specific kinds of modeling
user data. A common example would be storing data for assembling a
social network timeline. To create a user timeline, you could make
"timeline" a bucket in Riak, and make keys a unique user ID. You would
store timeline information as a value - a list of status update IDs
which could then be used to retrieve the full information from another
bucket, or perhaps containing the full status update. If you want to
store additional data - such as a timestamp, category or list properties
- turn the list into an array of hashes containing this additional
- information. Note that in Riak, you cannot append information to an
- object, so to add events in the timeline, you would have to read the
- full object, add the new values to the hash, and write it back.

#### Community Examples

<table class="links">
    <tr>
        <td><a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak at Yammer">
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

#### Simple Case

The simplest way to model blog posts, articles or other content is by
creating a bucket in Riak with some unique attribute for logical
division of content, perhaps  `blogs` or `articles` or something
similar. Keys could be unique identifiers for posts, perhaps the article
title, a combination of the title and time/date, or perhaps an integer
that can be used as part of a URL string. You can store content anyway
you want, from HTML blobs to plain text to JSON or XML or another
document type entirely. Keep in mind that data in Riak is opaque, with
the exception of [[Riak Data Types|Data Types]], so Riak won't "know"
about the object unless it is indexed [[using Riak Search]] or
[[using secondary indexes]].


#### Complex Case

Setting up a data model for content gets more complex based on the
querying and search requirements of your application or its various
aspects. For example, you may have different kinds of content you want
to generate in a view - not just a post, but comments, users and profile
information, etc. For many Riak developers, it will make sense to divide
out content into different buckets - a bucket for comments, for example,
that would be stored in the Riak cluster along with the posts bucket.
Comments for a given post could be stored as a document with the same
key as the content post - only the bucket/key combination must be
unique. Or you could store each comment with its own ID. Loading the
full view with comments would mean your application would need to call
from the posts and comments bucket to assemble the view. Another common
case that is slightly more complex is when you want to perform search
and query operations on content beyond just retrieving key/value pairs.
Riak Search, our full-text search engine that implements a SOLR-like API
and query model, is a great use case for text content, and users like
Clipboard have some great work available on how to optimize search
performance. For lighter-weight querying, secondary indexes allow you to
add additional metadata to objects for querying on exact match or range
values. Using secondary indexes, you could tag posts with dates,
timestamps, topic areas or others of interest. It's important to make
sure that your dataset will be a use case with 2i, as it can be
performance-prohibitive in clusters with over 512 partitions.

#### Community Examples

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
