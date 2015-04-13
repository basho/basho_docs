---
title: Building Applications with Riak
project: riak
version: 2.0.0+
document: guide
audience: beginner
keywords: [developers, applications]
---

So you've decided to build an application using Riak as a data store. We
think that this is a wise choice for a broad variety of use cases. But
using Riak isn't always straightforward, especially if you're used to
developing with relational databases like like MySQL or PostgreSQL or
non-persistent key/value stores like Redis. So in this guide, we'll walk
you through a set of questions that should be asked about your use case
before getting started. The answer to those questions may inform
decisions about which Riak features you should use, what kind of
replication and conflict resolution strategies you should employ, and
perhaps even how parts of your application should be built.

## What Kind of Data Are You Storing?

This is an important initial question for two reasons:

1. Not all data is a good fit for Riak. If your data isn't a good fit,
we would advise that you seek out a storage system that better suits
your needs.
2. The kinds of data that you're storing should guide your decision both
about _how_ to store and access your data in Riak and about which Riak
features would be helpful (and which ones might even be harmful).

### Good Fits for Riak

Riak tends to be an excellent choice if you're dealing with any of the
following:

* **Immutable data** --- While Riak provides several means of
  [[resolving conflicts|Conflict Resolution]] between different replicas
  of objects, those processes can lead to slower performance in some
  cases. Storing immutable data means that you can avoid those processes
  altogether and get the most out of Riak.
* **Small objects** --- Riak was not built as a store for large objects
  like video files or other
  [BLOB](http://en.wikipedia.org/wiki/Binary_large_object)s. We built
  [Riak CS](http://basho.com/riak-cloud-storage/) for that. Riak is
  great, however, for JSON, [[log files|Use Cases#log-data]], [[sensor
  data|Use Cases#sensor-data]], HTML files, and other objects that tend
  to run smaller than 1 MB.
* **Independent objects** --- Objects that do not have interdependencies
  on other objects are a good fit for Riak's [[eventually
  consistent|Eventual Consistency]] nature.
* **Objects with "natural" keys** --- It is almost always advisable to
  build keys for objects out of timestamps, [[usernames|User Accounts]],
  or other [["natural" markers|Key/Value Modeling]] that distinguish
  that object from other objects. Data that can be modeled this way fits
  nicely with Riak because Riak emphasizes extremely fast object lookup.
* **Data compatible with [[Riak Data Types|Using Data Types]]** --- If
  you're working with mutable data, one option is to run basic CRUD
  operations on that data in a standard key/value fashion and either
  manage conflict resolution yourself or allow Riak to do so. But if
  your data can be modeled as a [[counter|Using Data Types#Counters]],
  [[set|Using Data Types#Sets]], or [[map|Using Data Types#Map]], you
  should seriously consider using [[Riak Data Types|Using Data Types]],
  which can speed application development and transfer a great deal of
  complexity away from the application and to Riak itself.

### Not-so-good Fits for Riak

Riak may not such be a good choice if you use it to store:

* **Objects that exceed 1-2MB in size** --- If you will be
  storing a lot of objects over that size, we would recommend checking
  out [Riak CS](http://docs.basho.com/riakcs/latest/) instead, as Riak
  CS was built to solve this problem. Storing large objects in Riak will
  typically lead to substandard performance.
* **Objects with complex interdependencies** --- If your data cannot be
  easily denormalized or if it requires that objects can be easily
  assembled into and accessible as larger wholes---think columns or
  tables---then you might want to consider a relational database
  instead.

### Conclusion

If it sounds like Riak is a good choice for some or all of your
application's data needs, move on to the next sections, where you can
find out more about which Riak features are recommendable for your use
case, how you should model your data, and what kinds of data modeling
and development strategies we recommend.

## Which Features Should You Consider?

Basic CRUD key/value operations are almost always the most performant
operations when using Riak. If your needs can be served using CRUD
operations, we recommend checking out our tutorial on [[key/value
modeling]] for some basic guidelines. But if basic CRUD key/value
operations don't quite suffice for your use case, Riak offers a variety
of features that may be just what you're looking for. In the sections
immediately below, you can find brief descriptions of those features as
well as relevant links to Basho documentation.

## Search

Riak Search provides you with [Apache
Solr](http://lucene.apache.org/solr/)-powered full-text indexing and
querying on top of the scalability, fault tolerance, and operational
simplicity of Riak. Our motto for Riak Search: **Write it like Riak.
Query it like Solr**. That is, you can store objects in Riak [[like
normal|The Basics]] and run full-text queries on those objects later on
using the Solr API.

* [[Using Search]] --- Getting started with Riak Search
* [[Search Details]] --- A detailed overview of the concepts and design
  consideration behind Riak Search
* [[Search Schema]] --- How to create custom schemas for extracting data
  from Riak Search

### When to Use Search

* **When you need a rich querying API** --- Riak Search gives you access
  to the entirety of [Solr](http://lucene.apache.org/solr/)'s extremely
  broad API, which enables you to query on the basis of wildcards,
  strings, booleans, geolocation, ranges, language-specific fulltext,
  and far more. You can even use Search in conjunction with [[Riak Data
  Types|Using Data Types]] \(documentation coming soon).

<div class="note">
<div class="title">Search is preferred for querying</div>
In general, you should consider Search to be the default choice for
nearly all querying needs that go beyond basic CRUD/KV operations. If
your use case demands some sort of querying mechanism and you're in
doubt about what to use, you should assume that Search is the right tool
for you.
</div>

### When Not to Use Search

* **When deep pagination is needed** --- At the moment, you should
    consider [[secondary indexes|Using Secondary Indexes]] instead of
    Search if your use case requires deep pagination. This will be
    changed, however, in a future release of Riak, at which point you
    should consider Search the default choice for _all_ querying needs.
* **In large clusters** --- In clusters larger than 8-10 nodes, you may
    experience slower performance when using Search. In clusters of that
    size, we would recommend using Search in a limited fashion, setting
    up a separate, dedicated cluster for Search data, or finding another
    solution.

## Riak Data Types

When performing basic K/V operations, Riak is agnostic toward the actual
data stored within objects. Beginning with Riak 2.0, however, you now
have access to operations-based objects based on academic research on
[CRDTs](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf). Riak
Data Types enable you to update and read [[counters|Using Data
Types#counters]], [[sets|Using Data Types#sets]], and [[maps|Using Data
Types#maps]] directly in Riak, as well as [[registers|Data Types#maps]]
and [[flags|Data Types#maps]] inside of Riak maps.

The beauty of Riak Data Types is that all convergence logic is handled
by Riak itself according to deterministic, Data Type-specific rules,
which means that your application doesn't need to reason about
[[siblings|Conflict Resolution#Siblings]]. In many cases, this can
unburden applications of the need to handle object convergence on their
own.

* [[Using Data Types]] --- A guide to setting up Riak to use Data Types,
  including a variety of code samples for all of the Basho's official
  [[client libraries]]
* [[Data Types]] --- A theoretical treatment of Riak Data Types, along
  with implementation details
* [[Data Modeling with Riak Data Types]] --- An object modeling example
  that relies on Riak Data Types

**Note**: Riak Data Types can be used in conjunction with Riak Search,
meaning that the data stored in counters, sets, and maps can be indexed
and searched just like any other data in Riak. Documentation on Data
Types and Search is coming soon.

### When to Use Riak Data Types

* **When your data fits** --- If the data that you're storing can be
  modeled as one of the five available types, Riak Data Types could be a
  very good option. Please note that in many cases there may not be a
  1:1 correspondence between the five available types and the data that
  you'd like to store, but there may be workarounds to close the gap.
  Most things that can be stored as JSON, for example, can be stored as
  maps (though with modifications).
* **When you don't need to reason about siblings** --- If your use case
  doesn't require that your application have access to siblings and
  allows for sibling convergence logic to take place at the Riak level
  rather than at the application level, then Riak Data Types are well
  worth exploring.

### When Not to Use Riak Data Types

* **When you need to provide your own convergence logic** --- If your
  application needs to have access to all sibling values, then Riak Data
  Types are not a good choice because they by definition do not produce
  siblings.
* **When your data just doesn't fit** --- While the five existing Data
  Types allow for a great deal of flexibility and a wide range of use
  cases, they don't cover all use cases. If you have data that requires
  a modeling solution that can't be covered, you should stick to
  standard K/V operations.
* **When object size is of significant concern** --- Riak Data Types
  behave much like other Riak objects, but they tend to carry more
  metadata than normal Riak objects, especially maps. In most cases the
  metadata payload will be a small percentage of the object's total
  size, but if you want to keep objects as lean as possible, it may be
  better to stick to normal K/V operations.

## MapReduce

Riak's MapReduce feature enables you to perform batch processing jobs in
a way that leverages Riak's distributed nature. When a MapReduce job is
sent to Riak, Riak automatically distributes the processing work to
where the target data lives, which can reduce network bandwidth. Riak
comes equipped with a set of default MapReduce jobs that you can employ,
or you can write and run your own MapReduce jobs in
[Erlang](http://www.erlang.org/).

* [[Using MapReduce]] --- A general guide to using MapReduce
* [[Advanced MapReduce]] --- A more in-depth guide to MapReduce,
  including code samples and implementation details

### When to Use MapReduce

* **Batch processing only** --- You should use MapReduce only when truly
  truly necessary. MapReduce jobs are very computationally expensive and
  can degrade performance in production clusters. You should restrict
  MapReduce usage to infrequent batch processing operations, preferably
  carried out at times when your cluster is experiencing load that is
  well below average.

### When Not to Use MapReduce

* **When another Riak feature will do** --- Before even considering
  using MapReduce, you should thoroughly investigate [[Riak Search|Using
  Search]] or [[secondary indexes|Using Secondary Indexes]] as possible
  solutions to your needs.

In general, you should not think of MapReduce as, for example, Hadoop
within Riak. While it can be useful for certain types of
non-primary-key-based queries, it is neither a "Big Data" processing
tool nor an indexing mechanism nor a replacement for [[Riak Search|Using
Search]]. If you do need a tool like Hadoop or Apache Spark, you should
consider using Riak in conjunction with a more suitable data processing
tool.

## Secondary Indexes (2i)

Using basic key/value operations in Riak sometimes leads to the
following problem: how do I know which keys I should look for? Secondary
indexes (2i) provide a solution to this problem, enabling you to tag
objects with either binary or integer metadata and then query Riak for
all of the keys that share specific tags. 2i is especially useful if
you're storing binary data that is opaque to features like [[Riak
Search|Using Search]].

* [[Using Secondary Indexes]] --- A general guide to using 2i, along
  with code samples and information on 2i features like pagination,
  streaming, and sorting
* [[Advanced Secondary Indexes]] --- Implementation details behind 2i

### When to Use Secondary Indexes

* **When you require deep pagination** --- At the moment, 2i's
    deep pagination capabilities are more performant than those offered
    by Search if you require pagination of more than 3-5 pages. This
    will change, however, in the future, at which point we will
    recommend using Search instead.

### When Not to Use Secondary Indexes

* **For most querying purposes** --- If your use case does not
    involve deep pagination, we recommend Search over 2i for _all_
    querying purposes.
* **If you're using Bitcask** --- 2i is available only in the
    [[LevelDB]] backend. If you'd like to use [[Bitcask]] or the
    [[Memory]] backend, you will not be able to use 2i.

## Mixed Approach

One thing to always bear in mind is that Riak enables you to mix and
match a wide variety of approaches in a single cluster. You can use
basic CRUD operations for some of your data, index some of your data to
be queried by Riak Search, use Riak Data Types for another subset, etc.
You are always free to use a wide array of Riak features---or you can
use none at all and stick to key/value operations.

## How Should You Model Your Data?

It's difficult to offer universally applicable data modeling guidelines
because data models differ so markedly from use case to use case. What
works when storing [[user data|Use Cases#user-data]], for example, might
be a poor fit when working with [[sensor data|Use Cases#sensor-data]].
Nonetheless, there's a variety of material in our documentation that
might be helpful when thinking about data modeling:

* [[Object Modeling in Riak|Taste of Riak: Object Modeling]]
    - [[Java|Taste of Riak: Object Modeling with Java]]
    - [[Ruby|Taste of Riak: Object Modeling with Ruby]]
    - [[Python|Taste of Riak: Object Modeling with Python]]
    - [[C#|Taste of Riak: Object Modeling with CSharp]]
    - [[Erlang|Taste of Riak: Object Modeling with Erlang]]
* [[Key/Value Modeling]]

### Data Types

One feature to always bear in mind when using Riak is [[Riak Data
Types|Using Data Types]]. If some or all of your data can be modeled in
accordance with one of the available Data Types---flags (similar to
Booleans), registers (good for storing small binaries or text snippets),
[[counters|Using Data Types#counters]], [[sets|Using Data Types#sets]],
or [[maps|Using Data Types#maps]]---you might be able to streamline
application development by using them as an alternative to key/value
operations. In some cases, it might even be worthwhile to transform your
data modeling strategy in accordance with To see if this feature might
be a good fit for your application, we recommend checking out the
following documentation:

* [[Data Types]]
* [[Using Data Types]]
* [[Data Modeling with Riak Data Types]]

## What are Your Consistency Requirements?

Riak has traditionally been thought of as an [[eventually
consistent|Eventual Consistency]], AP system, i.e. as a system that
favors availability and partition tolerance over data consistency. In
Riak versions 2.0 and later, the option of applying strong consistency
guarantees is available to developers that want to use Riak as a strict
CP system. One of the advantages of Riak's approach to strong
consistency is that you don't need to store all of your data in a
strongly consistent fashion if you use this feature. Instead, you can
mix and match a CP approach with an AP approach in a single cluster in
any way you wish.

If you need some or all of your data to be subject to strong consistency
requirements, we recommend checking out the following documentation:

* [[Strong Consistency]]
* [[Using Strong Consistency]]
* [[Managing Strong Consistency]]

## Are Your Objects Mutable?

Although Riak always performs best when storing and retrieving immutable
data, Riak also handles mutable objects very ably using a variety of
eventual consistency principles. Storing mutable data in Riak, however,
can get tricky because it requires you to choose and implement a
conflict resolution strategy for when object conflicts arise, which is a
normal occurrence in Riak. For more implementation details, we recommend
checking out the following docs:

* [[Conflict Resolution]]
* [[Object Updates]]
* [[Replication Properties]]

## Getting Started

If you have a good sense of how you will be using Riak for your
application (or if you just want to experiment), the following guides
will help you get up and running:

* [[Five-Minute Install]] --- Install Riak and start up a five-node Riak
  cluster
* [[Client Libraries]] --- A listing of official and non-official client
  libraries for building applications with Riak
* [[Getting Started with Client
  Libraries|Five-Minute Install#Setting-Up-Your-Riak-Client]] --- How to
  get up and going with one of Basho's official client libraries (Java,
  Ruby, Python, and Erlang)
* [[The Basics]] --- A guide to basic key/value operations in Riak
* [[Riak Glossary]] --- A listing of frequently used terms in Riak's
  documentation
