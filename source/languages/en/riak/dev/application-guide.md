---
title: Building Applications with Riak
project: riak
version: 1.0.0+
document: guide
audience: beginner
keywords: [developers, applications]
---

So you've decided to build an application using Riak as a data store. We think that this is a wise choice for a wide variety of use cases. But using Riak isn't always straightforward, especially if you're used to developing with RDBMSs like MySQL or Postgres or non-persistent key/value stores like Redis. 

In this guide, we'll walk you through a set of questions that you should ask about your use case before getting started. The answer to some of these questions might say a lot about which Riak features you should use, what kind of replication and conflict resolution strategies you should employ, and even how you should build parts of your application.

## What Kind of Data is Being Stored?

This is an important initial question for two reasons:

1. Not all data is a good fit for Riak. If your data isn't a good fit, we would advise seeking out a different storage system.
2. The kind of data you're storing should guide your decision about *how* to store and access your data in Riak and which Riak features would be helpful (and which harmful).

Riak tends to be an excellent choice for data of the following kinds:

* **Immutable data** --- While Riak provides a number of means of resolving conflicts between different replicas of objects, those processes can produce latency. Storing immutable data means avoiding those processes altogether.
* **Small objects** --- Riak was not built as a store for large objects, like video files or other large [BLOB](http://en.wikipedia.org/wiki/Binary_large_object)s. We built [RiakCS](http://basho.com/riak-cloud-storage/) for that. Riak is great, however, for JSON, [[log files|Log Data]], [[sensor data]], and filetypes that tend to run smaller than 1 MB, like HTML files.
* **Independent objects** --- Objects that have complex interdependencies 
* Objects with "natural" keys, e.g. timestamps or [[usernames|User Accounts]]
* Mutable data that can be modeled as a [[counter|Data Types#Counters]], [[set|Data Types#Sets]], or [[map|Data Types#Map]]

Riak is probably not recommended if you need to store:

* Objects stored that exceed 1-2MB in size. If you will be storing a lot of objects over that size, we would recommend checking out [Riak CS](http://docs.basho.com/riakcs/latest/), which was built as 
* Objects with many complex interdependencies that can not be easily denormalized (in which case a traditional relational database)

#### Conclusion

If Riak fits your needs, move on to the next section

## The Flexibility of Riak

Riak is an extremely flexible system for three reasons:

1. It allows you to store any type of data, from plain text to JSON to binary large objects (BLOBs) to [[Riak Data Types|Using Data Types]] inspired by research on [[CRDTs|Data Types]].
2. It enables you to access your data in myriad ways, from simple [[key/value|The Basics]] retrieval to [[secondary indexes|Using Secondary Indexes]] to rich [[search capabilities|Using Search]] to [[MapReduce]] and pre- and post-[[commit hooks|Using Commit Hooks]].
3. While Riak is typically thought of as an AP system---favoring data availability over data consistency---there are a variety of ways of fine-tuning the trade-off between availability and consistency by adjusting [[N, R, and W values|Replication Properties]]. {{#2.0.0+}}You can even use Riak as a [[strongly consistent|Strong Consistency]] system for some or all of your data.{{/2.0.0+}}

TODO: link to denormalization / data modeling document

The following table

## How Should I Access My Data?

Riak provides a number of ways of retrieving data, each 

* Key/Value - this is the preferred method for accessing a value stored in Riak. It will have the lowest latency of any operation.
* Secondary indexes (2i)
* Riak [[Data Types]]
* Riak Search
* Strong consistency
* Using R vs PR
* What actually happens when I grab an object? (`r` value, `notfound_ok`, read repair, fallback vnodes)

## How Should I Store My Data?

* "happy path" - the ideal case.
* What can go wrong, and how do I recover? Timeouts, quorum not met
* Using W vs PW
* What actually happens when I store an object? (w value, fallback vnodes)

## Riak gotchas

* ZOMG Siblings
* Reading your own writes
* How do AAE and replication figure into all this?

## Riak Anti-Patterns

Riak is a powerful and flexible data storage system, there are ways of using Riak that play to its fundamental strengths and ways that do not. There are ways of accessing data in Riak that are faster and more scalable than others, there are features that are powerful yet should be used sparingly, and there are things that you should simply never, ever try.

In this guide, we'll walk you through some anti-patterns to bear in mind when developing with Riak.

## Scalability vs. Dynamic Querying

No matter how useful some of Riak's features may be, there is simply no match for basic GET/PUT/DELETE key/value operations in terms of scalability, flexibility, and raw performance. Riak works best when it is given a key and asked to write, retrieve, or delete an object. Period.

While these operations might seem somewhat primitive, especially if you're used to `SELECT * FROM table`-style and other SQL-flavored features, they are extremely flexible, allowing for a great deal of freedom on the application side: the freedom to store any kind of object behind any key, to construct keys any way you like, and to create your own data models---models that may not mesh well with row/column/table relational models.

Because Riak works best as a key/value store, you should always be wary of storing massive amounts of data in Riak and using features [[secondary indexes|Using Secondary Indexes]], [[Riak Search|Using Search]], [[MapReduce operations|Using MapReduce]], and other query features without careful consideration. These features are quite powerful but should be used only when truly necessary. Overuse will most likely lead to subpar performance and diminished scalability.

#### Conclusion

If your problem can be solved with GETs, PUTs, and DELETEs, absolutely choose that path. If not, consider other features.

## Mutable vs. Immutable Data

Because Riak objects live on multiple machines at once, using Riak almost always involves choosing a [[conflict resolution|Vector Clocks#Siblings]] strategy that dictates which objects are deemed most "correct" and up to date. Some resolution strategies, like setting `last_write_wins` to `true`, delegate conflict resolution to Riak itself. 

No matter what conflict resolution strategy you choose, 

#### Conclusion

Storing and fetch immutable data enables you to 
If your use case requires that data be mutable, Riak can still be a very good choice, as it provides a variety of options.

## Data Normalization vs. Open Namespaces

Riak was not built to allow for things like table joins, foreign key constraints, and other operations. Instead, Riak is mostly agnostic toward the data stored in it, with the exception of [[Riak Data Types|Data Types]]; it lacks the data awareness that would make it a good candidate for true normalization.

Instead, Riak consists of three namespaces: [[bucket types|Using Bucket Types]], [[buckets]], and keys. You can group keys within buckets however you wish, and buckets have no intrinsic meaning beyond the fact that they group keys together and share the same [[bucket properties|Buckets]]. Any effort to group objects together on the basis of the actual _content_ of those objects is bound to fail. Riak simply cannot be made queryable along the lines of `SELECT * FROM table1 LEFT OUTER JOIN table2...` because that kind of operation is antithetical to how Riak works.

#### Conclusion

Instead of seeking data normalization, you should think about other ways of structuring your application's access to Riak. One way to do this is to assign a meaningful key to each object, for example a username or a timestamp, that will enable an application to "know in advance," so to speak, how a Riak object can be accessed.

Although they are a less performant option that basic key/value operations, [[secondary indexes|Using Secondary Indexes]] are another option worth exploring. This feature enables you to attach metadata to each Riak object, beyond the key/value pair itself, that can assist applications in finding objects.

## Object Size: Keep it Small

#### Conclusion

Riak was not built to handle large objects. No matter which configuration options you choose, objects larger than 1-2 MB will entail a performance hit. If your use case demands that objects exceed this size, then we strongly recommend [Riak CS](http://basho.com/riak-cloud-storage/), which is built on top of Riak but with the goal of acting as a storage system for larger objects.

