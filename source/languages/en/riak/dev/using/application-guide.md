---
title: Building Applications with Riak
project: riak
version: 1.0.0+
document: guide
audience: beginner
keywords: [developers, applications]
---

So you've decided to build an application using Riak as a data store. We think that this is a wise choice for a wide variety of use cases. But using Riak isn't always straightforward, especially if you're used to developing with RDBMSs like MySQL or Postgres or non-persistent key/value stores like Redis. 

In this guide, we'll walk you through a set of questions that you should ask about your use case before getting started. The answer to some of these questions could inform decisions about which Riak features you should use, what kind of replication and conflict resolution strategies you should employ, and perhaps even how parts of your application should be built.

## What Kind of Data is Being Stored?

This is an important initial question for two reasons:

1. Not all data is a good fit for Riak. If your data isn't a good fit, we would advise seeking out a different storage system.
2. The kind of data you're storing should guide your decision about *how* to store and access your data in Riak and which Riak features would be helpful (and which harmful).

#### Good Fits for Riak

Riak tends to be an excellent choice if you're dealing with any of the following:

* **Immutable data** --- While Riak provides several means of resolving conflicts between different replicas of objects, those processes can produce latency. Storing immutable data means avoiding those processes altogether.
* **Small objects** --- Riak was not built as a store for large objects, like video files or other large [BLOB](http://en.wikipedia.org/wiki/Binary_large_object)s. We built [RiakCS](http://basho.com/riak-cloud-storage/) for that. Riak is great, however, for JSON, [[log files|Log Data]], [[sensor data]], and filetypes that tend to run smaller than 1 MB, such as HTML files.
* **Independent objects** --- Objects that do not have interdependencies on other objects are a good fit for Riak's [[eventually consistent|Eventual Consistency]] nature.
* **Objects with "natural" keys** --- It is almost always advisable to build keys for objects out of timestamps, [[usernames|User Accounts]], or other ["natural" markers](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example) that distinguish that an object from other objects. Data that can be modeled this way fits nicely with Riak because Riak emphasizes fast object lookup.
* **Data compatible with [[Riak Data Types|Using Data Types]]** --- If you're working with mutable data, you can run CRUD operations on that data in traditional key/value fashion and manage conflict resolution yourself or allow Riak to do so. If your data can be modeled as a [[counter|Data Types#Counters]], [[set|Data Types#Sets]], or [[map|Data Types#Map]]

#### Not-so-good Fits for Riak

Riak may not such be a good choice if you use it with the following:

* **Objects stored that exceed 1-2MB in size** --- If you will be storing a lot of objects over that size, we would recommend checking out [Riak CS](http://docs.basho.com/riakcs/latest/), which was built as 
* **Objects with complex interdependencies** --- If your data cannot be easily denormalized or if it requires that objects can be easily assembled into and accessible as larger wholes---think columns or tables---then you might want to consider a relational database instead.
* **Highly mutable objects** --- While Riak handles most mutable data just fine, if your application involves changing objects very frequently, you may want to consider either a different data store or a 


