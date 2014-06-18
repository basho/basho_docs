---
title: Building Applications with Riak
project: riak
version: 1.0.0+
document: guide
audience: beginner
keywords: [developers, applications]
---

So you've decided to build an application using Riak as a data store. We think that this is a wise choice for a broad variety of use cases. But using Riak isn't always straightforward, especially if you're used to developing with RDBMSs like MySQL or Postgres or non-persistent key/value stores like Redis. 

In this guide, we'll walk you through a set of questions that should be asked about your use case before getting started. The answer to those questions may inform decisions about which Riak features you should use, what kind of replication and conflict resolution strategies you should employ, and perhaps even how parts of your application should be built.

## What Kind of Data Are You Storing?

This is an important initial question for two reasons:

1. Not all data is a good fit for Riak. If your data isn't a good fit, we would advise seeking out a storage system that better suits your needs.
2. The kind of data you're storing should guide your decision about _how_ to store and access your data in Riak and which Riak features would be helpful (and which ones might even be harmful).

#### Good Fits for Riak

Riak tends to be an excellent choice if you're dealing with any of the following:

* **Immutable data** --- While Riak provides several means of resolving conflicts between different replicas of objects, those processes can lead to slower performance in some cases. Storing immutable data means taht you can avoid those processes altogether and get the most out of Riak.
* **Small objects** --- Riak was not built as a store for large objects, like video files or other large [BLOB](http://en.wikipedia.org/wiki/Binary_large_object)s. We built [Riak CS](http://basho.com/riak-cloud-storage/) for that. Riak is great, however, for JSON, [[log files|Use Cases#log-data]], [[sensor data|Use Cases#sensor-data]], HTML files, and other objects that tend to run smaller than 1 MB.
* **Independent objects** --- Objects that do not have interdependencies on other objects are a good fit for Riak's [[eventually consistent|Eventual Consistency]] nature.
* **Objects with "natural" keys** --- It is almost always advisable to build keys for objects out of timestamps, [[usernames|User Accounts]], or other ["natural" markers](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example) that distinguish that object from other objects. Data that can be modeled this way fits nicely with Riak because Riak emphasizes extremely fast object lookup.
* **Data compatible with [[Riak Data Types|Using Data Types]]** --- If you're working with mutable data, one option is to run basic CRUD operations on that data in a standard key/value fashion and either manage conflict resolution yourself or allow Riak to do so. But if your data can be modeled as a [[counter|Data Types#Counters]], [[set|Data Types#Sets]], or [[map|Data Types#Map]], you should seriously consider using [[Riak Data Types|Using Data Types]], which can speed application development and transfer a great deal of complexity away from the application and to Riak itself.

#### Not-so-good Fits for Riak

Riak may not such be a good choice if you use it with the following:

* **Objects stored that exceed 1-2MB in size** --- If you will be storing a lot of objects over that size, we would recommend checking out [Riak CS](http://docs.basho.com/riakcs/latest/) instead, as Riak CS was built to solve this problem. Storing large objects in Riak will typically lead to substandard performance.
* **Objects with complex interdependencies** --- If your data cannot be easily denormalized or if it requires that objects can be easily assembled into and accessible as larger wholes---think columns or tables---then you might want to consider a relational database instead.

#### Conclusion

If it sounds like Riak is a good choice for some or all of your application's data needs, move on to the next sections, where you can find out more about which Riak features are recommendable for your use case, how you should model your data, and what kinds of data modeling and development strategies we recommend.

## Which Features Should You Consider?

Basic CRUD key/value operations are almost always the most performant operations when using Riak. If your needs can be served using CRUD operations, we recommend checking out our tutorial on [[key/value modeling]] for some basic guidelines. But if basic CRUD key/value operations don't quite suffice for your use case, Riak offers a variety of features that may be just what you're looking for. In the sections immediately below, you can find brief descriptions of those features as well as relevant links to Basho documentation.

#### Search

Riak Search provides you with [Apache Solr](http://lucene.apache.org/solr/)-powered full-text indexing and querying on top of the scalability, fault tolerance, and operational simplicity of Riak. Our motto for Riak Search: "Write it like Riak. Query it like Solr." That is, you can store objects in Riak [[like normal|The Basics]] and run full-text queries on those objects later on using the Solr API.

* [[Using Search]] --- Getting started with Riak Search
* [[Search Details]] --- A detailed overview of the concepts and design consideration behind Riak Search
* [[Search Schema]] --- How to create custom schemas for extracting data from Riak Search

#### Riak Data Types

Basic key/value operations in Riak are agnostic toward the data stored within objects. Beginning with Riak 2.0, however, you now have access to operations-based objects based on academic research on [CRDTs](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf). Riak Data Types enable you to update and read [[counters|Using Data Types#counters]], [[sets|Using Data Types#sets]], and [[maps|Using Data Types#maps]] directly in Riak, as well as [[registers|Data Types#maps]] and [[flags|Data Types#maps]] inside of Riak maps.

The beauty of Riak Data Types is that all convergence logic is handled by Riak itself according to deterministic, Data Type-specific rules. In many cases, this can unburden applications of the need to handle object convergence on their own.

* [[Using Data Types]] --- A guide to setting up Riak to use Data Types, including a variety of code samples for all of the Basho's official [[client libraries]]
* [[Data Types]] --- A theoretical treatment of Riak Data Types, along with implementation details
* [[Data Modeling with Riak Data Types]] --- An object modeling example that relies on Riak Data Types

#### MapReduce

Riak's MapReduce feature enables you to perform powerful batch processing jobs in a way that fully leverages Riak's distributed nature. When a MapReduce job is sent to Riak, Riak automatically distributes the processing work to where the target data lives, which can reduce network bandwidth. Riak comes equipped with a set of default MapReduce jobs that you can employ, or you can write and run your own MapReduce jobs in [Erlang](http://www.erlang.org/).

* [[Using MapReduce]] --- A general guide to using MapReduce
* [[Advanced MapReduce]] --- A more in-depth guide to MapReduce, including code samples and implementation details

#### Secondary Indexes (2i)

Using basic key/value operations in Riak sometimes leads to the following problem: how do I know which keys I should look for? Secondary indexes (2i) provide a solution to this problem, enabling you to tag objects with either binary or integer metadata and then query Riak for all of the keys that share specific tags. 2i is especially useful if you're storing binary data that is opaque to features like [[Riak Search|Using Search]]. 

* [[Using Secondary Indexes]] --- A general guide to using 2i, along with code samples and information on 2i features like pagination, streaming, and sorting
* [[Advanced Secondary Indexes]] --- Implementation details behind 2i

#### Mixed Approach

One thing to always bear in mind is that Riak enables you to mix and match a wide variety of approaches in a single cluster. You can use basic CRUD operations for some of your data, attach secondary indexes to a handful of objects, run occasional MapReduce operations over a subset of your buckets, etc. You are always free to use a wide array of features---or you can use none at all and stick to key/value operations.

## How Should You Model Your Data?

It's difficult to offer universally applicable data modeling guidelines because data models differ so markedly from use case to use case. What works when storing [[user data|Use Cases#user-data]], for example, might be a poor fit when working with [[sensor data|Use Cases#sensor-data]]. Nonetheless, there's a variety of material in our documentation that might be helpful when thinking about data modeling:

* [[Object Modeling in Riak|Taste of Riak: Object Modeling]]
    - [[Java|Taste of Riak: Object Modeling with Java]]
    - [[Ruby|Taste of Riak: Object Modeling with Ruby]]
    - [[Python|Taste of Riak: Object Modeling with Python]]
    - [[Erlang|Taste of Riak: Object Modeling with Erlang]]
* [[Key/Value Modeling]]

#### Data Types

One feature to always bear in mind when using Riak is [[Riak Data Types|Using Data Types]]. If some or all of your data can be modeled in accordance with Riak Data Types, you might be able to streamline application development by using them as an alternative to key/value operations. In some cases, it might even be worthwhile to transform your data modeling strategy in accordance with To see if this feature might be a good fit for your application, we recommend checking out the following documentation:

* [[Data Types]]
* [[Using Data Types]]
* [[Data Modeling with Riak Data Types]]

## What are Your Consistency Requirements?

Riak has traditionally been thought of as an [[eventually consistent|Eventual Consistency]], AP system, i.e. a system that favors availability and partitional tolerance over data consistency. In Riak versions 2.0 and later, the option of applying strong consistency guarantees is available to developers that want to use Riak as a strict CP system. One of the advantages of Riak's approach to strong consistency is that you don't need to store all of your data in a strongly consistent fashion if you use this feature. Instead, you can mix and match a CP approach with an AP approach in a single cluster in any way you wish.

If you need some or all of your data to be subject to strong consistency requirements, we recommend checking out the following documentation:

* [[Strong Consistency]]
* [[Using Strong Consistency]]

## Are Your Objects Mutable?

Although Riak always performs best when storing and retrieving immutable data, Riak also handles mutable objects very ably using a variety of eventual consistency principles. Storing mutable data in Riak, however, can get tricky because it requires you to choose and implement a conflict resolution strategy for when object conflicts arise, which is a normal occurrence in Riak. For more implementation details, we recommend checking out the following docs:

* [[Conflict Resolution]]
* [[Replication Properties]]

## Getting Started

If you have a good sense of how you will be using Riak for your application (or if you just want to experiment), the following guides will help you get up and running:

* [[Five-Minute Install]] --- Install Riak and start up a five-node Riak cluster
* [[Client Libraries]] --- A listing of official and non-official client libraries for building applications with Riak
* [[Riak Glossary]] --- A listing of frequently used terms in Riak's documentation
