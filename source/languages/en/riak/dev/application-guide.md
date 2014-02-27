---
title: Building Applications with Riak
project: riak
version: 1.0.0+
document: guide
audience: beginner
keywords: [developers, applications]
---

So you've decided to build an application using Riak as a data store for some or all of your data. We think that this is a wise choice for a wide variety of use cases. But using Riak in an optimal way requires 

## What Kind of Data is Being Stored?

This is an important initial question to ask for two reasons:

1. Not all data is a good fit for Riak, and if this is true of your data we would advise seeking out a different storage system
2. The kind of data you're storing should guide your decision about *how* to store and access your data in Riak

Riak tends to be an excellent choice for data of the following kinds:

* Immutable data
* Small objects
* Independent objects, i.e. objects that don't have complex interdependencies with other objects
* Objects with "natural" keys, e.g. timestamps or [[usernames|User Accounts]]
* Mutable data that can be modeled as a [[counter|Data Types#Counters]], [[set|Data Types#Sets]], or [[map|Data Types#Map]] {{#2.0.0+}}

Riak is probably not recommended if you need to store:

* Objects stored that exceed 1-2MB in size (in which case we would recommend checking out [Riak CS](http://docs.basho.com/riakcs/latest/) instead)
* Objects with many complex interdependencies that can not be easily denormalized (in which case a traditional relational database)

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
