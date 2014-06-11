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

## What Kind of Data am I Storing?

This is an important initial question for two reasons:

1. Not all data is a good fit for Riak. If your data isn't a good fit, we would advise seeking out storage system that better suits your needs.
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

* **Objects stored that exceed 1-2MB in size** --- If you will be storing a lot of objects over that size, we would recommend checking out [Riak CS](http://docs.basho.com/riakcs/latest/), which was built to solve this problem.
* **Objects with complex interdependencies** --- If your data cannot be easily denormalized or if it requires that objects can be easily assembled into and accessible as larger wholes---think columns or tables---then you might want to consider a relational database instead.

#### Conclusion

If it sounds like Riak is a good choice for some or all of your application's data needs, move on to the next sections, where you can find out more about which Riak features are recommendable for your use case, how you should model your data, and what kinds of development strategies we recommend.

## What Features Should I Consider?

Basic CRUD operations are almost always the most performant operations when using Riak. If your needs can be served using CRUD operations, we recommend checking out our tutorial on [[key/value modeling]] for some basic guidelines.

If you think that you'll need something more than just CRUD operations,

While basic CRUD operations are almost always the most performant way to use Riak, Riak nonetheless offers a variety of features that can be helpful when basic key/value operations are not enough. Here, we'll list those features and explain when they're recommended.

#### Secondary Indexes (2i)

[[Using Secondary Indexes]]

#### Search

[[Search]]

#### Riak Data Types

[[Data Types]]
[[Using Data Types]]

#### MapReduce

[[Using MapReduce]]

#### Mixed Approach

One thing to always bear in mind is that Riak enables you to mix and match a wide variety of approaches in a single cluster. You can use basic CRUD operations for some of your data, attach secondary indexes to a handful of objects, run occasional MapReduce operations over a subset of your buckets, etc. You are always free to use a wide array of features or none at all.

## How Should I Model My Data?

It's difficult to offer universally applicable data modeling guidelines because data models differ to markedly from use case to use case. What works when storing [[user data]], for example, might be a poor fit when working with [[sensor data]]. Nonetheless, there's a variety of material in our documentation that might be helpful when thinking about data modeling.

#### Help with Modeling

* [[Taste of Riak: Object Modeling]]
    - [[Java|Taste of Riak: Object Modeling with Java]]
    - [[Ruby|Taste of Riak: Object Modeling with Ruby]]
    - [[Python|Taste of Riak: Object Modeling with Python]]
    - [[Erlang|Taste of Riak: Object Modeling with Erlang]]
* [[Data Modeling with Riak Data Types]]
* [[Key/Value Modeling]]

#### Data Types

One possibility to always bear in mind is using [[Riak Data Types|Using Data Types]] if some or all of your data can be modeled in accordance with the Data Types currently available. The table below lists the five available Riak Data Types:

Data Type | Description
:---------|:-----------
Flags |
Registers |
Counters |
Sets |
Maps |

## What are My Consistency Requirements?

In the past, Riak has typically been known as an [[eventually consistent|Eventual Consistency]], AP system, i.e. a system that favors availability and partitional tolerance over data consistency.

#### I Don't Need Much Consistency

#### I Need Strong Consistency

#### I Need Something Inbetween

## My Objects are Mutable

As stated above, Riak always 

## How Do I Get Started?

If you have a good sense of how you will be using Riak for your application (or if you just want to experiment), the set of steps below will help you get up and running.

#### Setting Up a Development Environment

#### Choosing a Riak Client Library

#### Getting Help

#### Examples Galore
