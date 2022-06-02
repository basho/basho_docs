---
title: "Riak TS Best Practices"
description: "Riak TS Best Practices"
menu:
  riak_ts-1.3.1:
    name: "Best Practices"
    identifier: "riakts_best_practices"
    weight: 501
    parent: "about"
project: "riak_ts"
project_version: "1.3.1"
toc: true
aliases:
    - /riakts/1.3.1/learn-about/bestpractices/
---


[glossary bucket]: {{< baseurl >}}riak/kv/2.1.4/learn/glossary/#bucket
[table arch]: ../tablearchitecture/


There are many ways to interact with and use Riak TS. This page will give recommendations for structuring your storage region (DDL) and choosing your quanta.


## TS Table Schema

One of the first things you will encounter in setting up Riak TS is defining a TS table's schema.  You use Data Definition Language (DDL), specifically the `CREATE TABLE` statement, to create a new table. When you create a TS Table, the data structure provided in the `CREATE TABLE` statement is compiled and passed around the Riak TS cluster.

The design of your TS table schema is really important because:

1. The values you choose for the [partition key][table arch] will impact the speed of your query returns.
2. Once activated, the TS table schema cannot be changed.

To help speed the performance of Riak TS and avoid destroying and recreating tables to change their definitions, think about the most common queries you will be executing and what tools you will be using to process the data. The answers to those will determine what values you use in the [primary key][table arch].

For instance, let's say we have a time series database storing information about air quality for a given location. The data coming in from sensors in given locations includes: geohash for location, timestamp, levels of CO2, smog particles in the air, temperature, and humidity.

The most common queries we'll have will be to determine, for a given location and timeframe, the spikes in O2 for a given temperature. Since our use case requires fast reads, we'll choose to correlate data with Spark. In that case, we'll choose to use geohash location and timestamp for the first and second fields in the partition key, since queries are faster on partition key. And we'll put CO2, smog, temperature, and humidity data as [secondary fields][table arch].


## Quantum

The quantum, the time-based part of the primary key, plays an important role in how data is distributed around the Riak ring, which determines the performance of writes. To provide better data locality, writes are sequential based on the primary key. The quantum was introduced to provide grouping of a time span for these sequential writes and allow for better data distribution around the ring.

When choosing the quantum, you'll want to consider how fast your writes will be coming in and the speed of the disks on your nodes. These answers will determine how fast a set of writes for a quantum happen. Think specifically about:

* Rate of data received/written given your other [partition key][table arch] fields.
* How many writes a given quantum needs to hold and how large the object size.

Choosing a quantum that is too big will create hotspots. But, while choosing a smaller quantum will even out the distribution of objects, a too-small quantum will negatively impact query performance.

If we assume the following partition key:

* first partition key - geohash
* second partition key - sensor_type
* quantum = 15 M

All the data for a geohash location of sensor type A will be written sequentially to a vnode for a span of 15 minutes. As the first and second partition keys change, the data is distributed around the ring.

During performance testing, we achieved 100% writes at 130K writes per second with a quantum of 15 minutes. A 15 minute quantum ensured our writes to the vnode would be sequential, which allowed us to write ~1 billion objects per quantum.

If you have fast, large SSD disks, you may want to increase your quantum to 30 minutes or an hour.