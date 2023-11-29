---
title: "Riak TS Best Practices"
description: "Riak TS Best Practices"
menu:
  riak_ts-1.2.0:
    name: "Best Practices"
    identifier: "riakts_best_practices"
    weight: 501
    parent: "about"
project: "riak_ts"
project_version: "1.2.0"
lastmod: 2016-02-16T00:00:00-00:00
sitemap:
  priority: 0.1
toc: true
aliases:
    - /riakts/1.2.0/learn-about/bestpractices/
---

[table arch]: ../tablearchitecture/

There are many ways to interact with and use Riak TS. This page will give recommendations for structuring your storage region (DDL) and

### DDL Definition

One of the first things you will encounter setting up Riak TS is defining storage regions/bucket types, also known as Data Definition Language (DDL). Creating a table will specify the DDL and compile it, which will create a bucket. The bucket information is passed around the ring.

There are two reasons that how you set up your DDL is really important. First, the values you choose for the [family and series fields][table arch] will impact the speed of your query returns. Second, once activated, the bucket definition cannot be changed, so the DDL cannot be changed either.

To help speed the performance of Riak TS and avoid recreating DDL definitions, think about the most common queries you will be executing and what tools you will be using to process the data. The answers to those will determine what value you use for the family and series fields in the primary key.

For instance, let's say we have a time series database storing information about air quality for a given location. The data coming in from sensors in given locations includes: geohash for location, timestamp, levels of CO2, smog particles in the air, temperature, and humidity.

The most common queries we'll have will be to determine, for a given location and timeframe, the spikes in O2 for a given temperature. Since our use case requires fast reads, we'll choose to correlate data with Spark. In that case, we'll choose to use geohas location and timestamp for the family and series fields in the primary key, since queries are faster on primary key. And we'll put CO2, smog, temperature, and humidity data as [column fields][table arch].

## Quantum

The quantum, the time-based part of the primary key, plays an important role in how data is distributed around the Riak ring, which determines the performance of writes. To provide better data locality, writes are sequential based on the primary key. The quantum was introduced to provide grouping of a time span for these sequential writes and allow for better data distribution around the ring.

When choosing the quantum, you'll want to consider how fast your writes will be coming in and the speed of the disks on your nodes. These answers will determine how fast a set of writes for a quantum happen. Think specifically about:

* Rate of data received/written given your other [primary key][table arch] fields (family, series).
* How many writes a given quantum needs to hold and how large the object size.

Choosing a quantum that is too big will create hotspots. But, while choosing a quantum that is smaller will even out the distribution of objects, a too-small quantum will negatively impact query performance.

If we assume the following primary key:

* family = geohash
* series = sensor type
* quantum = 15 M

All the data for a geohash location of sensor type A will be written sequentially to a vnode for a span of 15 minutes. As the family and series change, the data is distributed around the ring.

During performance testing, we achieved 100% writes at 130K writes per second with a quantum of 15 minutes. A 15 minute quantum ensured our writes to the vnode would be sequential, which allowed us to write ~1 billion objects per quantum.

If you have fast, large SSD disks, you may want to increase your quantum to 30 minutes or an hour.
