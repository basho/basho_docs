---
title: "Riak Redis Add-on Features"
description: ""
project: "riak_kv"
project_version: "2.0.8"
menu:
  riak_kv-2.0.8:
    name: "Redis Add-on Features"
    identifier: "add-ons_redis_features"
    weight: 504
    parent: "add-ons_redis"
toc: true
commercial_offering: true
---

[ee]: http://basho.com/contact/
[GET-sequence]: {{<baseurl>}}images/redis/GET_seq.msc.png
[SET-sequence]: {{<baseurl>}}images/redis/SET_seq.msc.png
[DEL-sequence]: {{<baseurl>}}images/redis/DEL_seq.msc.png
[Object-lifetime]: {{<baseurl>}}images/redis/Object_lifetime.msc.png
[redis docs]: http://redis.io/commands
[twemproxy docs]: https://github.com/twitter/twemproxy/blob/master/notes/redis.md

## Overview

The cache proxy service in Riak Redis Add-on (RRA) provides pre-sharding and connection aggregation as a service, which reduces latency and increases addressable cache memory space with lower-cost hardware.

On this page, you will find detailed descriptions of cache proxy service components, including what each component does and how you implement it. The following components are available:
 
* [Pre-sharding](#pre-sharding)
* [Connection Aggregation](#connection-aggregation)
* [Command Pipelining](#command-pipelining)
* [Read-through Cache](#read-through-cache)
* [Write-around Cache](#write-around-cache)
* [Commands](#commands)
* [Object Lifetime](#object-lifetime)

## Pre-sharding

Pre-sharding with consistent hashing dispatches object reads and writes based
on a configurable hash function, spreading load across multiple cache servers.
The cache proxy service uses pre-sharding to extend the total addressable cache memory space based on the number of Redis servers. Request keys are hashed, then
requests are routed to the Redis server that handles that portion of the key
range.

Redis with no persistence is used as the frontend cache proxy service, and
Redis as a data server holds all data in memory. The addressable memory of
cache proxy is limited. By employing pre-sharding, the total addressable cache
memory space is extended by the number of Redis servers.

## Connection Aggregation

Redis client connections are a limited resource. Using the cache proxy service, connections may be spread across multiple Riak Redis Add-on (RRA) servers. This reduces the total required connections to the Redis server for the same key.

Redis clients in various languages support specifying multiple servers, as well
as implementing multiple methods of spreading load across those servers (i.e.
round-robin load balancing or consistent hashing).  Since the cache proxy service is providing consistent hashing, any Redis client method of supporting multiple
servers will suffice.

## Command Pipelining

The cache proxy service increases performance by pipelining requests to Redis. While pipelining can be performed at the client, the cache proxy service is ideal due to connection aggregation. Pipelining reduces network roundtrips to Redis and
lowers CPU usage on Redis.

## Read-Through Cache

Implementing caching strategies in the cache proxy service reduces the cost of implementing cache strategies in client code in multiple applications and languages. The cache proxy service supports the read-through cache strategy, the most prevalent caching strategy used in distributed computing.

The read-through cache strategy of the GET command is represented by the
following sequence diagram:

![GET command sequence diagram]({{<baseurl>}}images/redis/GET_seq.msc.png)


The `CACHE_TTL` configuration option establishes how long the cache takes to
become consistent with the backend server during a write (DELETE or PUT) to the
backend server.

A short `CACHE_TTL`, for example "15s", reduces a significant amount of read
pressure from Riak, increasing performance of the overall solution.

## Write-Around Cache

The read-through cache strategy requires a TTL to keep cache as coherent as possible given that writes to Riak KV can and will be issued without the cache proxy service being informed of the write. The effect is that the cache proxy service is eventually consistent with the underlying Riak KV data store, with the time to consistency equal to the TTL.

The cache proxy service write-around cache strategy was introduced to provide a means to keep cache coherent with zero time to consistency with the underlying Riak KV data store for all writes that the cache proxy is informed of. For the Redis String (Value in KV) datatype, SET and DEL commands result in writes to the underlying Riak KV data store followed by a PEXPIRE to invalidate cache.

Of the three write cache strategies, the write-around cache strategy is the least
prone to race condition, but least optimal for the read which immediately follows
the write. In the overwhelming majority of distributed application data access
patterns, the added certainty of cache coherency afforded by write-around over
write-through is well worth the single cache miss. By definition, a key that is
cached is expected to be accessed frequently, hence the single cache miss is
expected to be followed by several accurate cache hits.

The write-around cache strategy of the SET command is represented by the
following sequence diagram:

![SET command sequence diagram]({{<baseurl>}}images/redis/SET_seq.msc.png)

The write-around cache strategy of the DEL command is represented by the
following sequence diagram:

![DEL command sequence diagram]({{<baseurl>}}images/redis/DEL_seq.msc.png)

## Commands

For command details, refer to the Redis [documentation][redis docs].

The cache proxy service supports the following augmented Redis commands fully:

* GET - get the value of a key from Redis or Riak KV utilizing the read-through
  caching strategy with a TTL set at service configuration time.

* SET - set the value of a key to Riak KV and invalidate cache, issue a PEXPIRE
  to Redis.

* DEL - delete the value of a key to Riak KV and invalidate cache, issue a
  PEXPIRE to Redis.

The cache proxy service also supports the set of Redis commands supported by Twemproxy, but only to the point of pre-sharding and command pipelining, issued only to Redis. Refer to the Twemproxy [documentation][twemproxy docs].

>**Important:** While the cache proxy service does support issuing DEL commands, PEXPIRE, with a small TTL, is suggested instead when the semantic intent is to remove an item from cache.  With write-around, the DEL command will issue a delete to the Riak backend.

## Object Lifetime

With the combination of read-through and write-around cache strategies, the
full object lifetime for a key-value is represented by the following
sequence diagram:

![Object lifetime sequence diagram]({{<baseurl>}}images/redis/Object_lifetime.msc.png)
