---
title: "Cache Proxy Features"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Cache Proxy"
    identifier: "learn_cache_proxy"
    weight: 101
    parent: "learn"
toc: true
aliases:
  - /dataplatform/1.0.0/learn-about-dataplatform/cache-proxy-features/
  - /dataplatform/latest/learn/cache-proxy/
---

[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html
[readthrough-strategy]: {{<baseurl>}}images/readthrough-strategy.png
[writethrough-sequence]: {{<baseurl>}}images/writethrough-sequence.png

>Cache proxy is available to [Enterprise users only][ee].

## Overview

Basho Data Platform (BDP) cache proxy provides pre-sharding and connection aggregation as a service, which reduces latency and increases addressable cache memory space with lower cost hardware.

On this page, you will find detailed descriptions of cache proxy's components, including what each component does and how you implement it. Cache proxy has the following components:
 
* Pre-sharding
* Connection Aggregation
* Command Pipelining
* Read-through Cache

You will also find a list of commands you can use with cache proxy.

## Pre-sharding

Pre-sharding with consistent hashing dispatches object reads and writes based on a configurable hash function, spreading load across multiple cache servers. Cache proxy uses pre-sharding to extend the total addressable cache memory space based on the number of Redis servers. Request keys are hashed, then requests are routed to the Redis server that handles that portion of the key range.

Redis with no persistence is used as the frontend cache proxy service, and Redis as a data server holds all data in memory. The addressable memory of cache proxy is limited. By employing pre-sharding, the total addressable cache memory space is extended by the number of Redis servers.

## Connection Aggregation

Redis client connections are a limited resource. Using cache proxy, connections may be spread across multiple cache proxy servers. This reduces the total required connections to the Redis server for the same key.

Redis clients in various languages support specifying multiple servers, as well as implementing multiple methods of spreading load across those servers (i.e. round-robin load balancing or consistent hashing).  Since cache proxy is providing consistent hashing, any Redis client method of supporting multiple servers will suffice.

## Command Pipelining

Cache proxy increases performance by pipelining requests to Redis. While pipelining can be performed at the client, the cache proxy is ideal due to connection aggregation. Pipelining reduces network roundtrips to Redis and lowers CPU usage on Redis.

## Read-Through Cache

Implementing caching strategies in cache proxy reduces the cost of implementing cache strategies in client code in multiple applications and languages. Cache proxy supports the read-through cache strategy, the most prevalent caching strategy used in distributed computing.

The read-through cache strategy is represented by the following sequence diagram:

![read-through strategy sequence diagram][readthrough-strategy]


The `CACHE_TTL` configuration option establishes how long the cache takes to become consistent with the backend server during a write (DELETE or PUT) to the backend server.  

A short `CACHE_TTL`, for example “15s”, reduces a significant amount of read pressure from Riak, increasing performance of the overall solution.


## Commands

For command details, refer to the Redis [documentation](http://redis.io/commands)

The Cache Proxy supports the following augmented Redis commands fully:

* GET - get the value of a key from Redis or Riak KV utilizing the read-through caching strategy with a TTL set at service configuration time.

The Cache Proxy also supports the set of Redis commands supported by Twemproxy, but only to the point of presharding and command pipelining, refer to the Twemproxy [documentation](https://github.com/twitter/twemproxy/blob/master/notes/redis.md)

*!IMPORTANT!* While the Cache Proxy does support issuing DEL commands, PEXPIRE with a small TTL is suggested instead when the semantic intent is to remove an item from cache.  With write-through, the DEL command will issue a delete to the Riak backend.
