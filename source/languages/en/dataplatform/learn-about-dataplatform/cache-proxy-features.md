---
title: Cache Proxy Features
project: dataplatform
version: 1.1.0+
toc: true
index: true
audience: beginner
---

[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html
[GET-sequence]: /images/GET_seq.msc.png
[SET-sequence]: /images/SET_seq.msc.png
[DEL-sequence]: /images/DEL_seq.msc.png
[Object-lifetime]: /images/Object_lifetime.msc.png

>Cache proxy write-around cache strategy is available to [Enterprise users only][ee].

##Overview

Basho Data Platform (BDP) cache proxy provides pre-sharding and connection
aggregation as a service, which reduces latency and increases addressable cache
memory space with lower cost hardware.

On this page, you will find detailed descriptions of cache proxy's components,
including what each component does and how you implement it. Cache proxy has the
following components:
 
* Pre-sharding
* Connection Aggregation
* Command Pipelining
* Read-through Cache
* Write-around Cache [Enterprise Edition][ee]
* Commands
* Object Lifetime

##Pre-sharding

Pre-sharding with consistent hashing dispatches object reads and writes based
on a configurable hash function, spreading load across multiple cache servers.
Cache proxy uses pre-sharding to extend the total addressable cache memory
space based on the number of Redis servers. Request keys are hashed, then
requests are routed to the Redis server that handles that portion of the key
range.

Redis with no persistence is used as the frontend cache proxy service, and
Redis as a data server holds all data in memory. The addressable memory of
cache proxy is limited. By employing pre-sharding, the total addressable cache
memory space is extended by the number of Redis servers.

##Connection Aggregation

Redis client connections are a limited resource. Using cache proxy, connections
may be spread across multiple cache proxy servers. This reduces the total
required connections to the Redis server for the same key.

Redis clients in various languages support specifying multiple servers, as well
as implementing multiple methods of spreading load across those servers (i.e.
round-robin load balancing or consistent hashing).  Since cache proxy is
providing consistent hashing, any Redis client method of supporting multiple
servers will suffice.

##Command Pipelining

Cache proxy increases performance by pipelining requests to Redis. While
pipelining can be performed at the client, the cache proxy is ideal due to
connection aggregation. Pipelining reduces network roundtrips to Redis and
lowers CPU usage on Redis.

##Read-Through Cache

Implementing caching strategies in cache proxy reduces the cost of implementing
cache strategies in client code in multiple applications and languages. Cache
proxy supports the read-through cache strategy, the most prevalent caching
strategy used in distributed computing.

The read-through cache strategy of the GET command is represented by the
following sequence diagram:

![GET command sequence diagram][GET-sequence]


The `CACHE_TTL` configuration option establishes how long the cache takes to
become consistent with the backend server during a write (DELETE or PUT) to the
backend server.

A short `CACHE_TTL`, for example "15s", reduces a significant amount of read
pressure from Riak, increasing performance of the overall solution.

##Write-Around Cache

In BDP 1.0, the read-through cache strategy was introduced, requiring a TTL to
keep cache as coherent as possible given writes to Riak KV can and will be
issued without the cache proxy being informed of the write. The effect is that
the cache proxy is eventually consistent with the underlying Riak KV data
store with the time to consistency equal to the TTL.

In BDP 1.1 [Enterprise Edition][ee], the cache proxy write-around cache strategy
was introduced to provide a means to keep cache coherent with zero time to
consistency with the underlying Riak KV data store for all writes that the cache
proxy is informed of. For the Redis String (Value in KV) datatype, SET and DEL
commands result in writes to the underlying Riak KV data store followed by a
PEXPIRE to invalidate cache.

Of the three write cache strategies, the write-around cache strategy is the least
prone to race condition, but least optimal for the read which immediately follows
the write. In the overwhelming majority of distributed application data access
patterns, the added certainty of cache coherency afforded by write-around over
write-through is well worth the single cache miss. By definition, a key that is
cached is expected to be accessed frequently, hence the single cache miss is
expected to be followed by several accurate cache hits.

The write-around cache strategy of the SET command is represented by the
following sequence diagram:

![SET command sequence diagram][SET-sequence]

The write-around cache strategy of the DEL command is represented by the
following sequence diagram:

![DEL command sequence diagram][DEL-sequence]

## Commands

For command details, refer to the Redis [documentation](http://redis.io/commands)

The Cache Proxy supports the following augmented Redis commands fully:

* GET - get the value of a key from Redis or Riak KV utilizing the read-through
  caching strategy with a TTL set at service configuration time.

* SET - set the value of a key to Riak KV and invalidate cache, issue a PEXPIRE
  to Redis.

* DEL - delete the value of a key to Riak KV and invalidate cache, issue a
  PEXPIRE to Redis.

The Cache Proxy also supports the set of Redis commands supported by Twemproxy,
but only to the point of presharding and command pipelining, issued only to
Redis, refer to the Twemproxy
[documentation](https://github.com/twitter/twemproxy/blob/master/notes/redis.md)

*!IMPORTANT!* While the Cache Proxy does support issuing DEL commands, PEXPIRE
with a small TTL is suggested instead when the semantic intent is to remove an
item from cache.  With write-around, the DEL command will issue a delete to the
Riak backend.

## Object Lifetime

With the combination of read-through and write-around cache strategies, the
full object lifetime for a key-value is represented by the following
sequence diagram:

![Object lifetime sequence diagram][Object-lifetime]

## Memory Usage

The memory each Redis server uses depends on the amount of cached data. By default, if a data set keeps growing, each Redis server will use all the available memory of the box it runs on. Older data is removed through an [eviction mechanism](http://redis.io/topics/lru-cache).

## Recommended number of Redis servers per BDP cluster

The number of Redis servers is highly dependent on: 

* amount of data used
* amount of “hot” data to be cached in memory
* number of connected clients
* usage patterns. 

If you are just starting to learn BDP, we suggest using at least 2 (better 3) Redis server instances. This approach has the benefit of not having a single point of failure and will work faster as client requests are distributed between those servers.

## Recommended number of Cache Proxies per BDP cluster

We recommend running Cache Proxy for each Redis server. This allows user applications (which use Redis driver) to connect to any Cache Proxy. All Proxy servers share the same configuration, including pool of Redis Servers, global value for TTL, etc. The overhead is very small and is easily justified by added benefits.
