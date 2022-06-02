---
title: "Developing with Riak Redis Add-on"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Develop with Redis Add-on"
    identifier: "add-ons_redis_develop"
    weight: 403
    parent: "add-ons_redis"
toc: true
commercial_offering: true
---

[redis-clients]: http://redis.io/clients
[usage bucket types]: {{<baseurl>}}riak/kv/2.0.5/developing/usage/bucket-types/
[dev api http]: {{<baseurl>}}riak/kv/2.0.5/developing/api/http
[config-behaviors]: http://basho.com/posts/technical/riaks-config-behaviors-part-4/
[apps replication properties]: {{<baseurl>}}riak/kv/2.0.5/developing/app-guide/replication-properties
[usage commit hooks]: {{<baseurl>}}riak/kv/2.0.5/developing/usage/commit-hooks/
[concept causal context]: {{<baseurl>}}riak/kv/2.0.5/learn/concepts/causal-context
[ee]: http://basho.com/contact/

This page will walk you through setting up your environment for development with Riak Redis Add-on (RRA), as well as present examples and configuration parameters for basic development operations.

## Overview

Riak Redis Add-on (RRA) packages a cache proxy service. The cache proxy service provides accessibility to Riak KV, as a persistent data store, with Redis, as a cache through the various Redis client libraries and command-line interface tool `redis-cli`.

As with Riak KV, the cache proxy service almost always performs best and most
predictably when you use the basic CRUD operations -- Create, Read, Update,
Delete -- that you'd find in any key/value store. Learning these operations
is a great place to start when beginning to develop applications that use
RRA.

The set of clients (including recommendations) for Redis are listed at
[Redis clients][redis-clients]. For brevity sake, examples provided here are
in: 

* Erlang (Eredis)
* Javascript (node_redis)
* Python (redis-py)
* Ruby (redis-rb)
* Scala (lettuce) 
* Java, see the Scala examples. The code intentionally uses as few Scala tricks as possible to focus on the use of the Redis client.

## Riak KV Setup

While you can use Riak Redis Add-on with Riak KV configured so either `last_write_wins` is set to 'true' or `allow_mult` is set to 'true', we recommend using the  `allow_mult` setting in order to provide client sibling resolution in the event of a network partition. The examples and instructions on this page will assume `allow_mult` is set to 'true'.

The cache proxy service is tested under both configurations. However, due to lack of support via the Redis protocol for returning multiple values for a single `GET`, effectively `last_write_wins` semantics apply.

For a deeper explanation of Riak KV's configurable behaviors, see John Daily's
blog series [part 4][config-behaviors] .

### Bucket Type Setup

The following is an example, using Riak KV's default HTTP port, of setting `allow_mult` to 'true' and `last_write_wins` to 'false':

```sh
curl -XPUT -H 'Content-Type: application/json' \
         -d '{"props": {"allow_mult": true, "last_write_wins": false}}' \
         'http://127.0.0.1:8098/buckets/test/props'
```

For additional configuration options see [bucket properties][dev api http].

## Object/Key Operations

Riak KV organizes data into buckets, keys, and values, with
[bucket types][usage bucket types] acting as an additional namespace in Riak KV
versions 2.0 and greater. Values, which we'll refer to as objects, are identifiable by a unique key, and each key/value pair is stored in a bucket. 

Objects accessed via the cache proxy service in Riak Redis Add-on are restricted to plaintext format. This plaintext format may be a simple string, JSON, XML, or other plaintext representations that can be parsed in the client application (e.g. YAML).

While buckets are a flat namespace in Riak KV and you can name them
whatever you'd like (`bucket` or `a90bf521c` or `___`), within the cache proxy
service, Redis bucket_type:bucket:key is mapped to Riak KV
bucket_type/bucket/key, so bucket type and bucket names should not contain
colon (`:`). When not specified, bucket type defaults to "default".

Outside of the above restriction, bucket names have no intrinsic significance beyond allowing you to store objects with the same key in different buckets. 

The same goes for naming keys: many objects can have the same key as long as they're in different buckets. There is no restriction on key containing colon (`:`), and this practice of representing a nested namespace is common in applications using Redis.

Riak KV [bucket types][usage bucket types] enable you to provide common
configurations for buckets (as many buckets as you wish). This means you can
easily enable buckets to share common configurations, i.e. identical
[replication properties][apps replication properties] or
[commit hooks][usage commit hooks].


## Reading Objects

Reads via the cache proxy service are analogous to a Redis `GET`, with the added benefit of reading-through to Riak KV which results in greater resilience through node outages and network partitions.

To request a value at a bucket/key in Riak KV, issue the following:

```erlang
{ok, RedisClientPid} = eredis:start_link("127.0.0.1", 22122).
{ok, Value} = eredis:q(RedisClientPid, ["GET", "test:food"]).
```

```javascript
var redis = require("redis"),
    client = redis.createClient();

client.get("test:food", redis.print);
```

```python
import redis

r = redis.StrictRedis(host="127.0.0.1", port=22122)

r.get("test:food")
```

```ruby
require "redis"

redis = Redis.new

redis.get("test:food")
```

```scala
import com.lambdaworks.redis._

var client = RedisClient.create("redis://127.0.0.1:22122")
var connection = client.connect()

var value = connection.get("test:food")
```

### Get Configuration Parameters

>**Note:** The cache proxy service read option (related to replication factor and
consistency concern) may optionally be set within the nutcracker.conf. This will  result in an override of the setting value at the bucket-level in Riak KV.


|Parameter       |Description      |Default|
|----------------|-----------------|-------|
|`n_val`         | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**NOTE**: If you change the `n_val` after keys have been added to the bucket it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions. | `3` |
|`pr`            | How many vnodes must respond for a read to be deemed successful. | `0` |
|`r`             | How many replicas need to agree when retrieving an existing object before responding. | `2` |
|`basic_quorum`  | Whether to return early in some failure cases, e.g. when `r`=1 and you get 2 errors and a success. | `0` (false) |
|`sloppy_quorum` | Whether to treat vnodes holding values for another vnode as acceptable within the quorum determination. | `0` (false) |
|`notfound_ok`   | Whether to treat notfounds as successful reads for the purpose of `r`. | 1 (true) |
|`timeout`       | The number of milliseconds to await a response. | `0` (server specified) |


### Sibling Resolution

As the Redis protocol does not provide a means to return multiple siblings,
the cache proxy service must provide server-side sibling resolution. At present, only last-write-wins sibling resolution is available. The result is an effective
last-write-wins configuration for access through the cache proxy service.


## Writing Objects

Writes via the cache proxy service are analogous to a Redis `SET`, with the added
benefit of writing to Riak KV followed by a `PEXPIRE` to Redis, invalidating
cache. As with HTTP PUT, `SET` semantically covers both create and update
operations.

To set a value at a bucket/key in Riak KV, issue the following:

```erlang
{ok, RedisClientPid} = eredis:start_link("127.0.0.1", 22122).
{ok, KeysAffected} = eredis:q(RedisClientPid, ["SET", "test:food", "apple"]).
```

```javascript
var redis = require("redis"),
    client = redis.createClient();

client.set("test:food", "apple", redis.print);
```

```python
import redis

r = redis.StrictRedis(host="127.0.0.1", port=22122)

r.set("test:food", "apple")
```

```ruby
require "redis"

redis = Redis.new

redis.set("test:food', 'apple")
```

```scala
import com.lambdaworks.redis._

var client = RedisClient.create("redis://127.0.0.1:22122")
var connection = client.connect()

connection.set("test:food", "apple")
```

### Set Configuration Parameters

>**Note:** The cache proxy service write option (related to replication factor and
consistency concern) may optionally be set within the nutcracker.conf, resulting
in an override of the setting value at the bucket-level in Riak KV.


|Parameter       |Description      |Default|
|----------------|-----------------|-------|
|`n_val`         | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**NOTE**: If you change the `n_val` after keys have been added to the bucket it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions. | `3` |
|`pw`            | How many vnodes must respond for a write to be deemed successful. | `0` |
|`w`             | How many replicas need to acknowledge the write before responding. | `2` |
|`sloppy_quorum` | Whether to treat vnodes holding values for another vnode as acceptable within the quorum determination. | `0` (false) |


### Sibling Explosion

As noted in the section "Sibling Resolution" above, Riak KV provides for a line of
descendency (known as the [causal context][[concept causal context]]) for a value stored at a key. Clients
performing write operations provide this causal context by setting the vector
clock (VClock) that they last read.

If a client does not provide the causal context, Riak KV makes no assumptions and treats the write as a new causal context, semantically equivalent to a
create. In the case that a value is already stored at the key, this would lead
to a sibling.

Since the Redis protocol does not provide a means to pass a VClock, the cache
proxy service needs to perform a read-before-write to obtain the current VClock so the write can continue the causal context previously established and avoid
"sibling explosion".

Despite these efforts, in the event of a network partition, siblings will still
be created as clients writing to nodes on either side of the network partition
can create divergent lines of descendency. Sibling resolution remains the means
to merge these lines of descent into a coherent causal context.

## Deleting Objects

Deletes via the cache proxy service are analogous to a Redis `DEL`, with the added
benefit of writing to Riak KV followed by a `PEXPIRE` to Redis, invalidating
cache.

To delete a value at a bucket/key in Riak KV, issue the following:

```erlang
{ok, RedisClientPid} = eredis:start_link("127.0.0.1", 22122).
{ok, KeysAffected} = eredis:q(RedisClientPid, ["DEL", "test:food"]).
```

```javascript
var redis = require("redis"),
    client = redis.createClient();

client.del("test:food", redis.print);
```

```python
import redis

r = redis.StrictRedis(host="127.0.0.1", port=22122)

r.del("test:food")
```

```ruby
require "redis"

redis = Redis.new

redis.del("test:food")
```

```scala
import com.lambdaworks.redis._

var client = RedisClient.create("redis://127.0.0.1:22122")
var connection = client.connect()

connection.del("test:food")
```

### Delete Configuration Parameters

|Parameter       |Description      |Default|
|----------------|-----------------|-------|
|`n_val`         | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**NOTE**: If you change the `n_val` after keys have been added to the bucket it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions. | `3` |
|`pw`            | How many vnodes must respond for a write to be deemed successful. | `0` |
|`w`             | How many replicas need to acknowledge the write before responding. | `2` |
|`sloppy_quorum` | Whether to treat vnodes holding values for another vnode as acceptable within the quorum determination. | `0` (false) |
