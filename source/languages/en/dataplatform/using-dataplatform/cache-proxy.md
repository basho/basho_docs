---
title: Using Cache Proxy
project: dataplatform
version: 1.1.0+
document: guide
toc: true
index: true
audience: beginner 
---

[redis-clients]: http://redis.io/clients
[bucket-types]: http://docs.basho.com/riak/latest/dev/advanced/bucket-types/
[bucket-props]: http://docs.basho.com/riak/latest/dev/references/http/set-bucket-props/
[config-behaviors]: http://basho.com/posts/technical/riaks-config-behaviors-part-4/
[replication-properties]: http://docs.basho.com/riak/latest/dev/advanced/replication-properties/
[commit-hooks]: http://docs.basho.com/riak/latest/dev/using/commit-hooks/

The cache proxy provides accessibility to Riak KV as a persistent data store
with Redis as a cache through the various Redis client libraries and
command-line interface tool `redis-cli`.

As with Riak KV, the cache proxy almost always performs best and most
predictably when you use the basic CRUD operations -- Create, Read, Update,
Delete -- that you'd find in any key/value store. Learning these operations
is a great place to start when learning how to develop applications that use
Riak KV.

The set of clients, including recommendations, for Redis are listed at
[Redis clients][redis-clients]. For brevity sake, examples provided here are
in Erlang (Eredis), Javascript (node_redis), Python (redis-py), Ruby (redis-rb),
and Scala (lettuce). For Java, see the Scala examples as the code used here
intentionally uses as few Scala tricks as possible to focus on the use of
the Redis client.

##Riak KV Setup

Riak KV may be configured with `last_write_wins` set to `true` or `allow_mult`
set to `true` with `allow_mult` being the preferred option to provide client
sibling resolution in the event of a network partition. The cache proxy is
tested under both configurations, though due to lack of support via the Redis
protocol for returning multiple values for a single `GET`, effectively
`last_write_wins` semantics apply.

For a deeper explanation of Riak KV's configurable behaviors, see John Daily's
blog series, [part 4][config-behaviors] .

### Bucket Type Setup

Throughout this tutorial, we are assuming a setup where `allow_mult` is `true`
and `last_write_wins` is `false`. The following is an example, using Riak KV's
default HTTP port, of setting these bucket properties.

```bash
curl -XPUT -H 'Content-Type: application/json' \
         -d '{"props": {"allow_mult": true, "last_write_wins": false}}' \
         'http://127.0.0.1:8098/buckets/test/props'
```

For additional configuration options,see [bucket properties][bucket-props]

##Object/Key Operations

Riak KV organizes data into buckets, keys, and values, with
[bucket types][bucket-types] acting as an additional namespace in Riak KV
versions 2.0 and greater. Values (also referred simply as objects in this
tutorial) are identifiable by a unique key, and each key/value pair is stored
in a bucket. Objects can be any data type you wish, e.g. JSON, XML, binary
data, plaintext, and more. However, for values accessed via the cache proxy,
the Object is restricted to plaintext which may be a simple string, JSON, XML,
or other plaintext representations which may be parsed in the client
application, e.g. YAML.

Buckets are essentially a flat namespace in Riak KV. You can name them
whatever you'd like, even `bucket` or `a90bf521c` or `___`. They have no
intrinsic significance beyond allowing you to store objects with the same key
in different buckets. The same goes for naming keys: many objects can have the
same key as long as they're in different buckets.

Within the cache proxy, Redis bucket:key is mapped to Riak KV bucket/key, so
bucket should not contain colon (`:`). There is no restriction on key
containing colon (`:`) and this practice of representing a nested namespace is
common in applications using Redis.

Riak KV [bucket types][bucket-types] enable you to provide common
configurations for buckets (as many buckets as you wish). This means you can
easily enable buckets to share common configurations, i.e. identical
[replication properties][replication-properties] or
[commit hooks][commit-hooks].

Cache proxy read and write options related to replication factor and
consistency concern may optionally be set within the nutcracker.conf, resulting
in an override of the setting value set at the bucket-level in Riak KV.

##Reading Objects

Reads via the cache proxy are analogous to a Redis `GET` with the added benefit
of reading-through to Riak KV, resulting in greater resilience through node
outages and network partitions.

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

###Get Configuration Parameters

|Parameter       |Description      |Default|
|----------------|-----------------|-------|
|`n_val`         | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**NOTE**: If you change the `n_val` after keys have been added to the bucket, it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions | `3` |
|`pr`            | How many vnodes must respond for a read to be deemed successful | `0` |
|`r`             | How many replicas need to agree when retrieving an existing object before responding | `2` |
|`basic_quorum`  | Whether to return early in some failure cases, e.g. when `r`=1 and you get 2 errors and a success | `0` (false) |
|`sloppy_quorum` | Whether to treat vnodes holding values for another vnode as acceptable within the quorum determination | `0` (false) |
|`notfound_ok`   | Whether to treat notfounds as successful reads for the purpose of `r` | 1 (true) |
|`timeout`       | The number of milliseconds to await a response | `0` (server specified) |

###Sibling Resolution

As the Redis protocol does not provide a means to return multiple siblings,
the cache proxy must provide server-side sibling resolution. At present, only
last-write-wins sibling resolution is available. The result is an effective
last-write-wins configuration for access through the cache proxy.

##Writing Objects

Writes via the cache proxy are analogous to a Redis `SET` with the added
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

###Set Configuration Parameters

|Parameter       |Description      |Default|
|----------------|-----------------|-------|
|`n_val`         | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**NOTE**: If you change the `n_val` after keys have been added to the bucket, it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions | `3` |
|`pw`            | How many vnodes must respond for a write to be deemed successful | `0` |
|`w`             | How many replicas need to acknowledge the write before responding | `2` |
|`sloppy_quorum` | Whether to treat vnodes holding values for another vnode as acceptable within the quorum determination | `0` (false) |

###Sibling Explosion

As noted in the section Sibling Resolution, Riak KV provides for a line of
descendency, known as the causal context, for a value stored at a key. Clients
performing write operations provide this causal context by setting the vector
clock (VClock) that they last read.

If a client does not provide the causal context, Riak KV makes no assumptions,
so treats the write as a new causal context, so semantically equivalent to a
create. In the case that a value is already stored at the key, this would lead
to a sibling.

Since the Redis protocol does not provide a means to pass a VClock, the cache
proxy needs to perform a read-before-write to obtain the current VClock so the
write can continue the causal context previously established, so avoid
"sibling explosion".

Despite these efforts, in the event of a network partition, siblings will still
be created as clients writing to nodes on either side of the network partition
can create divergent lines of descendency. Sibling resolution remains the means
to merge these lines of descent into a coherent causal context.

##Deleting Objects

Deletes via the cache proxy are analogous to a Redis `DEL` with the added
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

###Delete Configuration Parameters

|Parameter       |Description      |Default|
|----------------|-----------------|-------|
|`n_val`         | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**NOTE**: If you change the `n_val` after keys have been added to the bucket, it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions | `3` |
|`pw`            | How many vnodes must respond for a write to be deemed successful | `0` |
|`w`             | How many replicas need to acknowledge the write before responding | `2` |
|`sloppy_quorum` | Whether to treat vnodes holding values for another vnode as acceptable within the quorum determination | `0` (false) |

##In The Lab

For a reference demonstrating the Riak and Redis client interactions with the BDP Cache Proxy and Riak KV check out [rrrmatey](https://github.com/paegun/rrrmatey), an ODM (Object-Document-Mapper) framework for use with Basho Data Platform (BDP) Cache Proxy and Riak KV.
