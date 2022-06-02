---
title: "Replication"
description: ""
project: "riak_kv"
project_version: "2.1.1"
menu:
  riak_kv-2.1.1:
    name: "Replication"
    identifier: "usage_replication"
    weight: 115
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.1.1/dev/advanced/replication-properties
  - /riak/kv/2.1.1/dev/advanced/replication-properties
---

[usage bucket types]: {{<baseurl>}}riak/kv/2.1.1/developing/usage/bucket-types
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.1.1/learn/concepts/eventual-consistency
[plan backend leveldb]: {{<baseurl>}}riak/kv/2.1.1/setup/planning/backend/leveldb
[plan backend bitcask]: {{<baseurl>}}riak/kv/2.1.1/setup/planning/backend/bitcask
[use ref strong consistency]: {{<baseurl>}}riak/kv/2.1.1/using/reference/strong-consistency
[concept clusters]: {{<baseurl>}}riak/kv/2.1.1/learn/concepts/clusters

Riak was built to act as a multi-node [cluster][concept clusters].  It
distributes data across multiple physical servers, which enables it to
provide strong availability guarantees and fault tolerance.

The [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem), which
undergirds many of the design decisions behind Riak's architecture,
defines distributed systems in terms of three desired properties:
consistency, availability, and partition (i.e. failure) tolerance. Riak
can be used either as an AP, i.e. available/partition-tolerant, system
or as a CP, i.e. consistent/partition-tolerant, system. The former
relies on an [Eventual Consistency][concept eventual consistency] model, while the latter relies on
a special [strong consistency][use ref strong consistency] subsystem.

Although the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem)
dictates that there is a necessary trade-off between data consistency
and availability, if you are using Riak in an eventually consistent
manner, you can fine-tune that trade-off. The ability to make these
kinds of fundamental choices has immense value for your applications and
is one of the features that differentiates Riak from other databases.

At the bottom of the page, you'll find a [screencast]({{<baseurl>}}riak/kv/2.1.1/developing/app-guide/replication-properties#screencast) that briefly explains how to adjust your
replication levels to match your application and business needs.

{{% note title="Note on strong consistency" %}}
An option introduced in Riak version 2.0 is to use Riak as a
<a href="{{< baseurl >}}riak/kv/2.1.1/using/reference/strong-consistency/">strongly
consistent</a> system for data in specified buckets. Using Riak in this way is
fundamentally different from adjusting replication properties and fine-tuning
the availability/consistency trade-off, as it sacrifices _all_ availability
guarantees when necessary. Therefore, you should consult the
<a href="{{< baseurl >}}riak/kv/2.1.1/developing/app-guide/strong-consistency">Using Strong
Consistency</a> documentation, as this option will not be covered in this
tutorial.
{{% /note %}}

## How Replication Properties Work

When using Riak, there are two ways of choosing replication properties:
1. On a per-request basis
2. In a more programmatic fashion, [using bucket types][usage bucket types]

### Per-request Replication Properties

The simplest way to apply replication properties to objects stored in
Riak is to specify those properties

### Replication Properties Through Bucket Types

Let's say, for example, that you want to apply an `n_val` of 5, an `r`
of 3, and a `w` of 3 to all of the data in some of the [buckets]({{<baseurl>}}riak/kv/2.1.1/learn/concepts/buckets) that
you're using. In order to set those replication properties, you should
create a bucket type that sets those properties. Below is an example:

```bash
riak-admin bucket-type create custom_props '{"props":{"n_val":5,"r":3,"w":3}}'
riak-admin bucket-type activate custom_props
```

Now, any time you store an object in a bucket with the type
`custom_props` those properties will apply to it.

## Available Parameters

The table below lists the most frequently used replication parameters
that are available in Riak. Symbolic values like `quorum` are discussed
[below](#symbolic-consistency-names). Each
parameter will be explained in more detail in later sections:

Parameter | Common name | Default value | Description
:---------|:------------|:--------------|:-----------
`n_val` | N | `3` | Replication factor, i.e. the number of nodes in the cluster on which an object is to be stored
`r` | R | `quorum` | The number of servers that must respond to a read request
`w` | W | `quorum` | Number of servers that must respond to a write request
`pr` | PR | `0` | The number of primary <a href="../../../learn/concepts/vnodes/">vnodes</a> that must respond to a read request
`pw` | PW | `0` | The number of primary <a href="../../../learn/concepts/vnodes/">vnodes</a> that must respond to a write request
`dw` | DW | `quorum` | The number of servers that must report that a write has been successfully written to disk
`rw` | RW | `quorum` | If R and W are undefined, this parameter will substitute for both R and W during object deletes. It is extremely unlikely that you will need to adjust this parameter.
`notfound_ok` | | `true` | This parameter determines how Riak responds if a read fails on a node. Setting to `true` (the default) is the equivalent to setting R to 1: if the first node to respond doesn't have a copy of the object, Riak will immediately return a `not found` error. If set to `false`, Riak will continue to look for the object on the number of nodes specified by N (aka `n_val`).
`basic_quorum` | | `false` | If `notfound_ok` is set to `false`, Riak will be more thorough in looking for an object on multiple nodes. Setting `basic_quorum` to `true` in this case will instruct Riak to wait for only a `quorum` of responses to return a `notfound` error instead of N responses.

## A Primer on N, R, and W

The most important thing to note about Riak's replication controls is
that they can be at the bucket level. You can use [bucket types]({{<baseurl>}}riak/kv/2.1.1/developing/usage/bucket-types)
to set up bucket `A` to use a particular set of replication properties
and bucket `B` to use entirely different properties.

At the bucket level, you can choose how many copies of data you want to
store in your cluster (N, or `n_val`), how many copies you wish to read
from at one time (R, or `r`), and how many copies must be written to be
considered a success (W, or `w`).

In addition to the bucket level, you can also specify replication
properties on the client side for any given read or write. The examples
immediately below will deal with bucket-level replication settings, but
check out the [section below](#client-level-replication-settings)
for more information on setting properties on a per-operation basis.

The most general trade-off to be aware of when setting these values is
the trade-off between **data accuracy** and **client responsiveness**.
Choosing higher values for N, R, and W will mean higher accuracy because
more nodes are checked for the correct value on read and data is written
to more nodes upon write; but higher values will also entail degraded
responsiveness, especially if one or more nodes is failing, because Riak
has to wait for responses from more nodes.

## N Value and Replication

All data stored in Riak will be replicated to the number of nodes in the
cluster specified by a bucket's N value (`n_val`). The default `n_val`
in Riak is 3, which means that data stored in a bucket with the default
N will be replicated to three different nodes, thus storing three
**replicas** of the object.

In order for this to be effective, you need at least three nodes in your
cluster. The merits of this system, however, can be demonstrated using
your local environment.

Let's create a bucket type that sets the `n_val` for any bucket with
that type to 2. To do so, you must create and activate a bucket type
that sets this property:

```bash
riak-admin bucket-type create n_val_equals_2 '{"props":{"n_val":2}}'
riak-admin bucket-type activate n_val_equals_2
```

Now, all buckets that bear the type `n_val_equals_2` will have `n_val`
set to 2. Here's an example write:

```curl
curl -XPUT http://localhost:8098/types/n_val_equals_2/buckets/test_bucket/keys/test_key \
  -H "Content-Type: text/plain" \
  -d "the n_val on this write is 2"
```

Now, whenever we write to a bucket of this type, Riak will write a
replica of the object to two different nodes.

{{% note title="A Word on Setting the N Value" %}}
`n_val` must be greater than 0 and less than or equal to the number of actual
nodes in your cluster to get all the benefits of replication. We advise
against modifying the `n_val` of a bucket after its initial creation as this
may result in failed reads because the new value may not be replicated to all
the appropriate partitions.
{{% /note %}}

## R Value and Read Failure Tolerance

Read requests to Riak are sent to all N nodes that are known to be
currently responsible for the data. The R value (`r`) enables you to
specify how many of those nodes have to return a result on a given read
for the read to be considered successful. This allows Riak to provide
read availability even when nodes are down or laggy.

You can set R anywhere from 1 to N; lower values mean faster response
time but a higher likelihood of Riak not finding the object you're
looking for, while higher values mean that Riak is more likely to find
the object but takes longer to look.

As an example, let's create and activate a bucket type with `r` set to
`1`. All reads performed on data in buckets with this type require a
result from only one node.

```bash
riak-admin bucket-type create r_equals_1 '{"props":{"r":1}}'
riak-admin bucket-type activate r_equals_1
```

Here's an example read request using the `r_equals_1` bucket type:

```ruby
bucket = client.bucket_type('r_equals_1').bucket('animal_facts')
obj = bucket.get('chimpanzee')
```

```java
Location chimpanzeeFact =
  new Location(new Namespace("r_equals_1", "animal_facts"), "chimpanzee");
FetchValue fetch = new FetchValue.Builder(chimpanzeeFact).build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
System.out.println(obj.getValue().toString());
```

```php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('chimpanzee', 'animal_facts', 'r_equals_1')
  ->build()
  ->execute();

echo $response->getObject()->getData();
```

```python
bucket = client.bucket_type('r_equals_1').bucket('animal_facts')
bucket.get('chimpanzee')
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                                {<<"r_equals_1">>, <<"animal_facts">>},
                                <<"chimpanzee">>).
```

```curl
curl http://localhost:8098/types/r_equals_1/buckets/animal_facts/keys/chimpanzee
```

As explained above, reads to buckets with the `r_equals_1` type will
typically be completed more quickly, but if the first node to respond
to a read request has yet to receive a replica of the object, Riak will
return a `not found` response (which may happen even if the object lives
on one or more other nodes). Setting `r` to a higher value will mitigate
this risk.

## W Value and Write Fault Tolerance

As with read requests, writes to Riak are sent to all N nodes that are
know to be currently responsible for the data. The W value (`w`) enables
you to specify how many nodes must complete a write to be considered
successful---a direct analogy to R. This allows Riak to provide write
availability even when nodes are down or laggy.

As with R, you can set W to any value between 1 and N. The same
performance vs. fault tolerance trade-offs that apply to R apply to W.

As an example, let's create and activate a bucket type with `w` set to
`3`:

```bash
riak-admin bucket-type create w_equals_3 '{"props":{"w":3}}'
riak-admin activate w_equals_3
```

Now, we can attempt a write to a bucket bearing the type `w_equals_3`:

```ruby
bucket = client.bucket_type('w_equals_3').bucket('animal_facts')
obj = Riak::RObject.new(bucket, 'giraffe')
obj.raw_data = 'The species name of the giraffe is Giraffa camelopardalis'
obj.content_type = 'text/plain'
obj.store
```

```java
Location storyKey =
  new Location(new Namespace("w_equals_3", "animal_facts"), "giraffe");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(BinaryValue.create("The species name of the giraffe is Giraffa camelopardalis"));
StoreValue store = new StoreValue.Builder(obj)
        .withLocation("giraffe")
        .build();
client.execute(store);
```

```php
(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('giraffe', 'animal_facts', 'w_equals_3')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('w_equals_3').bucket('animal_facts')
obj = RiakObject(client, bucket, 'giraffe')
obj.content_type = 'text/plain'
obj.data = 'The species name of the giraffe is Giraffa camelopardalis'
obj.store()
```

```erlang
Obj = riakc_object:new({<<"w_equals_3">>, <<"animal_facts">>},
                       <<"giraffe">>,
                       <<"The species name of the giraffe is Giraffa camelopardalis">>,
                       <<"text/plain">>),
riakc_pb_socket:put(Pid, Obj).
```

```curl
curl -XPUT \
  -H "Content-type: text/plain" \
  -d "The species name of the giraffe is Giraffa camelopardalis" \
  http://localhost:8098/types/w_equals_3/buckets/animal_facts/keys/giraffe
```

Writing our `story.txt` will return a success response from Riak only if
3 nodes respond that the write was successful. Setting `w` to 1, for
example, would mean that Riak would return a response more quickly, but
with a higher risk that the write will fail because the first node it
seeks to write the object to is unavailable.

## Primary Reads and Writes with PR and PW

In Riak's replication model, there are N [vnodes]({{<baseurl>}}riak/kv/2.1.1/learn/glossary/#vnode),
called _primary vnodes_, that hold primary responsibility for any given
key. Riak will attempt reads and writes to primary vnodes first, but in
case of failure, those operations will go to failover nodes in order to
comply with the R and W values that you have set. This failover option
is called _sloppy quorum_.

In addition to R and W, you can also set integer values for the *primary
read* (PR) and _primary write_ (PW) parameters that specify how many
primary nodes must respond to a request in order to report success to
the client. The default for both values is zero.

Setting PR and/or PW to non-zero values produces a mode of operation
called _strict quorum_. This mode has the advantage that the client is
more likely to receive the most up-to-date values, but at the cost of a
higher probability that reads or writes will fail because primary vnodes
are unavailable.

{{% note title="Note on PW" %}}
If PW is set to a non-zero value, there is a higher risk (usually very small)
that failure will be reported to the client upon write. But this does not
necessarily mean that the write has failed completely. If there are reachable
primary vnodes, those vnodes will still write the new data to Riak. When the
failed vnode returns to service, it will receive the new copy of the data via
either read repair or active anti-entropy.
{{% /note %}}

## Durable Writes with DW

The W and PW parameters specify how many vnodes must _respond_ to a
write in order for it to be deemed successful. What they do not specify
is whether data has actually been written to disk in the storage backend.
The DW parameters enables you to specify a number of vnodes between 1
and N that must write the data to disk before the request is deemed
successful. The default value is `quorum` (more on symbolic names below).

How quickly and robustly data is written to disk depends on the
configuration of your backend or backends. For more details, see the
documentation on [Bitcask][plan backend bitcask], [LevelDB][plan backend leveldb], and [multiple backends]({{<baseurl>}}riak/kv/2.1.1/setup/planning/backend/multi).

## Delete Quorum with RW

{{% note title="Deprecation notice" %}}
It is no longer necessary to specify an RW value when making delete requests.
We explain its meaning here, however, because RW still shows up as a property
of Riak buckets (as `rw`) for the sake of backwards compatibility. Feel free
to skip this explanation unless you are curious about the meaning of RW.
{{% /note %}}

Deleting an object requires successfully reading an object and then
writing a tombstone to the object's key that specifies that an object
once resided there. In the course of their operation, all deletes must
comply with any R, W, PR, and PW values that apply along the way.

If R and W are undefined, however, the RW (`rw`) value will substitute
for both R and W during object deletes. In recent versions of Riak, it
is nearly impossible to make reads or writes that do not somehow specify 
oth R and W, and so you will never need to worry about RW.

## The Implications of `notfound_ok`

The `notfound_ok` parameter is a bucket property that determines how
Riak responds if a read fails on a node. If `notfound_ok` is set to
`true` (the default value) and the first vnode to respond doesn't have a
copy of the object, Riak will assume that the missing value is
authoritative and immediately return a `not found` result to the client.
This will generally lead to faster response times.

On the other hand, setting `notfound_ok` to `false` means that the
responding vnode will wait for something other than a `not found` error
before reporting a value to the client. If an object doesn't exist under
a key, the coordinating vnode will wait for N vnodes to respond with
`not found` before it reports `not found` to the client. This setting
makes Riak search more thoroughly for objects but at the cost of slower
response times, a problem can be mitigated by setting `basic_quorum` to
`true`, which is discussed in the next section.

## Early Failure Return with `basic_quorum`

Setting `notfound_ok` to `false` on a request (or as a bucket property)
is likely to introduce additional latency. If you read a non-existent
key, Riak will check all 3 responsible vnodes for the value before
returning `not found` instead of checking just one.

This latency problem can be mitigated by setting `basic_quorum` to
`true`, which will instruct Riak to query a quorum of nodes instead of N
nodes. A quorum of nodes is calculated as floor(N/2) + 1, meaning that 5
nodes will produce a quorum of 3, 6 nodes a quorum of 4, 7 nodes a
quorum of 4, 8 nodes a quorum of 5, etc.

The default for `basic_quorum` is `false`, so you will need to
explicitly set it to `true` on reads or in a bucket's properties. While
the scope of this setting is fairly narrow, it can reduce latency in
read-heavy use cases.

## Symbolic Consistency Names

Riak provides a number of "symbolic" consistency options for R, W, PR,
RW, and DW that are often easier to use and understand than specifying
integer values. The following symbolic names are available:

* `all` --- All replicas must reply. This is the same as setting R, W, PR, RW, or DW equal to N.
* `one` --- This is the same as setting 1 as the value for R, W, PR, RW, or DW.
* `quorum` --- A majority of the replicas must respond, that is, half plus one. For the default N value of 3, this calculates to 2, an N value of 5 calculates to 3, and so on.
* `default` --- Uses whatever the per-bucket consistency property is for R, W, PR, RW, or DW, which may be any of the above symbolic values or an integer.

Not submitting a value for R, W, PR, RW, or DW is the same as using
`default`.

## Client-level Replication Settings

Adjusting replication properties at the bucket level by [using bucket types][usage bucket types]
is how you set default properties for _all_ of a bucket's reads and
writes. But you can also set replication properties for specific reads
and writes without setting those properties at the bucket level, instead
specifying them on a per-operation basis.

Let's say that you want to set `r` to 2 and `notfound_ok` to `true` for
just one read. We'll fetch [John Stockton](http://en.wikipedia.org/wiki/John_Stockton)'s
statistics from the `nba_stats` bucket.

```ruby
bucket = client.bucket('nba_stats')
obj = bucket.get('john_stockton', r: 2, notfound_ok: true)
```

```java
Location johnStocktonStats =
  new Namespace(new Namespace("nba_stats"), "john_stockton");
FetchValue fetch = new FetchValue.Builder(johnStocktonStats)
        .withOption(FetchOption.R, new Quorum(2))
        .withOption(FetchOption.NOTFOUND_OK, true)
        .build();
client.execute(fetch);
```

```php
(new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('john_stockton', 'nba_stats')
  ->withParameter('r', 2)
  ->withParameter('notfound_ok', true)
  ->build()
  ->execute();
```

```python
bucket = client.bucket('nba_stats')
obj = bucket.get('john_stockton', r=2, notfound_ok=True)
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                                <<"nba_stats">>,
                                <<"john_stockton">>,
                                [{r, 2}, {notfound_ok, true}]).
```

```curl
curl http://localhost:8098/buckets/nba_stats/keys/john_stockton?r=2&notfound_ok=true
```

Now, let's say that you want to attempt a write with `w` set to 3 and
`dw` set to 2. As in the previous example, we'll be using the `default`
bucket type, which enables us to not specify a bucket type upon write.
Here's what that would look like:

```ruby
bucket = client.bucket('nba_stats')
obj = Riak::RObject.new(bucket, 'michael_jordan')
obj.content_type = 'application/json'
obj.data = '{"stats":{ ... large stats object ... }}'
obj.store(w: 3, dw: 2)
```

```java
Location michaelJordanKey =
  new Location(new Namespace("nba_stats"), "michael_jordan");
RiakObject obj = new RiakObject()
        .setContentType("application/json")
        .setValue(BinaryValue.create("{'stats':{ ... large stats object ... }}"));
StoreValue store = new StoreValue.Builder(obj)
        .withLocation(michaelJordanKey)
        .withOption(StoreOption.W, new Quorum(3))
        .withOption(StoreOption.DW, new Quorum(2))
        .build();
client.execute(store);
```

```php
(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildJsonObject('{'stats':{ ... large stats object ... }}')
  ->buildLocation('john_stockton', 'nba_stats')
  ->withParameter('w', 3)
  ->withParameter('dw', 2)
  ->build()
  ->execute();
```

```erlang
Obj = riakc_obj:new(<<"nba_stats">>,
                    <<"michael_jordan">>,
                    <<"{'stats':{ ... large stats object ... }}">>,
                    <<"application/json">>),
riakc_pb_socket:put(Pid, Obj).
```

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d '{"stats":{ ... large stats object ... }}' \
  http://localhost:8098/buckets/nba_stats/keys/michael_jordan?w=3&dw=2
```

All of Basho's [official Riak clients]({{<baseurl>}}riak/kv/2.1.1/developing/client-libraries) enable you to
set replication properties this way. For more detailed information,
refer to the tutorial on [basic key/value operations in Riak KV]({{<baseurl>}}riak/kv/2.1.1/developing/getting-started)
or to client-specific documentation:

* [Ruby](https://github.com/basho/riak-ruby-client/blob/master/README.md)
* [Java](http://basho.github.io/riak-java-client/2.0.0/)
* [Python](http://basho.github.io/riak-python-client/)
* [Erlang](http://basho.github.io/riak-erlang-client/)

## Illustrative Scenarios

In case the above explanations were a bit too abstract for your tastes,
the following table lays out a number of possible scenarios for reads
and writes in Riak and how Riak is likely to respond. Some of these
scenarios involve issues surrounding conflict resolution, vector clocks,
and siblings, so we recommend reading the [Vector Clocks]({{<baseurl>}}riak/kv/2.1.1/learn/concepts/causal-context#vector-clocks) documentation for more information.

#### Read Scenarios

These scenarios assume that a read request is sent to all 3 primary
vnodes responsible for an object.

Scenario | What happens in Riak
:--------|:--------------------
All 3 vnodes agree on the value | Once the first 2 vnodes return the value, that value is returned to the client
2 of 3 vnodes agree on the value, and those 2 are the first to reach the coordinating node | The value is returned to the client. Read repair will deal with the conflict per the later scenarios, which means that a future read may return a different value or <a href="{{< baseurl >}}riak/kv/2.1.1/learn/concepts/causal-context#siblings">siblings</a>
2 conflicting values reach the coordinating node and <a href="{{< baseurl >}}riak/kv/2.1.1/learn/concepts/causal-context#vector-clocks">vector clocks</a> allow for resolution | The vector clocks are used to resolve the conflict and return a single value, which is propagated via read repair to the relevant vnodes
2 conflicting values reach the coordinating node, vector clocks indicate a fork in the object history, and `allow_mult` is set to `false` | The object with the most recent timestamp is returned and propagated via read repair to the relevant vnodes
2 siblings or conflicting values reach the coordinating node, vector clocks indicate a fork in the object history, and `allow_mult` is set to `true` | All keys are returned as siblings, optionally with associated values (depending on how the request is made)

#### Write Scenarios

These scenarios assume that a write request is sent to all 3 primary
vnodes responsible for an object.

Scenario | What happens in Riak
:--------|:--------------------
A vector clock is included with the write request, and is newer than the vclock attached to the existing object | The new value is written and success is indicated as soon as 2 vnodes acknowledge the write
A vector clock is included with the write request but conflicts with the vclock attached to the existing object, with `allow_mult` set to `true` | The new value is created as a sibling for future reads
A vector clock is included with the write request but conflicts with (or is older than) the vclock attached to the existing object, with `allow_mult` set to `false` | Riak will decide which object "wins" on the basis of timestamps; no sibling will be created
A vector clock is not included with the write request and an object already exists, with `allow_mult` set to `true` | The new value is created as a sibling for future reads
A vector clock is not included with the write request and an object already exists, with `allow_mult` set to `false` | The new value overwrites the existing value

## Screencast

Here is a brief screencast that shows just how the N, R, and W values
function in our running 3-node Riak cluster:

<div style="display:none" class="iframe-video"
id="http://player.vimeo.com/video/11172656"></div>

<a href="http://vimeo.com/11172656">Tuning CAP Controls in Riak</a> from
<a href="http://vimeo.com/bashotech">Basho Technologies</a> on <a
href="http://vimeo.com">Vimeo</a>.
