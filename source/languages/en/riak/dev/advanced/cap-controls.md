---
title: Replication Properties
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [developers, cap, replication]
interest: [
"[[Installing and Upgrading]]",
"[[Concepts]]",
"[[Planning for a Riak System]]",
"[[Cluster Capacity Planning]]",
"[[Use Cases]]"
]
moved: {
  '1.4.0-': '/tutorials/fast-track/Tunable-CAP-Controls-in-Riak'
}
---

Riak was built with the assumption that a Riak installation acts as a multi-node [[cluster|Clusters]], distributing data across multiple physical servers.

Riak's guiding design principle is Dr. Eric Brewer's [CAP Theorem](http://en.wikipedia.org/wiki/CAP_theorem). The CAP theorem defines distributed systems in terms of three desired properties: consistency, availability, and partition (i.e. failure) tolerance. Riak chooses to focus on the A and P of CAP, which puts it in the eventually consistent camp. It should be stated, however, that the window for "eventually consistent" is usually in the neighborhood of milliseconds, which can be good enough for many applications.

Although the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem) dictates that there is a necessary trade-off between data consistency and availability, Riak enables you to fine-tune that trade-off. The ability to make these kinds of fundamental choices has immense value for your applications and is one of the features that we feel truly differentiates Riak from other technologies.

At the bottom of the page, you'll find a screencast that briefly explains how to adjust your replication levels to match your application and business needs.

The table below lists the most frequently used replication parameters that are available in Riak. Symbolic values like `quorum` are discussed [[below|Replication Properties#symbolic-consistency-names]]. Each parameter will be explained in more detail in the sections below:

Parameter | Common name | Default value | Description
:---------|:------------|:--------------|:-----------
`n_val` | N | `3` | Replication factor, i.e. the number of nodes in the cluster on which an object is to be stored
`r` | R | `quorum` | The number of servers that must respond to a read request
`w` | W | `quorum` | Number of servers that must respond to a write request
`pr` | PR | `0` | The number of primary [[vnodes|Riak Glossary#vnode]] that must respond to a read request
`pw` | PW | `0` | The number of primary [[vnodes|Riak Glossary#vnode]] that must respond to a write request
`dw` | DW | `quorum` | The number of servers that must report that a write has been successfully written to disk

The following parameters are also available to define the replication properties of a bucket, bucket type, or specific request. They tend to be seldom used, but you may either encounter them when viewing the properties associated with a bucket type and in some rare cases you may need to adjust them yourself:

Parameter | Common name | Default value | Description
:---------|:------------|:--------------|:-----------
`rw` | RW | `quorum` | If R and W are undefined, this parameter will substitute for both R and W during object deletes. It is extremely unlikely that you will need to adjust this parameter.
`notfound_ok` | N/A | `true` | This parameter determines how Riak responds if a read fails on a node. Setting to `true` (the default) is the equivalent to setting R to 1: if the first node to respond doesn't have a copy of the object, Riak will immediately return a `not found` error. If set to `false`, Riak will continue to look for the object on the number of nodes specified by N (aka `n_val`).
`basic_quorum` | N/A | `false` | If `notfound_ok` is set to `false`, Riak will be more thorough in looking for an object on multiple nodes. Setting `basic_quorum` to `true` in this case will instruct Riak to wait for only a `quorum` of reponses to return a `notfound` error instead of N responses.

## A Primer on N, R, and W

The most important thing to note about Riak's replication controls is that they can be at the bucket level. You can use [[bucket types|Using Bucket Types]] to set up bucket `A` to use a particular set of replication properties and bucket `B` to use entirely different properties.

At the bucket level, you can choose how many copies of data you want to store in your cluster (N, or `n_val`), how many copies you wish to read from at one time (R, or `r`), and how many copies must be written to be considered a success (W, or `w`).

In addition to the bucket level, you can also specify replication properties on the client side for any given read or write. The examples immediately below will deal with bucket-level replication settings, but check out the [[section below|Replication Properties#client-level-replication-settings]] for more information on setting properties on a per-operation basis.

<div class="note">
<div class="title">Note on strong consistency</div>
An option introduced in Riak version 2.0 is to use Riak as a <a href="/theory/concepts/strong-consistency/">strongly consistent</a> system for data in specified buckets. Using Riak in this way is fundamentally different from adjusting replication properties and fine-tuning the availability/consistency trade-off, as it sacrifices <em>all</em> availability guarantees when necessary. Therefore, you should consult the <a href="/dev/advanced/strong-consistency">Using Strong Consistency</a> documentation, as this option will not be covered in this tutorial.
</div>

The most general trade-off to be aware of when setting these values is the trade-off between **data accuracy** and **client responsiveness**. Choosing higher values for N, R, and W will mean higher accuracy because more nodes are checked for the correct value on read and data is written to more nodes upon write; but higher values will also entail degraded responsiveness, especially if one or more nodes is failing, because Riak has to wait for more responses.

## N Value and Replication

All data stored in Riak will be replicated to the number of nodes in the cluster specified by a bucket's N value (`n_val`). The default `n_val` in Riak is 3, which means that data stored in a bucket with the default N will be replicated to three different nodes, thus storing three replicas of the object.

In order for this to be effective, you need at least three nodes in your cluster. The merits of this system, however, can be demonstrated using your local environment.

Let's create a bucket type that sets the `n_val` for any bucket with that type to 2. To do so, you must create and activate a bucket type that sets this property:

```bash
riak-admin bucket-type create n_val_equals_2 '{"props":{"n_val":2}}'
riak-admin bucket-type activate n_val_equals_2
```

Now, all buckets that bear the type `n_val_equals_2` will have `n_val` set to 2. Here's an example write:

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "the n_val on this write is 2" \
  http://localhost:8098/types/n_val_equals_2/buckets/test_bucket/keys/test_key
```

Now, whenever we write to a bucket of this type, Riak will write two replicas to two different nodes.

<div class="note">
<div class="title">A Word on Setting the N Value</div>
<tt>n_val</tt> must be greater than 0 and less than or equal to the number of actual nodes in your cluster to get all the benefits of replication. We advise against modifying the <tt>n_val</tt> of a bucket after its initial creation as this may result in failed reads because the new value may not be replicated to all the appropriate partitions.
</div>

## R Value and Read Failure Tolerance

Read requests to Riak are sent to all N nodes that are know to be currently responsible for the data. The R value (`r`) enables you to specify how many of those nodes have to return a result on a given read for the read to be considered successful. This allows Riak to provide read availability even when nodes are down or laggy.

You can set R anywhere from 1 to N; lower values mean faster response time but a higher likelihood of Riak not finding the object you're looking for, while higher values mean that Riak is more likely to find the object but takes longer to look.

As an example, let's create and activate a bucket type with `r` set to `1`. All reads performed on data in buckets with this type require a result from only one node.

```bash
riak-admin bucket-type create r_equals_1 '{"props":{"r":1}}'
riak-admin bucket-type activate r_equals_1
```

Here's an example read request using the `r_equals_1` bucket type:

```ruby
bucket = client.bucket('animal_facts')
obj = bucket.get('chimpanzee', bucket_type: 'r_equals_1')
```

```java
Location chimpanzeeFact = new Location("animal_facts")
        .setBucketType("r_equals_1")
        .setKey("chimpanzee");
FetchValue fetch = new FetchValue.Builder(chimpanzeeFact).build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
System.out.println(obj.getValue().toString());
```

```python
bucket = client.bucket('animal_facts', bucket_type='r_equals_1')
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

As explained above, reads to buckets with the `r_equals_1` type will typically be completed more quickly, but if the first node where Riak attempts to find the object is down, Riak will return a `not found` response (which may happen even if the object lives on one or more other nodes). Setting `r` to a higher value will mitigate this risk.

## W Value and Write Fault Tolerance

As with read requests, writes to Riak are sent to all N nodes that are know to be currently responsible for the data. The W value (`w`) enables you to specify how many nodes must report success on writes for the write to be considered successful---a direct analogy to R. This allows Riak to provide write availability even when nodes are down or laggy.

As with R, you can set W to any value between 1 and N. The same performance vs. fault tolerance trade-offs that apply to R apply to W.

As an example, let's create and activate a bucket type with `w` set to `3`:

```bash
riak-admin bucket-type create w_equals_3 '{"props":{"w":3}}'
riak-admin activate w_equals_3
```

Now, we can attempt a write to a bucket bearing the type `w_equals_3`:

```ruby
bucket = client.bucket('animal_facts')
obj = Riak::RObject.new(bucket, 'giraffe')
obj.raw_data = 'The species name of the giraffe is Giraffa camelopardalis'
obj.content_type = 'text/plain'
obj.store(bucket_type: 'w_equals_3')
```

```java
Location storyKey = new Location("animal_facts")
        .setBucketType("w_equals_3")
        .setKey(filename);
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(BinaryValue.create("The species name of the giraffe is Giraffa camelopardalis"));
StoreValue store = new StoreValue.Builder(obj)
        .withLocation("giraffe")
        .build();
client.execute(store);
```

```python
bucket = client.bucket('animal_facts', bucket_type='w_equals_3')
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

Writing our `story.txt` will return a success response from Riak only if three nodes respond that the write was successful. Setting `w` to 1, for example, would mean that Riak would return a response more quickly, but with a higher risk that the write will fail because the first node it seeks to write the object to is down.

## Primary Reads and Writes with PR and PW

In Riak's replication model, there are N [[vnodes|Riak Glossay#vnodes]], called *primary vnodes*, that hold primary responsibility for any given key. Riak will attempt reads and writes to primary vnodes first, but in case of failure, those operations will go to failover nodes in order to comply with the R and W values that you have set. This failover option is called *sloppy quorum*.

In addition to R and W, you can also set integer values for the *primary read* (PR) and *primary write* (PW) parameters that specify how many primary nodes must respond to a request in order to report success to the client. The default for both values is zero.

Setting PR and/or PW to non-zero values produces a mode of operation called *strict quorum*. This mode has the advantage that the client is more likely to receive the most up-to-date values, but at the cost of a higher probability that reads or writes will fail because primary vnodes are unavailable.

<div class="note">
<div class="title">Note on PW</div>
If PW is set to a non-zero value, there is a higher risk (usually very small) that failure will be reported to the client upon write. But this does not necessarily mean that the write has failed completely. If there are reachable primary vnodes, those vnodes will still write the new data to Riak. When the failed vnode returns to service, it will receive the new copy of the data via either read repair or Active Anti-Entropy.
</div>

## Durable Writes with DW

The W and PW parameters specify how many vnodes must _respond_ to a write in order for it to be deemed successful. What they do not specify is whether data has actually been written to disk in the storage backend. The DW parameters enables you to specify a number of vnodes between 1 and N that must write the data to disk before the request is deemed successful. The default is `quorum` (more on symbolic names below).

How quickly and robustly data is written to disk depends on the configuration of your backend or backends. For more details, see the documentation on [[Bitcask]], [[LevelDB]], and [[multiple backends|Multi]].

## Delete Quorum with RW

<div class="note"><div class="title">Deprecation notice</div>
It is no longer necessary to specify an RW value when making delete requests. We explain its meaning here, however, because RW still shows up as a property of Riak buckets (as <tt>rw</tt>) for the sake of backwards compatibility. Feel free and skip this explanation unless you are curious about what RW means.
</div> 

Deleting an object requires successfully reading an object and then writing a tombstone to the object's key that specifies that an object once resided there. In the course of their operation, all deletes must comply with any R, W, PR, and PW values that apply along the way.

If R and W are undefined, however, the RW (`rw`) value will substitute for both R and W during object deletes. In recent versions of Riak, it is nearly impossible to make reads or writes that do not somehow specify both R and W, and so you will likely never need to worry about RW.

## The Implications of `notfound_ok`

The `notfound_ok` parameter is a bucket property that determines how Riak responds if a read fails on a node. If `notfound_ok` is set to `true` (this is the default) is the equivalent to setting R to 1: if the first vnode to respond doesn't have a copy of the object, Riak will deem the failure authoritative and immediately return a `not found` error to the client.

On the other hand, setting `notfound_ok` to `false` means that the responding vnode will wait for something other than a `not found` error before reporting a value to the client. If an object doesn't exist under a key, the coordinating vnode will wait for N vnodes to respond with `not found` before it reports `not found` to the client.

In general, setting `notfound_ok` to `true` will return any `not found`responses to the client more quickly, instead of potentially leaving clients hanging on the line, with the drawback that Riak will falsely return `not found` if it turns out that the data lives on a vnode that is not checked for the value.

Setting `notfound_ok` to `false` will be more thorough in checking for the value but at the cost of a small performance hit in cases where the coordinating vnode waits for responses from all vnodes and all return a `not found`. This problem can be mitigated by setting `basic_quorum` to `true`, which is discussed in the next section.

## Early Failure Return with `basic_quorum`

Setting `notfound_ok` to `false` on a request (or as a bucket property) is likely to introduce additional latency. If you read a non-existent key, Riak will check all three responsible vnodes for the value before returning `not found` instead of checking just one.

This latency problem can be mitigated by setting `basic_quorum` to `true`. If N is set to 3, for example, Riak will return `not found` after querying only 2 vnodes instead of 3. If N is set to 5, 3 nodes need to return `not found` (and so on).

The default for `basic_quorum` is `false`, so you will need to explicitly set it to `true` on reads or in a bucket's properties. While the scope of this setting is fairly narrow, it can greatly reduce latency in read-heavy use cases.

## Symbolic Consistency Names

Riak provides a number of "symbolic" consistency options for R, W, PR, RW, and DW that are often easier to use and understand than specifying integer values. The following symbolic names are available:

* `all` --- All replicas must reply. This is the same as setting R, W, PR, RW, or DW equal to N.
* `one` --- This is the same as setting 1 as the value for R, W, PR, RW, or DW.
* `quorum` --- A majority of the replicas must respond, that is, half plus one. For the default N value of 3, this calculates to 2, an N value of 5 calculates to 3, and so on.
* `default` --- Uses whatever the per-bucket consistency property is for R, W, PR, RW, or DW, which may be any of the above symbolic values or an integer.

Not submitting a value for R, W, PR, RW, or DW is the same as using `default`.

## Client-level Replication Settings

Adjusting replication properties at the bucket level, using [[bucket types|Using Bucket Types]], is a way of managing those properties at an abstract level, determining those properties for _all_ reads from and writes to a bucket. But you can also set replication properties for a given read or write without setting those properties at the bucket level, specifying them on a per-operation basis.

Let's say that you want to set `r` to 2 and `notfound_ok` to `true` for just one read. We'll fetch [John Stockton](http://en.wikipedia.org/wiki/John_Stockton)'s statistics from the `nba_stats` bucket. We won't specify a [[bucket type|Using Bucket Types]], which means that Riak will use the `default` bucket type.

```ruby
bucket = client.bucket('nba_stats')
obj = bucket.get('john_stockton', r: 2, notfound_ok: true)
```

```java
Location johnStocktonStats = new Location("nba_stats")
        .setKey("john_stockton");
FetchValue fetch = new FetchValue.Builder(johnStocktonStats)
        .withOption(FetchOption.R, new Quorum(2))
        .withOption(FetchOption.NOTFOUND_OK, true)
        .build();
client.execute(fetch);
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

Now, let's say that you want to attempt a write with `w` set to 3 and `dw` set to 2. As in the previous example, we'll be using the `default` bucket type, which enables us to not specify a bucket type upon write. Here's what that would look like:

```ruby
bucket = client.bucket('nba_stats')
obj = Riak::RObject.new(bucket, 'michael_jordan')
obj.content_type = 'application/json'
obj.data = '{"stats":{ ... large stats object ... }}'
obj.store(w: 3, dw: 2)
```

```java
Location michaelJordanKey = new Location("nba_stats")
        .setKey("michael_jordan");
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

All of Basho's [[official Riak clients|Client Libraries]] enable you to set replication properties this way. For more detailed information, refer to the tutorial on [[basic key/value operations in Riak|The Basics]] or to client-specific documentation:

* [Ruby](https://github.com/basho/riak-ruby-client/blob/master/README.markdown)
* [Java](http://basho.github.io/riak-java-client/2.0.0-SNAPSHOT/)
* [Python](http://basho.github.io/riak-python-client/)
* [Erlang](http://basho.github.io/riak-erlang-client/)

## Illustrative Scenarios

In case the above explanations were a bit too abstract for your tastes, the following table lays out a number of possible scenarios for reads and writes in Riak and how Riak is likely to respond. Some of these scenarios involve issues surrounding conflict resolution, vector clocks, and siblings, so we recommend reading the [[Vector Clocks]] documentation for more information.

#### Read Scenarios

These scenarios assume that a read request is sent to all 3 primary vnodes responsible for an object.

Scenario | What happens in Riak
:--------|:--------------------
All 3 vnodes agree on the value | Once the first 2 vnodes return the value, that value is returned to the client
2 of 3 vnodes agree on the value, and those 2 are the first to reach the coordinating node | The value is returned returned to the client. Read repair will deal with the conflict per the later scenarios, which means that a future read may return a different value or [[siblings|Vector Clocks#siblings]]
2 conflicting values reach the coordinating node and [[vector clocks]] allow for resolution | The vector clocks are used to resolve the conflict and return a single value, which is propagated via read repair to the relevant vnodes
2 conflicting values reach the coordinating node, vector clocks indicate a fork in the object history, and `allow_mult` is set to `false` | The object with the most recent timestamp is returned and propagated via read repair to the relevant vnodes
2 siblings or conflicting values reach the coordinating node, vector clocks indicate a fork in the object history, and `allow_mult` is set to `true` | All keys are returned as siblings, optionally with associated values (depending on how the request is made)

#### Write Scenarios

These scenarios assume that a write request is sent to all 3 primary vnodes responsible for an object.

Scenario | What happens in Riak
:--------|:--------------------
A vector clock is included with the write request, and is newer than the vclock attached to the existing object | The new value is written and success is indicated as soon as 2 vnodes acknowledge the write
A vector clock is included with the write request but conflicts with the vclock attached to the existing object, with `allow_mult` set to `true` | The new value is created as a sibling for future reads
A vector clock is included with the write request but conflicts with (or is older than) the vclock attached to the existing object, with `allow_mult` set to `false` | The new value overwrites the old
A vector clock is not included with the write request and an object already exists, with `allow_mult` set to `true` | The new value is created as a sibling for future reads
A vector clock is not included with the write request and an object already exists, with `allow_mult` set to `false` | The new value overwrites the existing value

## N, R and W in Action

Here is a brief screencast that shows just how the N, R, and W values function in our running three-node Riak cluster:

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/11172656"></div>

<p><a href="http://vimeo.com/11172656">Tuning CAP Controls in Riak</a> from <a href="http://vimeo.com/bashotech">Basho Technologies</a> on <a href="http://vimeo.com">Vimeo</a>.</p>