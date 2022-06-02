---
title: "Latency Reduction Checklist"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Latency Reduction"
    identifier: "performance_latency_reduction"
    weight: 104
    parent: "managing_performance"
toc: true
aliases:
  - /riak/2.1.3/ops/tuning/latency-reduction
  - /riak/kv/2.1.3/ops/tuning/latency-reduction
---

Although latency is unavoidable in distributed systems like Riak, there
are a number of actions that can be undertaken to reduce latency
to the lowest levels possible within a cluster. In this guide, we'll
list potential sources of high latency and what you can do about it.

## Large Objects

Riak always performs best with smaller objects. Large objects, which can
be mistakenly inserted into Riak by your application or caused by
siblings (see below), can often increase latency.

We recommend keeping all objects stored in Riak smaller than 1-2 MB,
preferably below 100 KB. Large objects lead to increased I/O activity
and can put strain on memory resources. In some cases, just a few large
objects can impact latency in a cluster, even for requests that are
unrelated to those objects.

If your use case requires large objects, we recommend checking out
[Riak CS]({{<baseurl>}}riak/cs/latest/), which is intended as a storage system for large objects.

### Mitigation

The best way to find out if large objects are impacting latency is to
monitor each node's object size stats. If you run [`riak-admin status`](../../admin/riak-admin/#status) or make an HTTP `GET` request
to Riak's `/stats` endpoint, you will see the results for the following
metrics related to object size, all of which are calculated only for
`GET` operations (i.e. reads):

Metric                        | Explanation
:-----------------------------|:-----------
`fsm_node_get_objsize_mean`   | The mean object size encountered by this node in the last minute
`fsm_node_get_objsize_median` | The median object size encountered by this node in the last minute
`fsm_node_get_objsize_95`     | The 95th-percentile object size encountered by this node in the last minute
`fsm_node_get_objsize_99`     | The 99th-percentile object size encountered by this node in the last minute
`fsm_node_get_objsize_100`    | The 100th-percentile object size encountered by this node in the last minute

The `mean` and `median` measurements may not be good indicators,
especially if you're storing billions of keys. Instead, you should be on
the lookout for trends in the `95`, `99`, and `100` measures:

* Is there an upward trend?
* Do the metrics indicate that there are outliers?
* Do these trends coincide with increased latency?

If you suspect that large object size is impacting latency, try making
the following changes to each node's [configuration](../../../configuring/reference):

* If you are using the newer, `riak.conf`-based configuration system,
the commented-out value for `erlang.distribution_buffer_size` is `32MB`.
Uncomment this setting and re-start your node.
* If you are using the older, `app.config`/`vm.args`-based configuration
system, try increasing the `+zddbl` setting in `vm.args` to `32768` or
higher (measured in kilobytes). This increases the size of the
distributed Erlang buffer from its default of 1024 KB. Re-start your
node when configuration changes have been made.

Large objects can also impact latency even if they're only present on
some nodes. If increased latency occurs only on N nodes, where N is your
[replication factor](../../../developing/app-guide/replication-properties/#n-value-and-replication), also known as `n_val`, this could indicate that a single large object and its replicas are slowing down _all_ requests on those nodes.

If large objects are suspected, you should also audit the behavior of
siblings in your cluster, as explained in the [next section](#siblings).

## Siblings

In Riak, object conflicts are handled by keeping multiple versions of
the object in the cluster either until a client takes action to resolve
the conflict or until [active anti-entropy](../../../learn/glossary/#active-anti-entropy) resolves the conflict without client intervention. While sibling production is normal, [sibling explosion](../../../learn/concepts/causal-context/#sibling-explosion) is a problem that can come about if many siblings of an object are produced. The negative effects are the same as those associated with [large objects](#large-objects).

### Mitigation

The best way to monitor siblings is through the same [`riak-admin status`](../../admin/riak-admin/#status) interface used to monitor
object size (or via an HTTP `GET` request to `/stats`). In the output of
`riak-admin status` in each node, you'll see the following
sibling-related statistics:

Metric                         | Explanation
:------------------------------|:-----------
`node_get_fsm_siblings_mean`   | The mean number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_median` | The median number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_95`     | The 95th percentile of the number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_99`     | The 99th percentile of the number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_100`    | The 100th percentile of the number of siblings encountered during all GET operations by this node within the last minute

Is there an upward trend in these statistics over time? Are there any
large outliers? Do these trends correspond to your observed latency
spikes?

If you believe that sibling creation problems could be responsible for
latency issues in your cluster, you can start by checking the following:

* If `allow_mult` is set to `true` for some or all of your buckets, be
  sure that your application is correctly resolving siblings. Be sure to
  read our documentation on [conflict resolution](../../../developing/usage/conflict-resolution) for a fuller picture of how this can be done. **Note**: In Riak versions 2.0 and later, `allow_mult` is set to `true` by default for all bucket types that you create and activate.
  If you wish to set `allow_mult` to `false` on a bucket type, you will have to do so explicitly.
* Application errors are a common source of problems with
  siblings. Updating the same key over and over without passing a
  [causal context](../../../learn/concepts/causal-context) to Riak can cause sibling explosion. If this seems to be the issue, modify your application's [conflict resolution](../../../developing/usage/conflict-resolution)
  strategy. Another possibility worth exploring is using [dotted version vectors](../../../learn/concepts/causal-context/#dotted-version-vectors) \(DVVs) in place of traditional vector clocks. DVVs can be enabled [using bucket types](../../../developing/usage/bucket-types) by setting the `dvv_enabled` parameter to `true` for buckets that seem to be experiencing sibling explosion.

## Compaction and Merging

The [Bitcask](../../../setup/planning/backend/bitcask) and [LevelDB](../../../setup/planning/backend/leveldb) storage backends occasionally go through
heavily I/O-intensive compaction phases during which they remove deleted
data and reorganize data files on disk. During these phases, affected
nodes may be slower to respond to requests than other nodes. If your
cluster is using one or both of these backends, there are steps that can
be taken to monitor and address latency issues.

### Mitigation

To determine whether compaction and merging cycles align with increased
latency, keep an eye on on your `console.log` files (and LevelDB `LOG`
files if you're using LevelDB). Do Bitcask merging and/or LevelDB
compaction events overlap with increased latencies?

If so, our first recommendation is to examine your [replication properties](../../../developing/app-guide/replication-properties/) to make sure that neither R nor W are set to N, i.e. that you're not requiring that reads or writes go to all nodes in the cluster. The problem with setting `R=N` or `W=N` is that any request will only respond as quickly as the slowest node amongst the N nodes involved in the request.

Beyond checking for `R=N` or `W=N` for requests, the recommended
mitigation strategy depends on the backend:

#### Bitcask

With Bitcask, it's recommended that you:

* Limit merging to off-peak hours to decrease the effect of merging
cycles on node traffic
* Stagger merge windows between nodes so that no more than one node is
undergoing a merge phase at any given time

Instructions on how to accomplish both can be found in our guide to
[tuning Bitcask](../../../setup/planning/backend/bitcask/#tuning-bitcask).

It's also important that you adjust your maximum file size and merge
threshold settings appropriately. This setting is labeled
`bitcask.max_file_size` in the newer, `riak.conf`-based [configuration files](../../../configuring/reference) and `max_file_size` in the older, `app.config`-based system.

Setting the maximum file size lower will cause Bitcask to merge more
often (with less I/O churn), while setting it higher will induce less
frequent merges with more I/O churn. To find settings that are ideal for
your use case, we recommend checking out our guide to [configuring Bitcask](../../../setup/planning/backend/bitcask/#configuring-bitcask).

#### LevelDB

The more files you keep in memory, the faster LevelDB will perform in
general. To make sure that you are using your system resources
appropriately with LevelDB, check out our guide to [LevelDB parameter planning](../../../setup/planning/backend/leveldb/#parameter-planning).

## OS Tuning

While a number of latency-related problems can manifest themselves in
development and testing environments, some performance limits only
become clear in production environments.

### Mitigation

If you suspect that OS-level issues might be impacting latency, it might
be worthwhile to revisit your OS-specific configurations. The following
guides may be of help:

* [Open files limit](../open-files-limit)
* General [System performance tuning](../)
* [AWS performance tuning](../amazon-web-services) if you're running Riak on [Amazon Web Services](http://aws.amazon.com/)

## I/O and Network Bottlenecks

Riak is a heavily I/O- and network resource-intensive system.
Bottlenecks on either front can lead to undue latency in your cluster.
We recommend an active monitoring strategy to detect problems
immediately when they arise.

### Mitigation

To diagnose potential I/O bottlenecks, there are a number of Linux tools
at your disposal, including
[iowait](http://www.linuxquestions.org/questions/linux-newbie-8/what-is-iowait-415961/)
and [netstat](http://en.wikipedia.org/wiki/Netstat).

To diagnose potential overloads, Riak versions 1.3.2 and later come
equipped with an overload protection feature designed to prevent
cascading failures in overly busy nodes. This feature limits the number
of GET and PUT finite state machines (FSMs) that can exist
simultaneously on a single Riak node. Increased latency can result if a
node is frequently running up against these maximums.

* Monitor `node_get_fsm_active` and `node_get_fsm_active_60s` to get an
  idea of how many operations your nodes are coordinating. If you see
  non-zero values in `node_get_fsm_rejected` or
  `node_get_fsm_rejected_60s`, that means that some of your requests are
  being discarded due to overload protection.
* The FSM limits can be increased, but disabling overload protection
  entirely is not recommended. More details on these settings are
  available in the [release
  notes](https://github.com/basho/riak/blob/1.3/RELEASE-NOTES.md) for
  Riak version 1.3.

## Object Settings

In versions 2.0 and later, Riak enables you to configure a variety of
settings regarding Riak objects, including allowable object sizes, how
many [siblings](../../../learn/concepts/causal-context/#siblings) to allow, and so on. If you suspect that undue latency in your cluster stems from object size or related factors, you may consider adjusting these settings.

A concise listing of object-related settings can be found in the [Riak configuration](../../../configuring/reference/#object-settings) documentation. The sections below explain these settings in detail.

> **Note on configuration files in 2.0**
>
> The object settings listed below are only available using the new system
for [configuration files](../../../configuring/reference/) in Riak 2.0. If you are using the older, `app.config`-based system, you will not have access to
these settings.

### Object Size

As stated above, we recommend _always_ keeping objects below 1-2 MB
and preferably below 100 KB if possible. If you want to ensure that
objects above a certain size do not get stored in Riak, you can do so by
setting the `object.size.maximum` parameter lower than the default of
`50MB`, which is far above the ideal object size. If you set this
parameter to, say, `1MB` and attempt to store a 2 MB object, the write
will fail and an error message will be returned to the client.

You can also set an object size threshold past which a write will
succeed but will register a warning in the logs, you can adjust the
`object.size.warning_threshold` parameter. The default is `5MB`.

### Sibling Explosion Management

In order to prevent or cut down on [sibling explosion](../../../learn/concepts/causal-context/#sibling explosion), you can either prevent Riak from storing
additional siblings when a specified sibling count is reached or set a
warning threshold past which Riak logs an error (or both). This can be
done using the `object.siblings.maximum` and
`object.siblings.warning_threshold` settings. The default maximum is 100
and the default warning threshold is 25.

### Object Storage Format

There are currently two possible binary representations for objects
stored in Riak:

* Erlang's native `term_to_binary` format, which tends to have a higher
  space overhead
* A newer, Riak-specific format developed for more compact storage of
  smaller values

You can set the object storage format using the `object.format`
parameter: `0` selects Erlang's `term_to_binary` format while `1` (the
default) selects the Riak-specific format.
