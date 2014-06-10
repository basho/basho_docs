---
title: Latency Reduction Checklist
project: riak
version: 1.0.0+
document: guide
audience: intermediate
keywords: [operator, troubleshooting, latency]
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
[[Riak CS]], which is intended as a storage system for large objects.

### Mitigation

The best way to find out if large objects are impacting latency is to
monitor each node's object size stats. If you run `[[riak-admin status|riak-admin Command Line#status]]`
or make an HTTP `GET` request to Riak's `/stats` endpoint, you will see
the results for the following metrics related to object size:

Metric                        | Explanation
:-----------------------------|:-----------
`fsm_node_get_objsize_mean`   | The mean object size encountered by this node in the last minute.
`fsm_node_get_objsize_median` | The median object size encountered by this node in the last minute.
`fsm_node_get_objsize_95`     | The 95th-percentile object size encountered by this node in the last minute.
`fsm_node_get_objsize_99`     | The 99th-percentile object size encountered by this node in the last minute.
`fsm_node_get_objsize_100`    | The 100th-percentile object size encountered by this node in the last minute.

The `mean` and `median` measurements may not be good indicators,
especially if you're storing billions of keys. Instead, you should be on
the lookout for trends in the `95`, `99`, and `100` measures:

* Is there an upward trend?
* Do the metrics indicate that there are outliers?
* Do these trends coincide with increased latency?

If you suspect that large object size is impacting latency, try making
the following changes to each node's [[configuration|Configuration
Files]]:

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
replication factor (or `n_val`), this could indicate that a single large
object and its replicas are slowing down _all_ requests on those nodes.

{{#1.4.8+}}
Riak will log large objects and their keys to the `console.log` file.
You can use this data to track down these objects and delete them,
reduce their size, or resolve their siblings (if present).
{{/1.4.8+}}

If large objects are suspected, you should also audit the behavior of
siblings in your cluster, as explained in the [[next section|Latency
Reduction Checklist#siblings]].

## Siblings

In Riak, object conflicts are handled by keeping multiple versions of
the object in the cluster either until a client takes action to resolve
the conflict or until [[active anti-entropy|Riak Glossary#active-anti-entropy]]
resolves the conflict without client intervention. While sibling
production is normal, [[sibling explosion|Vector Clocks#sibling-explosion]]
is a problem that can come about if many siblings of an object are
produced. The negative effects are the same as those associated with
[[large objects|Latency Reduction Checklist#large-objects]].

### Mitigation

The best way to monitor siblings is through the same `[[riak-admin status|riak-admin Command Line#status]]`
interface used to monitor object size (or via an HTTP `GET` request to
`/stats`). In the output of `riak-admin status` in each node, you'll see
the following sibling-related statistics:

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

* If `allow_mult` is set to `true` for some or all of your buckets, be sure that your application is correctly resolving siblings. Be sure to read our documentation on [[conflict resolution]] for a fuller picture of how this can be done.
* Application errors are a common source of problems with siblings. Updating the same key over and over without passing a [[vector clock|Vector Clocks]] to Riak can cause sibling explosion. If this seems to be the issue, modify your application's conflict resolution strategy. {{2.0.0-}}
* Application errors are a common source of problems with siblings. Updating the same key over and over without passing a [[vector clock|Vector Clocks]] to Riak can cause sibling explosion. If this seems to be the issue, modify your application's conflict resolution strategy.

## Compaction and Merging

The [[Bitcask]] and [[LevelDB]] storage backends occasionally go through
heavily I/O-intensive compaction phases during which they remove deleted
data and reorganize data files on disk. During these phases, affected
nodes may be slower to respond to requests than other nodes.

### Mitigation

To determine whether compaction and merging cycles align with, keep an
eye on on your `console.log` files (and LevelDB `LOG` files if you're
using LevelDB). Do Bitcask merging or LevelDB compaction events overlap
with increased latencies?

If so, our first recommendation is to examine your [[replication strategy|Replication Properties]]
to make sure that neither R nor W are set to N, i.e. that you're not
requiring that reads or writes go to all nodes in the cluster. The
problem with setting `R=N` or `W=N` is that any request will only
respond as quickly as the slowest node in the cluster.

Beyond checking for `R=N` or `W=N` for requests, the recommended
mitigation strategy depends on the backend:

#### Bitcask

With Bitcask, it's recommended that you:

* Limit merging to off-peak hours to decrease the effect of merging
cycles on node traffic
* Stagger merge windows between nodes so that no more than one node is
undergoing a merge phase at any given time

Instructions on how to accomplish both can be found in our guide to
[[tuning Bitcask|Bitcask#tuning-bitcask]].

It's also important that you adjust your maximum file size and merge
threshold settings appropriately. This setting is labeled
`max_file_size` in the older, `app.config`-based [[configuration
system|Configuration Files]] and `bitcask.max_file_size` in the newer,
`riak.conf`-based system.

Setting the maximum file size lower will cause Bitcask to merge more
often (with less I/O churn), while setting it higher will induce less
frequent merges with more I/O churn. To find settings that are ideal for
your use case, we recommend checking out our guide to [[configuring
Bitcask|Bitcask#configuring-bitcask]].

#### LevelDB

The more files you keep in memory, the better LevelDB will perform in
general.  To make sure that you are using your system resources
appropriately with LevelDB, check out our guide to [[LevelDB parameter
planning|LevelDB#parameter-planning]].

## OS Tuning

While a number of latency-related problems can manifest themselves in
development and testing environments, some performance limits only
become clear in production environments.

### Mitigation

If you suspect that OS-level issues might be impacting latency, it might
be worthwhile to revisit your OS-specific configurations. The following
guides may be of help:

* [[Open files limit]]
* [[File system tuning]]
* General [[Linux system tuning]]
* [[AWS performance tuning]] if you're running Riak on [Amazon Web Services](http://aws.amazon.com/)

## I/O and Network Bottlenecks

There are a number of Linux tools that you can use to diagnose I/O
bottlenecks, including [iowait](http://www.linuxquestions.org/questions/linux-newbie-8/what-is-iowait-415961/)
and [netstat](http://en.wikipedia.org/wiki/Netstat).

### Mitigation

## Overload Protection

Versions of Riak 1.3.2 and later come equipped with an overload
protection feature intended to prevent cascading failures in overly busy
nodes. This feature limits the number of GET and PUT finite state
machines (FSMs) that can exist simultaneously on a single Riak node.
Increased latency can result if a node is frequently running up against
these maximums.

### Mitigation

* Monitor `node_get_fsm_active` and `node_get_fsm_active_60s` to get an idea of how many operations your nodes are coordinating.  If you see non-zero values in `node_get_fsm_rejected` or `node_get_fsm_rejected_60s`, that means that some of your requests are being discarded due to overload protection.  
* The fsm limits can be increased, but disabling overload protection entirely is not recommended. More details on these settings are available in the [release notes](https://github.com/basho/riak/blob/1.3/RELEASE-NOTES.md) for Riak version 1.3.
