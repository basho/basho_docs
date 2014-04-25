---
title: Latency Reduction Checklist
project: riak
version: 1.0.0+
document: guide
audience: intermediate
keywords: [operator, troubleshooting, latency]
---

Although _some_ latency is unavoidable in distributed systems like Riak, there
are a number of actions that can be undertaken to reduce latency to the lowest
levels possible within a cluster. In this guide, we'll list potential sources of
undue latency and what you can do about it.

## Sources of Latency

Excess latency in Riak most frequently stems from one of the following sources.

## Large Objects

Riak always performs best with smaller objects. We recommend keeping all objects
stored in Riak smaller than 1 MB, preferably below 100 KB. Large objects lead
to increased I/O activity and strain on memory resources.

Larger objects---even a few of them---can impact even requests that are
unrelated to those objects due to the nature of networking buffers in
distributed Erlang.

If your use case requires larger objects, we recommend checking out [[Riak CS]],
which is intended as a cloud object storage system.

#### Mitigation

The best way to find out if large objects are impacting latency is to monitor
each node's object size stats. If you run `[[riak-admin status|riak-admin Command Line#status]]`,
you will see the results for the following metrics related ot object size:

Metric | Explanation
:------|:-----------
`fsm_node_get_objsize_mean` | The mean object size encountered by this node in the last minute.
`fsm_node_get_objsize_median` | The median object size encountered by this node in the last minute.
`fsm_node_get_objsize_95` | The 95th-percentile object size encountered by this node in the last minute.
`fsm_node_get_objsize_99` | The 99th-percentile object size encountered by this node in the last minute.
`fsm_node_get_objsize_100` | The 100th-percentile object size encountered by this node in the last minute.

While the `mean` and `median` measurements may not be good indicators,
especially if you're storing billions of keys, but you should be on the lookout
for trends in the `95`, `99`, and `100` measures. Is there an upward trend? Do
the metrics indicate that there are outliers? Do these trends coincide with
increased latency?

If you suspect that large object size is impacting latency, try making the
following changes to each node's [[configuration|Configuration Files]]:

* If you are using the newer, `riak.conf`-based configuration system, the commented-out value for `erlang.distribution_buffer_size` is `32MB`. Uncomment this setting and re-start your node.
* If you are using the older, `app.config`/`vm.args`-based configuration system, try increasing the `+zddbl` setting in `vm.args` to `32768` or higher (measured in kilobytes). This increases the size of the distributed Erlang buffer from its default of 1024 KB. Re-start your node when configuration changes have been made.

Large objects can also impact latency even if they're only present on some
nodes. If increased latency occurs only on N nodes, where N is your replication
factor (or `n_val`), this could indicate that a single large object (and its
replicas) is slowing down _all_ requests on those nodes. If that is the case,
you should audit the behavior of siblings in your cluster, as explained in the
[[next section|Latency Reduction Checklist#siblings]].

## Siblings

In Riak, object conflicts are handled by keeping multiple versions of the object
in the cluster either until a client takes action to resolve the conflict or
until [[active anti-entropy|Riak Glossary#active-anti-entropy]] resolves the
conflict without client intervention. While sibling production is normal,
[[sibling explosion|Vector Clocks#sibling-explosion]] is a problem that can come
about if many siblings of an object are produced. The negative effects are the
same as those associated with [[large objects|Latency Reduction Checklist#large-objects]].

#### Mitigation

The best way to monitor siblings is through the same `[[riak-admin status|riak-admin Command Line#status]]`
interface used to monitor object size. In the output of `riak-admin status` in
each node, you'll see the following sibling-related statistics:

Metric | Explanation
:------|:-----------
`node_get_fsm_siblings_mean` | The mean number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_median` | The median number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_95` | The 95th percentile of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_99` | The 99th percentile of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_100` | The 100th percentile of siblings encountered during all GET operations by this node within the last minute

Is there an upward trend in these statistics over time? Are there any large
outliers? 

## Compaction and Merging

## OS Tuning

## IO/Network Bottlenecks

## Overload Protection
