---
title_supertext: "V3 Multi-Datacenter Replication Reference:"
title: "Cascading Realtime Writes"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Cascading Writes"
    identifier: "managing_ref_v3_cascading_writes"
    weight: 102
    parent: "managing_ref_v3"
toc: true
commercial_offering: true
aliases:
  - /riak/2.0.1/ops/mdc/v3/cascading-writes
  - /riak/kv/2.0.1/ops/mdc/v3/cascading-writes
---

## Introduction

Riak Enterprise includes a feature that cascades realtime writes across
multiple clusters.

Cascading Realtime Writes is enabled by default on new clusters running
Riak Enterprise. It will need to be manually enabled on new clusters.

Cascading realtime requires the `{riak_repl, rtq_meta}` capability to
function.

{{% note title="Note on cascading tracking" %}}
Cascading tracking is a simple list of where an object has been written. This
works well for most common configurations. Larger installations, however, may
have writes cascade to clusters to which other clusters have already written.
{{% /note %}}


```
+---+     +---+     +---+
| A | <-> | B | <-> | C |
+---+     +---+     +---+
  ^                   ^
  |                   |
  V                   V
+---+     +---+     +---+
| F | <-> | E | <-> | D |
+---+     +---+     +---+
```

In the diagram above, a write at cluster A will begin two cascades. One
goes to B, C, D, E, and finally F; the other goes to F, E, D, C, and
finally B. Each cascade will loop around to A again, sending a
replication request even if the same request has already occurred from
the opposite direction, creating 3 extra write requests.

This can be mitigated by disabling cascading in a cluster. If cascading
were disabled on cluster D, a write at A would begin two cascades. One
would go through B, C, and D, the other through F, E, and D. This
reduces the number of extraneous write requests to 1.

A different topology can also prevent extra write requests:

```
+---+                     +---+
| A |                     | E |
+---+                     +---+
 ^  ^                     ^  ^
 |   \  +---+     +---+  /   |
 |    > | C | <-> | D | <    |
 |   /  +---+     +---+  \   |
 V  V                     V  V
+---+                     +---+
| B |                     | F |
+---+                     +---+
```

A write at A will cascade to C and B. B will not cascade to C because
A will have already added C to the list of clusters where the write has
occurred. C will then cascade to D. D then cascades to E and F. E and F
see that the other was sent a write request (by D), and so they do not
cascade.

## Usage

Riak Enterprise Cascading Writes can be enabled and disabled using the
`riak-repl` command. Please see the [Version 3 Operations guide]({{<baseurl>}}riak/kv/2.0.1/using/cluster-operations/v3-multi-datacenter) for more information.

To show current the settings:

`riak-repl realtime cascades`

To enable cascading:

`riak-repl realtime cascades always`

To disable cascading:

`riak-repl realtime cascades never`
