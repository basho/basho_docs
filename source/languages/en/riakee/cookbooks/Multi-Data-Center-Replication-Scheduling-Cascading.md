---
title: "Multi Data Center Replication: Cascading Realtime Writes"
project: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, replication, realtime, cascading]
---

## Cascading Realtime Writes

Riak Enterprise 1.4 includes a feature that cascades realtime writes across multiple clusters.

Cascading Realtime Writes is enabled by default on new clusters running Riak Enterprise 1.4.0. On existing clusters, it will need to be manually enabled.

Cascading realtime requires the capability {riak_repl, rtq_meta} to function.

*Cascading tracking is a simple list of where an object has been written. This works well for most common configurations, however larger installations may have writes cascade to clusters that other clusters have already written to.*


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

In the diagram above, a write at cluster A will begin two cascades. One goes to B, C, D, E, and ending at F; and the other goes to F, E, D, C, and ending at B. Each cascade will loop around to A again, sending a replication request even if the same request has already occured from the opposite direction, creating 5 extra write requests.

This can be mitigated by disabling cascading at a cluster. If cascading were disabled on cluster D, a write at A would begin two cascades. One would go through B, C and D; the other through F, E, and D. This reduces the number of extraneous write requests to 1.

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

A write at A will cascade to C and B. B will not cascade to C as A will have already added C to the list of clusters the write has occured at. C will then cascade to D. D then cascades to E and F. E and F see the other was sent a write request (by D), and so they do not cascade.

### Usage

Riak Enterprise Cascading Writes can be enabled and disabled through the `riak-repl` command. Please see the TODO: Advanced Operations guide for more information.

To show current setting:

	* riak-repl realtime cascades

To enable cascading

	* riak-repl realtime cascades always

To disable cascading

	* riak-repl realtime cascades never
	
