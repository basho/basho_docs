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

*Cascading tracking is a simple list of where an object has been written. This works well for most common configurations, however larger installations may have double writes.*


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

In the diagram above, a write at cluster A will go through clusters B, C, D, E and F as well as F, E, D, C and B. This can be mitigated by disabling cascading at a cluster. If cascading were disabled on cluster D, a write at cluster A would go through B, C, and D as well as F, E, and D. This means there is a single double-write opposed to the 4 without the break. A write at cluster E would go to D as well as F, A, B, C and D.

### Usage

Riak Enterprise Cascading Writes can be enabled and disabled through the `riak-repl` command. Please see the TODO: Advanced Operations guide for more information.

To show current setting:

	* riak-repl realtime cascades

To enable cascading

	* riak-repl realtime cascades always

To disable cascading

	* riak-repl realtime cascades never
	
