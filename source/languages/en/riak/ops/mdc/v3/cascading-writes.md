---
title: "Multi Data Center Replication: Cascading Realtime Writes"
project: riak
header: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, replication, realtime, cascading]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-Cascading-Writes'
}
---

## Introduction

Riak Enterprise includes a feature that cascades realtime writes across multiple clusters.

Cascading Realtime Writes is enabled by default on new clusters running Riak Enterprise. On existing clusters, it will need to be manually enabled.

Cascading realtime requires the `{riak_repl, rtq_meta}` capability to function.

<div class="note">Cascading tracking is a simple list of where an object has been written. This works well for most common configurations. Larger installations, however, may have writes cascade to clusters to which other clusters have already written.
</div>


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

In the diagram above, a write at cluster _A_ will begin two cascades. One goes to _B_, _C_, _D_, _E_, and finally _F_; and the other goes to _F_, _E_, _D_, _C_, and finally B. Each cascade will loop around to _A_ again, sending a replication request even if the same request has already occurred from the opposite direction, creating 3 extra write requests.

This can be mitigated by disabling cascading in a cluster. If cascading were disabled on cluster _D_, a write at _A_ would begin two cascades. One would go through _B_, _C_, and _D_, the other through _F_, _E_, and _D_. This reduces the number of extraneous write requests to 1.

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

A write at _A_ will cascade to _C_ and _B_. _B_ will not cascade to _C_ because _A_ will have already added _C_ to the list of clusters where the write has occurred. _C_ will then cascade to _D_. _D_ then cascades to _E_ and _F_. _E_ and _F_ see that the other was sent a write request (by _D_), and so they do not cascade.

## Usage

Riak Enterprise Cascading Writes can be enabled and disabled using the `riak-repl` command. Please see the [[Version 3 Operations guide|Multi Data Center Replication v3 Operations]] for more information.

To show current the settings:

`riak-repl realtime cascades`

To enable cascading:

`riak-repl realtime cascades always`

To disable cascading:

`riak-repl realtime cascades never`
