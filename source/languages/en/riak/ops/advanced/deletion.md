---
title: Object Deletion
project: riak
version: 2.0.0+
document: guide
audience: advanced
keywords: [operators, deletion, delete_mode, tombstones]
---

Most useful when deleting and then re-creating keys rapidly
Default is `keep`

The central problem: Riak needs to keep objects available during
failures by storing copies on multiple nodes; deletion is complex
because Riak must decide what to do when an object is deleted only on
some nodes and not others

Deletion process:

1. A tombstone objects (`<<>>`) is written to N nodes
2. If all N nodes store the tombstone, the object is removed
3. If fallback nodes are in use, the object will not be removed

The `delete_mode` setting provides control over how that process
functions; it dictates what happens in the time window between (a) the 
GET FSM deciding that an object can be removed and (b) that removal
taking place.

Settings:

* `keep` --- Disables tombstone removal; protects against an edge case in which an object is deleted and recreated on the owning nodes while a fallback is either down or awaiting handoff
* `immediate` --- The tombstone is removed as soon as the request is received. 
* interval --- How long to wait until the tombstone is removed

MDC => the problem is that deleted objects can be resurrected when
synchronizing between multiple DCs, especially when connectivity is an
issue; this problem can be avoided using `delete_mode`

Fetching the vclock for a deleted key => setting `deletedvclock` to
`true` via PBC [[PBC Fetch Object]]

```java
FetchValue fetch = new FetchValue.Builder(location)
		.withOption(Option.DELETED_VCLOCK, true)
		.build();
```

## Resources

http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-October/006048.html