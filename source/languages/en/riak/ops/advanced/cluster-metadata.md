---
title: Cluster Metadata
project: riak
toc: true
---

https://gist.github.com/jrwest/a0d88571d5d7787758ce

Subsystem inside of Riak
Used by `riak_core` applications wishing to work with information stored cluster wide. It is useful for storing application metadata or any information that needs to be read without blocking on communication over the network.

CMD is a key/value store
Values are opaque Erlang terms addressed by a "Full Prefix" and "Key", e.g.:

```erlang
{atom() | binary(), atom() | binary()}
```

Key is any Erlang term

Values are stored on disk; full copy also in memory; reads only from memory, writes in both media

Updates are eventually consistent; writing requires acknowledgment from a single node, reads return values from the local node only

Updates are replicated to every node in the cluster, including nodes that join the cluster after the update has already reached all nodes in the previous set of members

## API

`riak_core_metadata` module; official docs are housed there

Access local value with `get/2` (uses default) and `get/3` (for passing in options)

Updating is done using `put/3` or `put/4`

Deleting: `delete/2`, `delete/3`; both act the same as `put/3` and `put/4` with respect to blocking and broadcast

Full prefixes can be iterated over; they are not ordered or read isolated, but they do guarantee that each key is seen _at most once_ for the lifetime of an iterator: `iterator/2`, `itr_*`

Conflict resolution can be done on read or write; `allow_put` passed to `get/3` or `iterator/2` controls whether or not the resolved value will be written back to local storage and broadcast asynchronously; on write, conflicts are resolved by passing a function instead of a new value to `put/3,4`

The `prefix_hash` function can be polled to determine when groups of keys, by prefix or full prefix, have changed

## Internals

CMD stores data in a different way from that of clients; e.g. clients are not exposed to logical clocks alongside values (but they are used heavily within the cluster)

`riak_core_metadata_object` defines the internal representation, i.e. the **metadata object**; provides a set of operations that can be performed on metadata objects, e.g. conflict resolution, version reconciliation, accessing values, clock, add'l information about the object, etc.

Logical clocks used by CMD are [dotted version vectors](https://github.com/ricardobcl/Dotted-Version-Vectors); `dvvset.erl`

Nodes manage storage via the `riak_core_metadata_manager` process running on it; the metadata manager is not responsible for replication and is not aware of other nodes; it is, however, aware that writes are causally related by their logical clocks, and it uses this informatio to make local storage decisions, e.g. when an object is causally older than the value already stored; data read from the metadata manager is a metadata object or a raw value

Hash trees are the basis for determining differences betwen data stored on two nodes; used to determine when keys change; `hashtree_tree` module; each node maintains a `hashtree` via the `riak_core_metadata_hashtree` process; only maintained for the life of the running node; rebuilt each time a node starts and are never rebuilt for as long as the node continues to run; backed by LevelDB and cleared on graceful shutdown and each time the node is started; the entire tree is stored in one LevelDB instance

Broadcast: separate subsystem; ties nodes together; `riak_core_broadcast` module; the `riak_core_broadcast_handler` process