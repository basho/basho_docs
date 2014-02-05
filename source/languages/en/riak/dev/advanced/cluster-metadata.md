---
title: Cluster Metadata
project: riak
toc: true
---

Cluster metadata is a subsystem inside of Riak (specifically [`riak_core`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl)) that enables applications to work with information that is stored cluster wide. It is useful for storing information that needs to be read without blocking on communication over the network. A variety of Riak features, such as bucket types, were built using cluster metadata.

* KV store (no longer a flat namespace); eventually consistent; fully replicated; asynchronous replication
* Local reads and writes (`r=1`, `w=1`); in memory and on disk; logical clocks instead of timestamp-based clocks (dotted version vectors, *not* vclocks)
* You don't want to have to talk over the network to get certain values
* Epidemic broadcast trees (https://docs.di.fc.ul.pt/jspui/bitstream/10455/3002/1/07-14.pdf)
* Broadcast (not redundant, but also not reliable) vs. gossip (reliable but also redundant) vs. Plumtree (reliable and not very redundant)
* Self-healing spanning tree (gossip for healing the tree, tree as primary form of broadcast)

Cluster metadata functinos as is its own key/value store. Values are opaque Erlang terms addressed by a full prefix and a key:

```erlang
{atom() | binary(), atom() | binary()}
```

Keys can be any Erlang term. Values are stored on disk, with a full copy also stored in memory. Reads are only from memory, while writes are to both memory and disk. Updates to cluster metadata are eventually consistent. Writing a value only requires acknowledgment from one node, whereas reads return values from the local node only. Updates are replicated to every node in the cluster, including nodes that join the cluster after the update has already reached all nodes in the previous set of members.

## API

The cluster metadata API is defined by the [`riak_core_metadata`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl) module. The API allows you to perform a variety of operations, including getting, putting, and deleting metadata and iterating through keys.

Performing a put only blocks until the write is effected locally, whereas broadcasting the update takes place asynchronously.

Deletion of keys is logical and tombstones are not reaped. Delete operations function the same way as put operations in that deletes are blocking only until the operations is effected locally, while broadcasting deletes to other nodes takes place asynchronously.

The API's iterator functions enable you to iterate over either keys, values, or full prefixes. Iterators are neither ordered nor read-isolated. They do, however, guarantee that each key is seen _at most once_ for the lifetime of the iterator (thus no duplicate keys).

Conflict resolution can be done on read or write; `allow_put` passed to `get/3` or `iterator/2` controls whether or not the resolved value will be written back to local storage and broadcast asynchronously. On write, conflicts are resolved by passing a function instead of a new value to `put/3`  or `put/4`.

The following two tables list and describe available API operations.

### Get, Put, and Delete Operations

Function | Arguments | Description |
:--------|:----------|:------------|
`get/2` | `FullPrefix`<br />`Key` | Access the metadata value stored at the given prefix and key. |
`get/3` | `{Prefix, Subprefix}=FullPrefix`<br />`Key`<br />`Opts` | Access metadata value, passing in one of the following options:<ul><li>`default` --- The value to return if no value is found; `undefined` if not given.</li><li>`resolver` --- A function that resolves conflicts if they are encountered; if not given, last-write-wins is used (the atom `Lww`)</li><li>`allow_put` --- Whether to write and broadcast a resolved value; defaults to `true`</li></ul> |
`to_list` | `FullPrefix` | Returns a list of all keys and values stored under a given prefix/subprefix; available options are the same as those provided to `iterator/2` (below) |
`put/3` | `FullPrefix`<br />`Key`<br />`ValueOrFun` | Stores or updates the value at a given  |
`put/4` | `{Prefix, SubPrefix}=FullPrefix`<br />`Key`<br />`ValueOrFun`<br />`Opts`| |
`delete/2` | `FullPrefix`<br />`Key` | Acts the same way as `put/3` |
`delete/3` | `FullPrefix`<br />`Key`<br />`Opts` | Acts the same way was `put/4` |

### Iterative Operations

Function | Arguments | Description |
:--------|:----------|:------------|
`fold` | `Fun`<br />`Acc0`<br />`FullPrefix`<br />`Opts` | Folds over all keys and values stored under a given prefix/subprefix; available options are the same as those provided to `iterator/2` (below) |
`iterator/2` | `FullPrefix` | Returns an iterator pointing to the first key stored under a prefix. Possible values:<ul><li>`resolver` --- Either the atom `Lww` or a function that resolves conflicts if they are encountered (see `get/3` above for more details). Conflict resolution is performed when values are retrieved. If no resolver is provided, no resolution is performed. The default is not to provide a resolver.</li><li>`allow_put` --- Whether to write and broadcast a resolved value; defaults to `true`</li><li>`default` --- Used when the value an iterator points to is a tombstone. The default is an arity-1 function or a value. If a function, the key the iterator points to is passed as the argument and the result is returned in place of the tombstone. If default is a value, the value is returned in place of the tombstone. This applies when using functions such as `itr_values/1` and `itr_key_values/1`.</li><li>`match` --- A tuple containing Erlang terms and `_`s. `match` can be used to iterate over a subset of keys, assuming that the keys stored are tuples.</li></ul> |
`itr_next` | `{It, Opts}` | Advances the iterator |
`itr_close` | `{It, _Opts_}` | Closes the iterator |
`itr_done` | `{It, _Opts}` | Returns `true` if there is nothing more to iterate over. |
`itr_key` | `{It, Opts}` | 
`itr_key_values` | `{It, Opts}` | Returns the key and value(s) pointed at by the iterator. Can only be called if `itr_done` returns `false`. If a resolver was passed to `iterator/0` when creating the given iterator, siblings will be resolved using the given function or last-write-wins (if `Lww` is passed as the resolver). If no resolver was used then no conflict resolution will take place. If conflicts are resolved, the resolved value is written to local metadata and a broadcast is submitted to update other nodes in the cluster if `allow_put` is set to `true`. If `allow_put` is `false`, the values are resolved but are not written or broadcast. A single value is returned as the second element of the tuple in the case the values are resolved. If no resolution takes place then a list of values will be returned as the second element (even if there is only a single sibling). **Note**: If resolution may be performed, this function must be called at most once before calling `itr_next` on the iterator, at which point the function can be called once more. |

### Other Operations

Function | Arguments | Description |
:--------|:----------|:------------|
`prefix_hash` | `Prefix` | Returns the local hash associated with a full prefix or prefix. The hash value is updated periodically and does not always reflect the most recent value. This function can be used to determine when keys stored under a full prefix or prefix have changed. If the tree has not yet been updated or there are no keys stored, the given prefix or full prefix. `undefined` is returned. |

## Metadata Objects

Function | Arguments | Description |
:--------|:----------|:------------|
`value` | `Metadata` | Returns a single value. If the object holds more than one value, an error is generated. |
`values` | `{metadata, Object}` | Returns a list of values held in the object |
`value_count` | `{metadata, Object}` | Returns the number of siblings in the given object |
`context` | `{metadata, Object}` | Returns the context, i.e. opaque causal history, for the given object |
`empty_context` | none | Returns the representation for an empty context, i.e. opaque causal history |
`hash` | `{metadata, Object}` | Returns a hash representing the metadata object's contents |
`modify` | `undefined, Context, Fun, ServerId` or `Obj, Context, Fun, ServerId` or `undefined, _Context, Value, ServerId` (ignores context) or `{metadata, Existing}, Context, Value, ServerId` | Modifies a potentially existing object, settings its value and updating the causal history. If a function is provided as the third argument, this function is also used for conflict resolution. The difference between this function and `resolve/2` is that the logical clock is advanced in the case of this function. Additionally, the resolution functions are slightly different. |
`reconcile` | `undefined, _LocalObj` or `RemoteObj, undefined` or `{metadata, RemoteObj}, {metadata, LocalObj}` | Reconciles a remote object received during replication or anti-entropy with a local object. If the remote object is an ancestor of or is equal the local object, `false` is returned; otherwise, the reconciled object is returned as the second element of the two-tuple. |
`resolve` | `{metadata, Object}, lww` or `{metadata, Existing}, Reconcile` | Resolves siblings using either last-write-wins or the provided function and returns an object containing a single value. The causal history is not updated. |
`is_stale` | `_, undefined` or `RemoteContext, {metadata, Obj}` | Determines if the given context (version vector, *not* vclock) is causally newer than an existing object. If the object is missing or if the context does not represent an ancestor of the current key, `false` is returned. Otherwise, when the context does represent an ancestor of the existing object or the existing object itself, `true` is returned. |
`descends` | ? | ??? |
`equal_context` | `Context, {metadata, Obj}` | Returns `true` if the given context and the context of the existing object are equal. |

## Metadata Exchange

[`riak_core_metadata_exchange_fsm`](https://github.com/basho/riak_core/blob/9c2a4c8e67afab61c9d0de86a010c980c281a1c3/src/riak_core_metadata_exchange_fsm.erl)

Function | Arguments | Description |
:--------|:----------|:------------|
`start` | `Peer`<br />`Timeout` | Starts an exchange of cluster metadata hash trees between this node and the `Peer` node. `Timeout` is the number of milliseconds the process will wait to acquire the remote lock or to update both trees. |

## Metadata Hash Trees

[`riak_core_metadata_hashtree`](https://github.com/basho/riak_core/blob/9c2a4c8e67afab61c9d0de86a010c980c281a1c3/src/riak_core_metadata_hashtree.erl)

Function | Arguments | Description |
:--------|:----------|:------------|
`start_link/0` | **none** | Starts the process using `start_link/1` (directly below), passing in the directory where other cluster metadata is stored in `platform_data_dir` as the root |
`start_link/1` | `DataRoot` | Starts a registered process that manages a `hashtree_tree` for cluster metadata. 






## Internals

CMD stores data in a different way from that of clients; e.g. clients are not exposed to logical clocks alongside values (but they are used heavily within the cluster)

`riak_core_metadata_object` defines the internal representation, i.e. the **metadata object**; provides a set of operations that can be performed on metadata objects, e.g. conflict resolution, version reconciliation, accessing values, clock, add'l information about the object, etc.

Logical clocks used by CMD are [dotted version vectors](https://github.com/ricardobcl/Dotted-Version-Vectors); `dvvset.erl`

Nodes manage storage via the `riak_core_metadata_manager` process running on it; the metadata manager is not responsible for replication and is not aware of other nodes; it is, however, aware that writes are causally related by their logical clocks, and it uses this informatio to make local storage decisions, e.g. when an object is causally older than the value already stored; data read from the metadata manager is a metadata object or a raw value

Hash trees are the basis for determining differences betwen data stored on two nodes; used to determine when keys change; `hashtree_tree` module; each node maintains a `hashtree` via the `riak_core_metadata_hashtree` process; only maintained for the life of the running node; rebuilt each time a node starts and are never rebuilt for as long as the node continues to run; backed by LevelDB and cleared on graceful shutdown and each time the node is started; the entire tree is stored in one LevelDB instance

Broadcast: separate subsystem; ties nodes together; `riak_core_broadcast` module; the `riak_core_broadcast_handler` process

## Usage Examples

All operations are of the following form:

```erlang
(node_name@node_address)> riak_core_metadata:metadata_function(args).
```

For `metadata_function` substitute any of the functions listed in the table above.

These examples are for the node `dev1@127.0.0.1`. We'll use Eshell (the Erlange shell) for these examples, but you may of course wish to interact with cluster metadata programmatically in your application.

#### Gets

```erlang
(dev1@127.0.0.1)> riak_core_metadata:get(_, _).
```

```erlang
(dev1@127.0.0.1)> riak_core_metadata:get(_, _, _).
```

#### Puts

```erlang
(dev1@127.0.0.1)> riak_core_metadata:put(_, _, _).
```

```erlang
(dev1@127.0.0.1)> riak_core_metadata:put(_, _, _, _).
```

#### Deletes

```erlang
(dev1@127.0.0.1)> riak_core_metadata:delete(_, _).
```

```erlang
(dev1@127.0.0.1)> riak_core_metadata:delete(_, _, _).
```

#### Iteration

## "Controlled Epidemics" talk

http://www.youtube.com/watch?v=s4cCUTPU8GI

Lots of custom bucket properties make Riak *slow*

Gossip system: periodic and change-triggered communication; as ring structure changes, there is a flood of nodes (graph overlay); binary tree with cycles; very chatty

Bucket properties problem; modify one bucket property and all of the others are broadcast around (cold rumor spreading)

https://gist.github.com/jrwest/a0d88571d5d7787758ce

https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl
https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata_exchange_fsm.erl
https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata_hashtree.erl
https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata_manager.erl
https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata_object.erl

![Riak cluster metadata diagram](https://a248.e.akamai.net/camo.github.com/2bd50766570280a928bf74aafeda4d45018b1c81/687474703a2f2f6563322d35342d3232372d3131372d39392e636f6d707574652d312e616d617a6f6e6177732e636f6d2f636f72652d6b762d6d65657475702d707265736f2f696d616765732f686967682d6c6576656c2e706e67)











