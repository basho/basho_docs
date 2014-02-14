---
title: Cluster Metadata
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, advanced, cluster-metadata]
---

Cluster metadata is a subsystem inside of Riak (specifically [`riak_core`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl)) that enables applications to work with information that is stored cluster wide and can be read without blocking on communication over the network.

Cluster metadata is essentially information in key/value form that needs to be available through a Riak cluster. One example is [[bucket types]]. When you create a new bucket type and assign a set of bucket properties to that type, that information needs to be made available to all nodes. It cannot behave like other Riak data. This is why it needs to be built as a separate subsystem (though it is an important subsystem, on which a great many features relies).

Riak features like [[security|Riak Security]] and [[bucket types]] are examples of systems relying on cluster metadata.

## How Cluster Metadata Works

The cluster metadata storage system is a simple key/value store that is separate from normal data stored in Riak. Cluster metadata is ansynchronously replicated across all nodes in a cluster when it is stored or modified. Writes require acknowledgment from only a single node (`w=1`), while reads return values only from the local node (`r=1`). All updates are eventually consistent and propagated to all nodes, including nodes that join the cluster after the update has already reached all nodes in the previous set of members.

All cluster metadata is stored both in memory and on disk, but it should be noted that reads are only from memory, while writes are made both to memory and to disk. Logical clocks, namely [dotted version vectors](http://arxiv.org/abs/1011.5808), are used in place of vclocks or timestamps to resolve value conflicts. Values stored as cluster metadata are opaque Erlang terms addressed by a full prefix and a key:

```erlang
{atom() | binary(), atom() | binary()}
```

Keys can be _any_ Erlang term.

## API

The cluster metadata API is defined in the [`riak_core_metadata`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl) module. The API allows you to perform a variety of operations, including getting, putting, and deleting metadata and iterating through keys.

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