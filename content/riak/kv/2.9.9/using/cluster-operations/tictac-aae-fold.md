---
title_supertext: "Using > Cluster Operations:"
title: "TicTac AAE Folds"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "TicTac AAE Folds"
    identifier: "cluster_operations_tictac_aae_fold"
    weight: 109
    parent: "managing_cluster_operations"
toc: true
since: 2.9.4
version_history:
  in: "2.9.4+"
aliases:
---
[code riak_kv_vnode]: https://github.com/basho/riak_kv/blob/develop-3.0/src/riak_kv_vnode.erl
[riak attach]: ../../admin/riak-cli/#attach
[config reference]: ../../../configuring/reference/#tictac-active-anti-entropy
[config tictacaae]: ../../../configuring/active-anti-entropy/tictac-aae
[tictacaae system]: ../tictac-active-anti-entropy
[tictacaae folds-overview]: ../tictac-aae-fold
[tictacaae client]: ../tictac-aae-fold#the-riak-client
[tictacaae find-keys]: ../tictac-aae-fold/find-keys
[tictacaae find-tombs]: ../tictac-aae-fold/find-tombs
[tictacaae list-buckets]: ../tictac-aae-fold/list-buckets
[tictacaae object-stats]: ../tictac-aae-fold/object-stats
[tictacaae count-tombs]: ../tictac-aae-fold/count-tombs
[tictacaae reap-tombs]: ../tictac-aae-fold/reap-tombs
[filters]: ../tictac-aae-fold/filters
[filter-by bucket]: ../tictac-aae-fold/filters#filter-by-bucket-name
[filter-by key-range]: ../tictac-aae-fold/filters#filter-by-key-range
[filter-by segment]: ../tictac-aae-fold/filters#filter-by-segment
[filter-by modified]: ../tictac-aae-fold/filters#filter-by-date-modified
[filter-by sibling-count]: ../tictac-aae-fold/find-keys/#the-sibling-count-filter
[filter-by object-size]: ../tictac-aae-fold/find-keys/#the-object-size-filter

Since Riak KV 2.9.1, the new AAE system, [TicTac AAE][tictacaae system], has added several useful functions that make performing keylisting and tombstone management tasks quicker and more efficient by using TicTacAAE's Merkle trees instead of iterating over the keys in a bucket.

These functions stabilised in Riak KV 2.9.4, and so are not recommended before that version.

## Configuration settings in `riak.conf`

For more TicTac AAE configuration settings, please see the [TicTac AAE configuration settings][config tictacaae] documentation.

### TicTacAAE

Turn on TicTacAAE. It works independantly of the legacy AAE system, so can be run in parallel or without the legacy system.

```riak.conf
tictacaae_active = active
```

Note that this will use up more memory and disk space as more metadata is being stored.

### Storeheads

Turn on TicTacAAE storeheads. This will ensure that TicTacAAE will store more information about each key, including the size, modified date, and tombstone status. Without setting this to `true`, the `aae_fold` functions on this page will not work as expected.

```riak.conf
tictacaae_storeheads = enabled
```

Note that this will use up more memory and disk space as more metadata is being stored.

## Tuning

You can increase the number of simultaneous workers by changing the `af4_worker_pool_size` value in `riak.conf`. The default is `1` per node.

```riak.conf
af4_worker_pool_size = 1
```

## General usage

Use [Riak attach][riak attach] to run these commands.

The general format for calling `aae_fold` is:

```riakattach
riak_client:aae_fold(
    query,
    Client).
```

`query` is a tuple describing the function to run and the parameters to use. The first value in the tuple is always the function name. For example, if calling the `list_buckets` function the tuple would look like `{list_buckets, ...}`. The number of values in the tuple depends on the function being called.

As an example, this will call `list_buckets`, which takes a single parameter:

```riakattach
riak_client:aae_fold({
    list_buckets,
    3
    }, Client).
```

### The Riak Client

For these calls to work, you will need a Riak client. This will create one in a reusable variable called `Client`:

```erlang
{ok, Client} = riak:local_client().
```

`Client` can now be used for the rest of the `riak attach` session.

## Troubleshooting - timeouts

The calls to `aae_fold` are synchronous calls with a 1 hour timeout, but they start an asynchronous process in the background.

If your command takes longer than 1 hour, then you will get `{error,timeout}` as a response after 1 hour. Note that the requested command continues to run in the background, so re-calling the same method will take up more resources.

To timeout you typically have to have a very large number of keys in the bucket.

### How to check if finished after a timeout

After experiencing a timeout, the current number of commands waiting to execute can be checked by asking for the size of the assured forwarding pool `af4_pool`. Once it reaches 0, there are no more workers as all commands have finished. The size of the pool can checked using this command:

```erlang
{_, _, _, [_, _, _, _, [_, _, {data, [{"StateData", {state, _, _, MM, _, _}}]}]]} =
    sys:get_status(af4_pool),
io:format("af4_pool has ~b workers\n", [length(MM)]),
f().
```
{{% note title="Warning: existing variables cleared" %}}
`f()` will unbind any existing variables, which may not be your intention. If you remove `f()` then please remember that `MM` will remain bound to the first value. For re-use, you should change the variable name or restart the `riak attach` session.
{{% /note %}}

### How to avoid timeouts

To reduce the chance of getting a timeout, reduce the number of keys checked by using the [bucket][filter-by bucket] and [key range][filter-by key-range] filters.

The [modified][filter-by modified] filter will not reduce the number of keys checked, and only acts as a filter on the result.

## Filters

Please see the [TicTac AAE Filters][filters] documentation.

These filters are used by several functions:

- Filter by bucket name - [Learn More >>][filter-by bucket]
  - Without a bucket type
  - With a bucket type
  - All
- Filter by key range - [Learn More >>][filter-by key-range]
  - From -> To
  - All
- Filter by segment - [Learn More >>][filter-by segment]
- Filter by modified date - [Learn More >>][filter-by modified]
  - From -> To
  - All

These filters can only be used with the `find_keys` function:

- Filter by sibling count - [Learn More >>][filter-by sibling-count]
- Filter by object size - [Learn More >>][filter-by object-size]

## Find keys

Function: `find_keys`

Returns a list of keys that meet the filter parameters.

[Learn More >>][tictacaae find-keys]

## Find Riak tombstones

Function: `find_tombs`

Returns tuples of bucket name, keyname, and object size of Riak tombstone objects that meet the filter parameters.

[Learn More >>][tictacaae find-tombs]

## List Buckets

Function: `list_buckets`

Returns a list of all buckets.

[Learn More >>][tictacaae list-buckets]

## Count tombstones

Function: `reap_tombs` with `count`

Counts the Riak tombstone objects that meet the filter parameters.

[Learn More >>][tictacaae count-tombs]

## Get object statistics

Function: `object_stats`

Returns a count of Riak objects that meet the filter parameters.

[Learn More >>][tictacaae object-stats]

## Reap tombstones

Function: `reap_tombs` with `local`

Reaps the Riak tombstone objects that meet the filter parameters.

[Learn More >>][tictacaae reap-tombs]

## Other functions not covered

`aae_fold` has various other functions that can be called, but are mostly for internal use by Riak. These functions should not be used without a good understanding of the source code, but are provided here for reference:

- `erase_keys`
- `fetch_clocks_nval`
- `fetch_clocks_range`
- `merge_branch_nval`
- `merge_root_nval`
- `merge_tree_range`
- `repair_keys_range`
- `repl_keys_range`
