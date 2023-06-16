---
title_supertext: "Using > TicTac AAE Fold:"
title: "Reap Tombstones"
description: ""
project: "riak_kv"
project_version: 3.0.12
menu:
  riak_kv-3.0.12:
    name: "Reap Tombs"
    identifier: "cluster_operations_tictac_aae_fold_reap_tombs"
    weight: 109
    parent: "cluster_operations_tictac_aae_fold"
toc: true
since: 2.9.4
version_history:
  in: "2.9.4+"
aliases:
---
[code riak_kv_vnode]: https://github.com/basho/riak_kv/blob/develop-3.0/src/riak_kv_vnode.erl
[riak attach]: ../../../admin/riak-cli/#attach
[config reference]: ../../../configuring/reference/#tictac-active-anti-entropy
[config tictacaae]: ../../../configuring/active-anti-entropy/tictac-aae
[tictacaae folds-overview]: ../
[tictacaae system]: ../../tictac-active-anti-entropy
[tictacaae client]: ../../tictac-aae-fold#the-riak-client
[tictacaae find-keys]: ../../tictac-aae-fold/find-keys
[tictacaae find-tombs]: ../../tictac-aae-fold/find-tombs
[tictacaae list-buckets]: ../../tictac-aae-fold/list-buckets
[tictacaae object-stats]: ../../tictac-aae-fold/object-stats
[tictacaae reap-tombs]: ../../tictac-aae-fold/reap-tombs
[filters]: ../../tictac-aae-fold/filters
[filter-by bucket]: ../../tictac-aae-fold/filters#filter-by-bucket-name
[filter-by key-range]: ../../tictac-aae-fold/filters#filter-by-key-range
[filter-by segment]: ../../tictac-aae-fold/filters#filter-by-segment
[filter-by modified]: ../../tictac-aae-fold/filters#filter-by-date-modified
[filter-by sibling-count]: ../../tictac-aae-fold/find-keys/#the-sibling-count-filter
[filter-by object-size]: ../../tictac-aae-fold/find-keys/#the-object-size-filter

Reaps the Riak tombstone objects that meet the filter parameters.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

Unreaped Riak tombstones are Riak objects that have been deleted, but have not been removed from the backend. Riak tracks this through tombstones. If automatic reaping is turned off (for example, by setting `delete_mode` = `keep`), then a large number of deleted objects can accumulate that Riak will never automatically remove. Manual dev ops intervention using this function is required.

Use the `reap_tombs` function to remove these objects.

## The `reap_tombs` function

Run this using [`riak attach`][riak attach].

This function has three available operational methods that are selected via the `method` value. The `local` method for reaping tombstones is detailed below. The general format for the function is:

```riakattach
riak_client:aae_fold({
    reap_tombs,
    bucket_filter,
    key_range_filter,
    segment_filter
    modified_filter,
    method
    }, Client).
```

Please see the list of [available filters](#available-filters) below.

{{% note title="Other `method`s" %}}
There are two other `method`s, `count` and `job`:

- `count` is used to count the tombstones (see [Count Tombstones](../../tictac-aae-fold/count-tombs) for more information).
- `job` is used internally by TicTac AAE. Do not use it unless you know what you are doing.
{{% /note %}}

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## The `local` method

Marks tombstones for reaping that meet the filter parameters. Returns the number of tombstones marked by calling this function.

```riakattach
riak_client:aae_fold({
    reap_tombs,
    bucket_filter,
    key_range_filter,
    segment_filter
    modified_filter,
    local
    }, Client).
```

Please see the list of [available filters](#available-filters) below.

For example, the following snippet will mark for reaping all tombstones with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in January 2022

```riakattach
riak_client:aae_fold({
    reap_tombs,
    {<<"animals">>,<<"dogs">>},
    {<<"A">>,<<"N">>},
    all,
    {date,{{2022,1,1},{0,0,0}},{{2022,2,1},{0,0,0}}},
    local
    }, Client).
```

## The response for the `local` method

The response will look something like this:

```erlang
{ok,5}
```

This indicates that 5 tombstones were found meeting the filter parameters and were marked to be reaped.

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys considered for reaping or counting.

These filters will reduce the keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]
- [`segment_filter`][filter-by segment]

These filters will reduce the number of keys considered for reaping or counting:

- [`modified_filter`][filter-by modified]
