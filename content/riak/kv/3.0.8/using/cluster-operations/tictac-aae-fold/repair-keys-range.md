---
title_supertext: "Using > TicTac AAE Fold:"
title: "Repair Keys"
description: ""
project: "riak_kv"
project_version: 3.0.8
menu:
  riak_kv-3.0.8:
    name: "Repair Keys"
    identifier: "cluster_operations_tictac_aae_fold_repair_keys_range"
    weight: 108
    parent: "cluster_operations_tictac_aae_fold"
toc: true
since: 3.0.8
version_history:
  in: "3.0.8+"
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

Performs a read-repair on the Riak objects that meet the filter parameters.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

Occasionally, you want to perform a read-repair on a number of keys quickly an efficiently. Previously, you had to do a [`find_keys`][tictacaae find-keys] call followed by a read request on each key. This was inefficient as all the data had to be sent over the network to the client. Now, you can have Riak perform a read-repair without sending the data to the client.

Use the `repair_keys_range` function to remove these objects.

## The `repair_keys_range` function

Run this using [`riak attach`][riak attach].

The format for the function is:

```erlang
riak_client:aae_fold({
    repair_keys_range, 
    bucket_filter, 
    key_range_filter, 
    modified_filter,
    all
    }, Client).
```
Please see the list of [available filters](#available-filters) below.

For example, the following snippet will perform a read-repair on all Riak objects with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in January 2022

```erlang
riak_client:aae_fold({
    repair_keys_range, 
    {<<"animals">>,<<"dogs">>}, 
    {<<"A">>,<<"N">>},
    {date,{{2022,1,1},{0,0,0}},{{2022,2,1},{0,0,0}}},
    all
    }, Client).
```

## The response

The response will look something like this:

```erlang
{ok}
```

This indicates that the keys found meeting the filter parameters and were read-repaired.

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys considered for reaping or counting.

These filters will reduce the keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]

These filters will reduce the number of keys considered for reaping or counting:

- [`modified_filter`][filter-by modified]


