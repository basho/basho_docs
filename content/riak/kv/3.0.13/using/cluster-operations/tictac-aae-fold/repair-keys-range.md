---
title_supertext: "Using > TicTac AAE Fold:"
title: "Repair Keys"
description: ""
project: "riak_kv"
project_version: 3.0.13
menu:
  riak_kv-3.0.13:
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

```riakattach
riak_client:aae_fold({
    repair_keys_range,
    bucket_filter,
    key_range_filter,
    modified_filter,
    all
    }, Client).
```
Please see the list of [available filters](#available-filters) below.

{{% note %}}
For the function `repair_keys_range`, only non-negative interger of seconds since `1970-01-01 00:00:00` works for `modified_filter` in this version. This is fixed in a later version.
{{% /note %}}

For example, the following snippet will perform a read-repair on all Riak objects with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in 2022

```riakattach
riak_client:aae_fold({
    repair_keys_range,
    {<<"animals">>,<<"dogs">>},
    {<<"A">>,<<"N">>},
    {date,1640995200,1672531200},
    all
    }, Client).
```

## The response

The response will look something like this:

```erlang
{ok,{[],0,all,128}}
```

This indicates that:

- `ok`: the read repair request finished successfully
- `[]`: the remaining items, which should be an empty list
- `0`: the number of keys repairs, in this case none
- `all`: a constant
- `128`: the size of each batch of read repairs, which is 128

This indicates that the keys found meeting the filter parameters and were read-repaired.

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys considered for reaping or counting.

These filters will reduce the keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]

These filters will reduce the number of keys considered for reaping or counting:

- [`modified_filter`][filter-by modified]

