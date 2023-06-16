---
title_supertext: "Using > TicTac AAE Fold:"
title: "Count Keys"
description: ""
project: "riak_kv"
project_version: 3.0.13
menu:
  riak_kv-3.0.13:
    name: "Count Keys"
    identifier: "cluster_operations_tictac_aae_fold_count_keys"
    weight: 105
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
[tictacaae count-tombs]: ../../tictac-aae-fold/count-tombs
[filters]: ../../tictac-aae-fold/filters
[filter-by bucket]: ../../tictac-aae-fold/filters#filter-by-bucket-name
[filter-by key-range]: ../../tictac-aae-fold/filters#filter-by-key-range
[filter-by segment]: ../../tictac-aae-fold/filters#filter-by-segment
[filter-by modified]: ../../tictac-aae-fold/filters#filter-by-date-modified
[filter-by sibling-count]: ../../tictac-aae-fold/find-keys/#the-sibling-count-filter
[filter-by object-size]: ../../tictac-aae-fold/find-keys/#the-object-size-filter

Counts keys that meet the filter parameters.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

This function allows you to count how many keys would be deleted in a single pass based on the supplied filters. It does not actually delete the keys. Manual dev ops intervention using this function is required.

Use the `erase_keys` function to count the keys that would be deleted.

If you need to find keys with more specific properties, you can also use the [Object Statistics function][tictacaae object-stats].

## The `erase_keys` function

Run this using [`riak attach`][riak attach].

This function has three available operational methods that are selected via the `method` value. The `count` method for counting keys is detailed below. The general format for the function is:

```riakattach
riak_client:aae_fold({
    erase_keys,
    bucket_filter,
    key_range_filter,
    segment_filter
    modified_filter,
    method
    }, Client).
```

Please see the list of [available filters](#available-filters) below.

{{% note title="Other `method`s" %}}
There are two other `method`s, `local` and `job`:

- `local` is used to delete the keys (see [Erase Keys](../../tictac-aae-fold/erase-keys) for more information).
- `job` is used internally by TicTac AAE. Do not use it unless you know what you are doing.
{{% /note %}}

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## The `count` method

Returns a count of keys that meet the filter parameters. Does NOT delete the keys.

```riakattach
riak_client:aae_fold({
    erase_keys,
    bucket_filter,
    key_range_filter,
    segment_filter
    modified_filter,
    count
    }, Client).
```

Please see the list of [available filters](#available-filters) below.

For example, the following snippet will count all keys that would be deleted with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in January 2022

```riakattach
riak_client:aae_fold({
    erase_keys,
    {<<"animals">>,<<"dogs">>},
    {<<"A">>,<<"N">>},
    all,
    {date,{{2022,1,1},{0,0,0}},{{2022,2,1},{0,0,0}}},
    count
    }, Client).
```

## The response for the `count` method

The response will look something like this:

```erlang
{ok,5}
```

This indicates that 5 keys were found meeting the filter parameters.

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys considered for counting.

These filters will reduce the number of keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]
- [`segment_filter`][filter-by segment]

These filters will reduce the number of keys considered for counting:

- [`modified_filter`][filter-by modified]
