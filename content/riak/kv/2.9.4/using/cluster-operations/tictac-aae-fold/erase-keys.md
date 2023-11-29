---
title_supertext: "Using > TicTac AAE Fold:"
title: "Erase Keys"
description: ""
project: "riak_kv"
project_version: "2.9.4"
lastmod: 2020-07-03T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-2.9.4:
    name: "Erase Keys"
    identifier: "cluster_operations_tictac_aae_fold_erase_keys"
    weight: 108
    parent: "cluster_operations_tictac_aae_fold"
toc: true
since: 
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

Erases keys that meet the filter parameters.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

This function allows you to delete many keys in a single pass based on the supplied filters. The Riak keys will be converted to Riak tombstones after which the normal Riak reaping functions take over. Manual dev ops intervention using this function is required.

Use the `erase_keys` function to delete these keys.

## The `erase_keys` function

Run this using [`riak attach`][riak attach].

This function has three available operational methods that are selected via the `method` value. The `local` method for deleting keys is detailed below. The general format for the function is:

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
There are two other `method`s, `count` and `job`:

- `count` is used to count the keys that would have been deleted (see [Count Objects](../../tictac-aae-fold/count-keys) for more information).
- `job` is used internally by TicTac AAE. Do not use it unless you know what you are doing.
{{% /note %}}

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## The `local` method

Deletes keys that meet the filter parameters so that they can then be reaped. Returns the number of keys deleted by calling this function.

```riakattach
riak_client:aae_fold({
    erase_keys,
    bucket_filter,
    key_range_filter,
    segment_filter
    modified_filter,
    local
    }, Client).
```

Please see the list of [available filters](#available-filters) below.

For example, the following snippet will delete keys with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in January 2022

```riakattach
riak_client:aae_fold({
    erase_keys,
    {<<"animals">>,<<"dogs">>},
    {<<"A">>,<<"N">>},
    all,
    {date,1640995200,1643673600},
    local
    }, Client).
```

## The response for the `local` method

The response will look something like this:

```erlang
{ok,5}
```

This indicates that 5 keys were found meeting the filter parameters and were deleted by Riak. Remember that a deleted Riak key is really converted to a Riak tombstone object, and will be actually removed from the backend at a later point based on your `delete_mode` setting.

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys considered for deleting.

These filters will reduce the keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]
- [`segment_filter`][filter-by segment]

These filters will reduce the number of keys considered for deleting:

- [`modified_filter`][filter-by modified]
