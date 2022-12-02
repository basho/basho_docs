---
title_supertext: "Using > TicTac AAE Fold:"
title: "Find Tombstones"
description: ""
project: "riak_kv"
project_version: 3.0.2
menu:
  riak_kv-3.0.2:
    name: "Find Tombstones"
    identifier: "cluster_operations_tictac_aae_fold_find_tombs"
    weight: 103
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

Returns tuples of bucket name, keyname, and object size of Riak tombstone objects that meet the filter parameters.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

## The `find_tombs` function

Run this using [`riak attach`][riak attach].

```riakattach
riak_client:aae_fold({
    find_tombs, 
    bucket_filter, 
    key_range_filter, 
    segment_filter
    modified_filter
    }, Client).
```
Please see the list of [available filters](#available-filters) below.

For example, the following snippet will find all tombstones with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in January 2022

```riakattach
riak_client:aae_fold({
    find_tombs, 
    {<<"animals">>,<<"dogs">>}, 
    {<<"A">>,<<"N">>},
    all,
    {date,{{2022,1,1},{0,0,0}},{{2022,2,1},{0,0,0}}}
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## The response

The response will be an array of `{bucket_name,key_name,object_size}` tuples that will look something like this:

```erlang
{ok,[{{<<"animals">>,<<"dogs">>},<<"Barkie">>,550000},
    {{<<"animals">>,<<"dogs">>},<<"Lord Snuffles III">>,820000}]}
```

This indicates that 2 tombstones were found meeting the filter parameters. For each tombstone object found, an additional `{bucket_name,key_name,object_size}` tuple will be added to the array.

Field | Example | Description
:-------|:--------|:--------
bucket_name | `<<"cars">>` or `{<<"animals">>,<<"dogs">>}` | The bucket name as an Erlang binary. In the case of a bucket with a bucket type, a tuple of bucket type and bucket name.
key_name | `<<"Barkie">>` | The key name as an Erlang binary.
object_size | 550000 | The size in bytes of the tombstone object.

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys considered for reaping or counting.

These filters will reduce the keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]
- [`segment_filter`][filter-by segment]

These filters will reduce the number of keys considered for reaping or counting:

- [`modified_filter`][filter-by modified]


