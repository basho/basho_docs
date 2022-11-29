---
title_supertext: "Using > TicTac AAE Fold:"
title: "Get Object Statistics"
description: ""
project: "riak_kv"
project_version: 3.0.3
menu:
  riak_kv-3.0.3:
    name: "Object Statistics"
    identifier: "cluster_operations_tictac_aae_fold_object_stats"
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
[filters]: ../../tictac-aae-fold/filters
[filter-by bucket]: ../../tictac-aae-fold/filters#filter-by-bucket-name
[filter-by key-range]: ../../tictac-aae-fold/filters#filter-by-key-range
[filter-by segment]: ../../tictac-aae-fold/filters#filter-by-segment
[filter-by modified]: ../../tictac-aae-fold/filters#filter-by-date-modified
[filter-by sibling-count]: ../../tictac-aae-fold/find-keys/#the-sibling-count-filter
[filter-by object-size]: ../../tictac-aae-fold/find-keys/#the-object-size-filter

Returns a count of objects that meet the filter parameters.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

## The `object_stats` function

Run this using [`riak attach`][riak attach].

```erlang
riak_client:aae_fold({
    object_stats, 
    bucket_filter, 
    key_range_filter, 
    modified_filter
    }, Client).
```

Please see the list of [available filters](#available-filters) below.

For example, the following snippet will count all objects with the filters:

- in the bucket "dogs" of bucket type "animals"
- whose keys are between "A" and "N"
- which were modified in January 2022

```erlang
riak_client:aae_fold({
    object_stats, 
    {<<"animals">>,<<"dogs">>}, 
    {<<"A">>,<<"N">>},
    {date,1640995200,1643673600}
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## The response

The response will look something like this:

```erlang
{ok,[{total_count,100},
     {total_size,500000},
     {sizes,[{1,91},{2,5},{3,4}]},
     {siblings,[{1,90},{2,6},{3,4}]}]}
```

Field | Example | Description
:-------|:--------|:--------
total_count | {total_count,100}  | The total number of objects. In the example, 100 objects were found.
total_size | {total_size,500000}  | The total size of all objects found in bytes. In the example, all found objects came to a total size of 500,000 bytes.
sizes | {sizes,[{1,90},{2,5},{3,4}]} | A set of tuples giving a histogram of object size. The first number in each tuple is the order of magnitude starting at 1=1KB (1024 bytes x 10^N). The second number is the number of objects of that magnitude. In the example, there are 90 objects under 1KB, 5 objects between 1KB and 10KB, and 4 objects between 10KB and 100KB.
siblings | {siblings,[{1,90},{2,6},{3,4}]} | A set of tuples giving the sibling count of objects. The first number in each tuple is the number of siblings. The second number in each tuple is the number of objects that have that many siblings. A sibling count of `1` means that there are no siblings (there is only 1 value). In the example, there are 90 objects with no siblings, 6 objects with 2 siblings, and 4 objects with 3 siblings.

{{% note title="`object_size` reference table" %}}
For quick reference, here is a table of magnitude and object size range for the first 10 orders of magnitude:

Magnitude | Minimum (bytes) | Maximum (bytes)
:--------:|---------:|---------:
1|0|1024
2|1,025|10,240
3|10,241|102,400
4|102,401|1,024,000
5|1,024,001|10,240,000
6|10,240,001|102,400,000
7|102,400,001|1,024,000,000
8|1,024,000,001|10,240,000,000
9|10,240,000,001|102,400,000,000
10|102,400,000,001|1,024,000,000,000

{{% /note %}}

## Available filters

These filters are detailed in the [Filters][filters] documentation and can be used to limit the keys counted.

These filters will reduce the keys to be searched:

- [`bucket_filter`][filter-by bucket]
- [`key_range_filter`][filter-by key-range]

These filters will reduce the number of objects included in the statistics:

- [`modified_filter`][filter-by modified]
