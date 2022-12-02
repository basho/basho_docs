---
title_supertext: "Using > TicTac AAE Fold:"
title: "Filters"
description: ""
project: "riak_kv"
project_version: 3.0.7
menu:
  riak_kv-3.0.7:
    name: "Filters"
    identifier: "cluster_operations_tictac_aae_fold_filters"
    weight: 101
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

## Filter by bucket name

This will reduce the number of keys checked.

### Buckets without a bucket type

Use the name of the bucket as a binary. For example, to query bucket "cars", one would use:

```erlang
<<"cars">>
```

This example will count the number of keys in the bucket "cars":

```riakattach
riak_client:aae_fold({
    object_stats,
    <<"cars">>, 
    all, 
    all
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

### Buckets with a bucket type

Use the name of the bucket type and the bucket as a tuple pair of binaries. For example, to query bucket "dogs" with bucket type "animals", one would use:

```erlang
{<<"animals">>, <<"dogs">>}
```

This example will count the number of keys in the bucket "dogs" of bucket type "animals":

```riakattach
riak_client:aae_fold({
    object_stats,
    {<<"animals">>, <<"dogs">>}, 
    all, 
    all
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## Filter by key range

This will reduce the number of keys checked.

### From -> To

TicTacAAE stores keys in a bucket in a tree, so if you want a key starting with `n` you would have to go through all keys starting with `a` to `m` before reaching your starting point - and then continue on past to the very last key. This is very inefficient if you want only a specific subset of keys. Thankfully, TicTacAAE's trees are sorted by keyname, and you can make `aae_fold` jump straight to any key before starting and then automatically stop at any later key using the key range filter.

Use the name of the key you want to start and end at as a tuple pair of binaries. For example, to query keys starting with `n`, you would filter by `n` to `o`:

```erlang
{<<"n">>, <<"o">>}
```

This example will count the number of keys in the bucket `cars` that start with `n`:

```riakattach
riak_client:aae_fold({
    object_stats, 
    <<"cars">>, 
    {<<"n">>,<<"o">>}, 
    all
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

{{% note title="Warning: case sensitive" %}}
As the values used for key filters are binary strings, they are case sensitive. So `a` and `A` are not the same.
{{% /note %}}

### All keys

To query all keys, just use `all` for the key range filter. This will count all keys in the bucket `cars`:

```riakattach
riak_client:aae_fold({
    object_stats, 
    <<"cars">>, 
    all, 
    all
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## Filter by segment

This filter is used internally by TictacAAE and for custom replication functions. It's usage is not covered by this guide.

Use `all` for this filter.

## Filter by date modified

This will not reduce the number of keys checked, but will reduce the number of keys returned.

This filter is used when you need to locate keys modified in a certain time frame.

The values are passed in a tuple with 3 values:

```erlang
{date,From,To}
```

`date` is required. `From` and `To` are either a non-negative interger of seconds since `1970-01-01 00:00:00`, or two tuples containing dates in the format `{{Year,Month,Day},{Hour,Minute,Second}}`.

For example, to get all keys modified between 1970-01-01 00:01:00 (`From` = `{1970,1,1},{0,1,0}` or `60`) and 1970-01-01 00:02:00 (`To` = `{1970,1,1},{0,2,0}` or `120`), one would use either of these:

```erlang
% using easily readable dates

{date,{{1970,1,1},{0,1,0}},{{1970,1,1},{0,2,0}}}

% or using seconds since 1970-01-01 00:00:00

{date,60,120}
```

This example returns all keys in the "cars" bucket that were modified after 12-noon on 2022-05-01 (`From` = 2022-05-01 12:00:00 and `To` = 2022-05-02 00:00:00):

```riakattach
riak_client:aae_fold({
    object_stats, 
    <<"cars">>, 
    all, 
    {date,{{2022,5,1},{12,0,0}},{{2022,5,2},{0,0,0}}}
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

{{% note title="Working out the number of seconds" %}}
It's easier to use the `{{Year,Month,Day},{Hour,Minute,Second}}` format for `From` and `To`, but if you want to use the number of seconds instead, they can be worked out using this helper function:

```erlang
Modified_Filter_Calculator = fun (StartDateTime, EndDateTime) ->
  EpochTime = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  LowTS = calendar:datetime_to_gregorian_seconds(StartDateTime) - EpochTime,
  HighTS = calendar:datetime_to_gregorian_seconds(EndDateTime) - EpochTime,
  {date, LowTS, HighTS}
end.
```

This can then be used like so to get the filter value for the range of "2022-01-01 00:00:00" to "2022-02-01 00:00:00" (i.e. all of January 2022):

```erlang
Modified_Filter_Value = Modified_Filter_Calculator(
  {{2022,1,1},{0,0,0}},
  {{2022,2,1},{0,0,0}}
),
riak_client:aae_fold({
    object_stats, 
    <<"cars">>, 
    all, 
    Modified_Filter_Value
    }, Client).
```

Or in one command to make it easily re-usable:

```riakattach
riak_client:aae_fold({
    object_stats, 
    <<"cars">>, 
    all, 
    Modified_Filter_Calculator({{2022,1,1},{0,0,0}}, {{2022,2,1},{0,0,0}})
    }, Client).
```

{{% /note %}}

## Filter by sibling count

This will not reduce the number of keys checked, but will reduce the number of keys returned.

This filter is used when you need to locate keys with more than a given number of siblings. It can only be used with [`find_keys`][tictacaae find-keys].

[Learn More >>][filter-by sibling-count]

## Filter by object size

This will not reduce the number of keys checked, but will reduce the number of keys returned.

This filter is used when you need to locate keys whose object size is greater than a specified value. It can only be used with [`find_keys`][tictacaae find-keys].

[Learn More >>][filter-by object-size]
