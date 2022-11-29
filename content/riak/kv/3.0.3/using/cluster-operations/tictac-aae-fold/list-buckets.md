---
title_supertext: "Using > TicTac AAE Fold:"
title: "List Buckets"
description: ""
project: "riak_kv"
project_version: 3.0.3
menu:
  riak_kv-3.0.3:
    name: "List Buckets"
    identifier: "cluster_operations_tictac_aae_fold_list_buckets"
    weight: 104
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

Returns a list of bucket names stored in Riak.

See the [TicTac AAE `aae_folds`][tictacaae folds-overview] documentation for configuration, tuning and troubleshootings help.

## The `list_buckets` function

Run this using [`riak attach`][riak attach].

```erlang
riak_client:aae_fold({
    list_buckets,
    assumed_nval
    }, Client).
```
There are no available filters for this method.

`assumed_nval` should ideally be set to your cluster's default nval, but can be safely set to `1` for this purpose. Do not set it to below `1` or above your highest nval.

This will list all buckets:

```erlang
riak_client:aae_fold({
    list_buckets,
    3
    }, Client).
```

{{% note %}}
How to get the value for `Client` is detailed in [The Riak Client](../../tictac-aae-fold#the-riak-client).
{{% /note %}}

## The response

The response will be an array of bucket names, or tuples of bucket types and bucket names, as Erlang binaries. It looks something like this:

```erlang
{ok,[{<<"animals">>,<<"dogs">>},
     <<"cars">>]}
```

This shows that there are two buckets:

- "dogs" of bucket type "animals"
- "cars" with no bucket type