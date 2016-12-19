---
title: "Configuring riak.conf in Riak TS"
description: "Configuration options for riak.conf in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "riak.conf"
    identifier: "riakconf"
    weight: 120
    parent: "configure"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
  locations:
    - [">=1.5.0", "using/configuring"]
aliases:
    - /riakts/1.5.0/configuring/riakconf
canonical_link: "https://docs.basho.com/riak/ts/latest/configuring/riakconf"
---


[glossary quanta]: ../../learn/glossary/quanta
[Riak object settings]: /riak/kv/2.2.0/configuring/reference/#object-settings


Riak TS exposes a few configuration settings in riak.conf. This document will walk you through the TS configurations. 

{{% note title="Deprecation Warning" %}}
The Riak TS configuration settings in riak.conf have changed. The old settings will be deprecated. Please update your riak.conf with the new settings.
{{% /note %}}


## Configuration options

Make certain the configuration file on each node gets the same parameters to avoid inconsistent behavior.

Benchmarking of your use cases and traffic load is recommended when changing these parameters. Settings which are too permissive can result in a slow database under heavy load.

### Timeout

Use `riak_kv.query.timeseries.timeout` to configure the timeout for queries, after which a timeout error is returned. The default is '10s'.

```riak.conf
riak_kv.query.timeseries.timeout = 10s
```

The supported units for a duration are:

- Milliseconds - `ms`
- Seconds - `s`
- Minutes - `m`
- Hours - `h`
- Days - `d`
- Weeks - `w`
- Fortnight - `f`

You can also combine units. For example, setting timeout to 3 minutes and 14 seconds:

```riak.conf
riak_kv.query.timeseries.timeout = 3m14s
```

If no unit is added, `riak_kv.query.timeseries.timeout` will be read in milliseconds:

```riak.conf
riak_kv.query.timeseries.timeout = 10000
```

*This setting was formerly `timeseries_query_timeout_ms`, please update accordingly.*


### Maximum query queues

Use `riak_kv.query.timeseries.max_concurrent_queries` to set the maximum number of query queues that can run concurrently per node (each queue serving one query). The default is 3. The total number of queries that can be run on a cluster is the number of nodes multiplied by the `riak_kv.query.timeseries.max_concurrent_queries` value. This constraint is to prevent an unbounded number of queries overloading the cluster.

```riak.conf
riak_kv.query.timeseries.max_concurrent_queries = 3
```

*This setting was formerly `timeseries_max_concurrent_queries`, please update accordingly.*


### Maximum query queue length

Use `riak_kv.query.maximum_query_queue_length` to set the query queue length.

Increase the queue length to avoid refusing to accept queries, at the expense of higher latencies.

```riak.conf
riak_kv.query.timeseries.maximum_query_queue_length = 15
```


### Maximum quanta

{{% note %}}
In Riak TS 1.4, `riak_kv.query.timeseries.max_quanta_span` was intended to protect the user from SELECTing excessive amounts of data, with a default set to a low value (5) and a note recommending to consider [requantizing the data](/riak/ts/1.5.0/learn-about/bestpractices/#quantum). Since version 1.5, we use a running estimation of projected query size to determine whether the query results can be safely returned to the client (see `max_returned_data_size` below).  The `max_quanta_span` parameter has been kept but is no longer the limiting factor.
{{% /note %}}

Use `riak_kv.query.timeseries.max_quanta_span` to configure the maximum number of quanta that a query can span. The default is 5000.

```riak.conf
riak_kv.query.timeseries.max_quanta_span = 5000
```

The maximum allowable value is 10000.

*This setting was formerly `timeseries_query_max_quanta_span`, please update accordingly.*


### Maximum returned data size

Largest estimated size of the data a query can return to the client.

When a query is broken down into per-quantum subqueries, all subqueries are queued for execution. Starting from the arrival of the second result set, we estimate the projected query result size as:

* `TotalQuerySize = AverageSubqueryResultSize * NumberOfSubqueries`, for regular queries without a `LIMIT` clause;
* `TotalQuerySize = AverageSubqueryResultSize * LimitValue`, for queries with a `LIMIT` clause.

If the total size is found to exceed `max_returned_data_size`, the query is cancelled, with an error code 1022 ("Projected result of a SELECT query is too big").

```riak.conf
riak_kv.query.timeseries.max_returned_data_size = 10*1000*1000
```


### Maximum subqueries

Use `max_running_fsms` to throttle the number of simultaneously running FSMs (i.e., processes collecting data for a single subquery).  This number times the max size of data chunks determines the maximum volume of data the coordinator will have to deal with at any given time.

Increasing this parameter allows for faster execution of queries over many small quanta. Conversely, if your quanta are large, it may be advisable to limit the number of concurrent FSMs even further, depending on the expected size of data per quantum.

{{% note %}}
Setting `max_running_fsms` to 1 will effectively disable parallel collection of subquery results. As a side effect, projected query size estimation will become deterministic, where subqueries will be processed from first to last, one at a time.
{{% /note %}}

```riak.conf
riak_kv.query.timeseries.max_running_fsms = 20
```


### Object Size Limits

Time Series default configuration now has smaller object sizes in order to improve performance.

The new defaults are now

```riak.conf
object.size.warning_threshold = 50K
object.size.maximum = 500K
```


### Query buffers root path

Root path for leveldb instances backing node-local query buffers.

For queries with an `ORDER BY` clause and/or `LIMIT` or `OFFSET` keywords, a separate, slower, code path will be used, whereby data collected from vnodes will be stored in a temporary *query buffers*. Each query buffer is a disk-backed leveldb table, created as a subdirectory under ` which will be deleted after the query is served.

{{% note %}}
1. Placing it on a fast media, such as a dedicated SSD or a RAM-based disk such as `/tmp`, is recommended.

2. The contents of this directory will be deleted at node startup and shutdown. If you choose to specify a path outside of `$(platform_data_dir)`, make sure no other process reads or writes to it.
{{% /note %}}

```riak.conf
riak_kv.query.timeseries.qbuf_root_path = "$(platform_data_dir)/query_buffers"
```


Further documentation about these settings can be found at [Riak object settings].
