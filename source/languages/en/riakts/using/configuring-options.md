---
title: Riak TS Configuration Options
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: advanced
---

Riak TS provides configuration options in each nodes `riak.conf` file.

##### timeseries_query_timeout_ms

Timeout in milliseconds for Time Series queries, after which riak will return a timeout error.

```
timeseries_query_timeout_ms = 10000
```

##### timeseries_query_max_quanta_span

The maximum number of quanta that a query can span is set by the ``timeseries_query_max_quanta_span` option.  The quanta relates to the table's quantum. For example:

```
quantum(time, 15, 'm')
```

Setting the `timeseries_query_max_quanta_span` option to 5 would allow a query to return results within a time span of 75 minutes.  If a query has a larger time span then an error is returned to the client and the query is not run.

It is constrained to prevent excessively long running queries, that could affect the performance of the cluster.

```
timeseries_query_max_quanta_span = 3
```

##### timeseries_max_concurrent_queries

The maximum number of queries that can run concurrently per node. The total number of queries that can be run on a cluster is the number of nodes multipled by the `timeseries_max_concurrent_queries` value. This constraint is to prevent an unbounded number of queries overloading the cluster.

```
timeseries_max_concurrent_queries = 3
```