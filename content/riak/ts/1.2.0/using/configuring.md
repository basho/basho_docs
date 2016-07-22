---
title: "Configure Riak TS"
description: "Configure Riak TS"
menu:
  riak_ts-1.2.0:
    name: "Configure"
    identifier: "configure_riakts"
    weight: 305
    parent: "using"
project: "riak_ts"
project_version: "1.2.0"
toc: true
aliases:
    - /riakts/1.2.0/using/configuring/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/configuring"
---


Depending on your needs, you can configure Riak TS in each node's riak.conf file.

## Configuration options

### Timeout

Use `timeseries_query_timeout_ms` to configure the timeout (in milliseconds) for queries, after which a timeout error is returned.

```riak.conf
timeseries_query_timeout_ms = 10000
```

### Maximum quanta

Use `timeseries_query_max_quanta_span` to configure the maximum number of quanta that a query can span.  The quanta relates to the table's quantum.

```
timeseries_query_max_quanta_span = 3
```

For example, if you set `quantum(time, 15, 'm')` in your Riak TS table, setting `timeseries_query_max_quanta_span` to '5' would allow a query to return results within a time span of 75 minutes.  If a query has a larger time span, the error `too_many_subqueries` will show and the query will not run.

This option is intended to prevent excessively long-running queries that could affect the performance of the cluster.


### Maximum queries

Use `timeseries_max_concurrent_queries` to set the maximum number of queries that can run concurrently per node. The total number of queries that can be run on a cluster is the number of nodes multiplied by the `timeseries_max_concurrent_queries` value. This constraint is to prevent an unbounded number of queries overloading the cluster.

```
timeseries_max_concurrent_queries = 3
```