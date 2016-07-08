---
title: "Configure Riak TS"
description: "Configure Riak TS"
menu:
  riak_ts-1.3.0:
    name: "Configure"
    identifier: "configure_riakts"
    weight: 305
    parent: "using"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/using/configuring/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/configuring"
---


[glossary quanta]: ../../learn/glossary/quanta


Riak TS exposes a few configuration settings via `riak.conf`. Make certain the configuration file on each node gets the same parameters to avoid inconsistent behavior.

Benchmarking of your use cases and traffic load is recommended when changing these parameters. Settings which are too permissive can result in a slow database under heavy load.

## Configuration options

### Timeout

Use `timeseries_query_timeout_ms` to configure the timeout (in milliseconds) for queries, after which a timeout error is returned.

```riak.conf
timeseries_query_timeout_ms = 10000
```


### Maximum quanta

Use `timeseries_query_max_quanta_span` to configure the maximum number of quanta that a query can span

```
timeseries_query_max_quanta_span = 3
```

For example, if you set `QUANTUM(time, 15, 'm')` in your TS table, setting `timeseries_query_max_quanta_span` to '5' would allow a query to return results within a time span of 75 minutes.  If a query has a larger time span, an error would be returned to the client and the query would not run.

This option is intended to prevent excessively long-running queries that could affect the performance of the cluster.

The maximum allowable value is 256.


### Maximum queries

Use `timeseries_max_concurrent_queries` to set the maximum number of queries that can run concurrently per node. The total number of queries that can be run on a cluster is the number of nodes multiplied by the `timeseries_max_concurrent_queries` value. This constraint is to prevent an unbounded number of queries overloading the cluster.

```
timeseries_max_concurrent_queries = 3
```
