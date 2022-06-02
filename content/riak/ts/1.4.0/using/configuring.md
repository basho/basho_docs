---
title: "Configure Riak TS"
description: "Configure Riak TS"
menu:
  riak_ts-1.4.0:
    name: "Configure"
    identifier: "configure_riakts"
    weight: 305
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
version_history:
  locations:
    - [">=1.5.0", "configuring"]
    - ["<=1.4.0",  "using/configuring"]
aliases:
    - /riakts/1.4.0/using/configuring/
---


[glossary quanta]: ../../learn/glossary/quanta


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


### Maximum quanta

{{% note %}}
Before you change this setting, we recommend you take a moment to determine whether requantizing your data would be a better option. If your data is not optimally quantized, upping the maximum quanta setting may make your queries less efficient. You can read more about best practices for quantizing your data [here]({{<baseurl>}}riak/ts/1.4.0/learn-about/bestpractices/#quantum).
{{% /note %}}


Use `riak_kv.query.timeseries.max_quanta_span` to configure the maximum number of quanta that a query can span. The default is 5.

```riak.conf
riak_kv.query.timeseries.max_quanta_span = 5
```

You should set this parameter to be one more than your desired timespan, else a fraction of your queries will fall outside your `riak_kv.query.timeseries.max_quanta_span` window. In general, your timespan won't be an integer multiple of the quantum, so if your maximum size query is dtmax, and your quantum size is Q, then we recommend: 
max_quanta = ceil(dtmax / Q + 1.0). 

If a query has a larger time span than your specified maximum, the error `too_many_subqueries` will show and the query will not run. This option is intended to prevent excessively long-running queries that could affect the performance of the cluster.

The maximum allowable value is 256.

*This setting was formerly `timeseries_query_max_quanta_span`, please update accordingly.*


### Maximum queries

Use `riak_kv.query.timeseries.max_concurrent_queries` to set the maximum number of queries that can run concurrently per node. The default is 3. The total number of queries that can be run on a cluster is the number of nodes multiplied by the `riak_kv.query.timeseries.max_concurrent_queries` value. This constraint is to prevent an unbounded number of queries overloading the cluster.

```riak.conf
riak_kv.query.timeseries.max_concurrent_queries = 3
```

*This setting was formerly `timeseries_max_concurrent_queries`, please update accordingly.*
