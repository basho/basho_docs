---
title: "Configure Multi-Datacenter Replication for Riak TS"
description: "Configure Multi-Datacenter Replication for Riak TS"
menu:
  riak_ts-1.4.0:
    name: "MDC"
    identifier: "configure_mdc"
    weight: 306
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
version_history:
  in: "1.3.0+"
  locations:
    - [">=1.5.0", "configuring/mdc"]
    - ["<=1.4.0",  "using/mdc"]
aliases:
    - /riakts/1.4.0/using/mdc
---


[activating]: {{<baseurl>}}riak/ts/1.4.0/using/creating-activating
[cluster ops v3 mdc]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/v3-multi-datacenter
[ee]: http://basho.com/contact/
[Enterprise]: http://basho.com/products/riak-ts/
[install]: {{<baseurl>}}riak/ts/1.4.0/setup/installing


Multi-Datacenter (MDC) replication makes it possible to replicate your time series data between Riak clusters. This document will walk through how to configure MDC to work with Riak TS.


## Prerequisites

* You must be an [Enterprise] customer. To contact Basho for information on becoming an Enterprise customer, go [here][ee].
* You must have Riak TS [installed][install] on two distinct clusters.

>**Warning:** Do not attempt to replicate TS data to a Riak cluster on Riak KV or any version of Riak TS prior to 1.3.1.

* You must be using Multi-Datacenter v3 (v2 is incompatible with Riak TS).
* We do not yet support AAE-based fullsync with Riak TS.

## Configuration

### MDC

Please see our documentation on basic [configuration for MDC][cluster ops v3 mdc].

Unlike fullsync, realtime replication can be globally disabled for TS data without impacting the synchronization of Riak KV data.

Realtime replication for TS data can be disabled in the `riak_repl`
section of your advanced.config file by adding the following setting:

```advanced.config
    {ts_realtime, false}
```

{{% note %}}
If `{ts_realtime, false}` is not the last entry in your `riak_repl` configuration block, be sure to add a comma at the end of the line.
{{% /note %}}

See [below](#turn-off-replication-per-table) for more granular controls.


### TS Tables

Each TS table to be replicated must be [defined][activating] on each
cluster. MDC will not create new tables for you, and will compare the
data definition language (DDL) on each cluster to make certain they are
equivalent before synchronization occurs.


#### Turn off replication per-table

In order to disable fullsync, realtime, or all replication for your TS tables, you will need to set the bucket type property `repl` to either `realtime` (to use only realtime sync),
`fullsync` (to use only fullsync), or `false`. The default value is `true`, indicating that
both realtime and fullsync are enabled.

To set the `repl` property to either `true` or `false`, you can use the `riak-admin` command. Assuming a table named `sensors`:

```
$ riak-admin bucket-type update sensors '{"props":{"repl":false}}'
```

If you wish to enable only one of the replication modes by setting
`repl` to `fullsync` or `realtime`, you must use tweak the properties
directly from a Riak node or use a protobuf client. Please contact
Basho support for assistance.

While replication requires that the TS table be created on both
clusters involved, the `repl` property need only be set on a
cluster that will be sending data.
