---
title: "Configuring Riak TS"
description: "Configuring Riak TS"
menu:
  riak_ts-1.5.1:
    name: "Configure"
    identifier: "configure"
    weight: 250
    pre: cog
project: "riak_ts"
project_version: "1.5.1"
toc: true
version_history:
  locations:
    - [">=1.5.1", "configuring"]
    - ["<=1.4.0",  "using/configuring"]
aliases:
    - /riakts/1.5.1/configuring/
canonical_link: "https://docs.basho.com/riak/ts/latest/configuring"
---


[riakconf]: /riak/ts/1.5.1/configuring/riakconf/
[mdc]: /riak/ts/1.5.1/configuring/mdc/
[global expiry]: /riak/ts/1.5.1/configuring/global-object-expiration/
[kv config]: /riak/kv/2.2.0/configuring/reference
[WITH]: /riak/ts/1.5.1/using/creating-activating/#using-the-with-clause

Riak TS mostly relies on Riak KV's [default configuration settings][kv config]. However, there are a few TS-specific configurations you should know about:

* Riak TS exposes a few configuration settings in riak.conf. Read more about those settings [here][riakconf].
* If you are using Riak TS Enterprise Edition, you can learn more about configuring multi-datacenter replication (MDC) [here][mdc].
* You have the option of deleting data via global object expiration. For more information on how to configure global expiry, go [here][global expiry].
* The default `n_val` (the number of distinct copies of each record kept in your cluster for safety and availability) is 3. You can only change this default per-table when you create a table using `WITH`. Read more about that [here][WITH].

