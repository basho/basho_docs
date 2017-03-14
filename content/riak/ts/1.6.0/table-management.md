---
title: "Table Management in Riak TS"
description: "Table Management in Riak TS"
menu:
  riak_ts-1.6.0:
    name: "Table Management"
    identifier: "table_management"
    weight: 275
    pre: icon-reorder
project: "riak_ts"
project_version: "1.6.0"
aliases:
    - /riakts/1.6.0/table-management/
canonical_link: "https://docs.basho.com/riak/ts/latest/table-management/"
---

[plan]: /riak/ts/1.6.0/using/planning/
[activating]: /riak/ts/1.6.0/table-management/creating-activating/
[global expiry]: /riak/ts/1.6.0/table-management/global-object-expiration/
[table expiry]: /riak/ts/1.6.0/table-management/per-table-object-expiration/
[mdc]: /riak/ts/1.6.0/configuring/mdc/
[writing]: /riak/ts/1.6.0/using/writingdata/

After [planning][plan] your Riak TS table:

1. [Create and activate][activating] your Riak TS table. (You'll need `sudo` and `su` access for this step.)
2. [Enable global object object expiration][global expiry] if you plan on setting up an object retention policy. If you are an Enterprise user, see the [per table object expiration][table expiry] instructions to enable object retention on per table basis.
3. Then, if you are an Enterprise user, [set up Multi-Datacenter replication][mdc].
4. [Write data][writing] to your table.

Check out [riak shell][riakshell], a handy tool for using TS.

Then check out how to [query][querying] your data, [customize your Riak TS configuration][configuring], analyze your data with [aggregate functions][aggregate], or apply some [arithmetic operations][arithmetic].
