---
title: "Using Riak TS"
description: "Using Riak TS"
menu:
  riak_ts-1.4.0:
    name: "Use"
    identifier: "using"
    weight: 300
    pre: database
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/
---


[activating]: creating-activating/
[aggregate]: querying/select/aggregate-functions/
[arithmetic]: querying/select/arithmetic-operations/
[configuring]: configuring/
[download]: {{<baseurl>}}riak/ts/1.4.0/downloads/
[installing]: ../setup/installing/
[mdc]: mdc/
[planning]: planning/
[querying]: querying/
[riakshell]: riakshell/
[writing]: writingdata/


Now that you've [downloaded][download] and [installed][installing] Riak TS, there's a recommended path for setting up and using it:

1. [Plan][planning] your Riak TS table. Once created, tables can't be edited, so it's important to get it right the first time.
2. [Create and activate][activating] your Riak TS table. (You'll need `sudo` and `su` access for this step.) Then, if you are an Enterprise user, [set up Multi-Datacenter replication][mdc].
3. [Write data][writing] to your table.

Check out [riak shell][riakshell], a handy tool for using TS.
 
Then check out how to [query][querying] your data, [customize your Riak TS configuration][configuring], analyze your data with [aggregate functions][aggregate], or apply some [arithmetic operations][arithmetic].
