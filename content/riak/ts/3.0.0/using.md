---
title: "Using Riak TS"
description: "Using Riak TS"
menu:
  riak_ts-3.0.0:
    name: "Use"
    identifier: "using"
    weight: 300
    pre: database
project: "riak_ts"
project_version: "3.0.0"
lastmod: 2022-09-20T00:00:00-00:00
sitemap:
  priority: 0.9
toc: true
aliases:
  - /riakts/3.0.0/using/

---

[activating]: creating-activating/
[aggregate]: querying/select/aggregate-functions/
[arithmetic]: querying/select/arithmetic-operations/
[configuring]: {{<baseurl>}}riak/ts/3.0.0/configuring/
[download]: {{<baseurl>}}riak/ts/3.0.0/downloads/
[installing]: ../setup/installing/
[mdc]: {{<baseurl>}}riak/ts/3.0.0/configuring/mdc/
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
