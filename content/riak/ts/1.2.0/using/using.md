---
title: "Using Riak TS"
description: "Using Riak TS"
menu:
  riak_ts-1.2.0:
    name: "Using Riak TS"
    identifier: "introduction"
    weight: 300
project: "riak_ts"
project_version: "1.2.0"
toc: true
<link rel="canonical" href="docs.basho.com/riak/ts/latest/using" />
---

[activating]: http://docs.basho.com/riakts/1.2.0/using/creating-activating/
[aggregate]: http://docs.basho.com/riakts/1.2.0/using/aggregate-functions/
[arithmetic]: http://docs.basho.com/riakts/1.2.0/using/arithmetic-operations/
[configuring]: http://docs.basho.com/riakts/1.2.0/using/configuring
[installing]: http://docs.basho.com/riakts/1.2.0/installing/installing/
[planning]: http://docs.basho.com/riakts/1.2.0/using/planning
[querying]: http://docs.basho.com/riakts/1.2.0/using/querying
[writing]: http://docs.basho.com/riakts/1.2.0/using/writingdata


Now that you've downloaded the package from ZenDesk and [installed][installing] Riak TS, there's a recommended path for setting up and using it:

1. [Plan][planning] your Riak TS table. Once created, tables can't be edited, so it's important to get it right the first time.
2. [Create and activate][activating] your Riak TS table. (You'll need `sudo` and `su` access for this step.)
3. [Write data][writing] to your table.

Once you've completed these steps you can go on to [query][querying] your data,[customize your Riak TS configuration][configuring], analyze your data with [aggregate functions][aggregate], or apply some [arithmetic operations][arithmetic].