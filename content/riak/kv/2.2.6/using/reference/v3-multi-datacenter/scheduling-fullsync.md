---
title_supertext: "V3 Multi-Datacenter Replication Reference:"
title: "Scheduling Fullsync"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Scheduling Fullsync"
    identifier: "managing_ref_v3_fullsync"
    weight: 103
    parent: "managing_ref_v3"
toc: true
commercial_offering: true
aliases:
  - /riak/2.2.6/ops/mdc/v3/scheduling-fullsync
  - /riak/kv/2.2.6/ops/mdc/v3/scheduling-fullsync
---

[config reference#advanced]: {{<baseurl>}}riak/kv/2.2.6/configuring/reference/#advanced-configuration

The `fullsync_interval` parameter can be configured in the `riak-repl`
section of [`advanced.config`][config reference#advanced] with either:

* a single integer value representing the duration to wait, in minutes,
  between fullsyncs, _or_
* a list of pairs of the form `[{"clustername", time_in_minutes},
  {"clustername", time_in_minutes}, ...]` pairs for each sink
  participating in fullsync replication. Note the commas separating each
  pair, and `[ ]` surrounding the entire list.

## Examples

Sharing a fullsync time (in minutes) for all sinks:

```advancedconfig
{riak_repl, [
    % ...
    {data_root, "/configured/repl/data/root"},
    {fullsync_interval, 90} %% fullsync runs every 90 minutes
    % ...
    ]}
```

List of multiple sinks with separate times in minutes:

```advancedconfig
{riak_repl, [
    % ...
    {data_root, "/configured/repl/data/root"},
    % clusters sink_boston + sink_newyork have difference intervals (in minutes)
    {fullsync_interval, [
        {"sink_boston", 120},  %% fullsync to sink_boston with run every 120 minutes
        {"sink_newyork", 90}]} %% fullsync to sink_newyork with run every 90 minutes
  
    ]}
```

## Additional Fullsync Stats

Additional fullsync stats per sink have been added in Riak.

* `fullsyncs_completed` &mdash; The number of fullsyncs that have been
  completed to the specified sink cluster.
* `fullsync_start_time` &mdash; The time the current fullsink to the
  specified cluster began.
* `last_fullsync_duration` &mdash; The duration (in seconds) of the last
  completed fullsync.
