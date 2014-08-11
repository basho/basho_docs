---
title: "Multi Data Center Replication: v3 Scheduling Fullsync"
project: riak
header: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, schedule, fullsync]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-v3-Scheduling-Full-Sync'
}
---

The `fullsync_interval` parameter can be configured in the `riak-repl` section of `[[app.config|Configuration Files#app.config]]` with:

* a single integer value representing the duration to wait *in minutes* between fullsyncs

    **OR**


* a list of `[{"clustername", time_in_minutes}, {"clustername", time_in_minutes}, ...]` pairs for each sink participating in fullsync replication. Note the commas separating each pair, and `[ ]` surrounding the entire list.

##Examples

Sharing a fullsync time (in minutes) for all sinks:

```erlang
{riak_repl, [
   {data_root, "/configured/repl/data/root"},
   {fullsync_interval, 90} %% fullsync runs every 90 minutes
  ]}
```

List of multiple sinks with separate times in minutes:

```erlang
{riak_repl, [
   {data_root, "/configured/repl/data/root"},
   % clusters sink_boston + sink_newyork have difference intervals (in minutes)
   {fullsync_interval, [
                {"sink_boston", 120},  %% fullsync to sink_boston with run every 120 minutes
                {"sink_newyork", 90}]} %% fullsync to sink_newyork with run every 90 minutes
  ]}
```

{{#1.4.0+}}
## Additional fullsync stats

Additional fullsync stats per sink have been added in Riak Enterprise.

* `fullsyncs_completed` &mdash; The number of fullsyncs that have been completed to the specified sink cluster.
* `fullsync_start_time` &mdash; The time the current fullsink to the specified cluster began.
* `last_fullsync_duration` &mdash; The duration (in seconds) of the last completed fullsync.
{{/1.4.0+}}
