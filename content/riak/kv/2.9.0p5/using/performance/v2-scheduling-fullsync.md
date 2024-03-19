---
title: "V2 Scheduling Fullsync"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
lastmod: 2019-11-21T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-2.9.0p5:
    name: "V2 Scheduling Fullsync"
    identifier: "performance_v2_scheduling_fullsync"
    weight: 103
    parent: "managing_performance"
toc: true
commercial_offering: true
aliases:
  - /riak/2.9.0p5/using/performance/v2-scheduling-fullsync/
  - /riak/2.9.0/using/performance/v2-scheduling-fullsync/
  - /riak/kv/2.9.0/using/performance/v2-scheduling-fullsync/
  - /riak/kv/2.9.0p1/using/performance/v2-scheduling-fullsync/
  - /riak/kv/2.9.0p2/using/performance/v2-scheduling-fullsync/
  - /riak/kv/2.9.0p3/using/performance/v2-scheduling-fullsync/
  - /riak/kv/2.9.0p4/using/performance/v2-scheduling-fullsync/
---

{{% note title="Deprecation Warning" %}}
v2 Multi-Datacenter Replication is deprecated and will be removed in a future version. Please use [v3]({{<baseurl>}}riak/kv/2.9.0p5/using/cluster-operations/v3-multi-datacenter/#fullsync-replication-commands/) instead.
{{% /note %}}

With the `pause` and `resume` commands it is possible to limit the
fullsync operation to off-peak times. First, disable `fullsync_interval`
and set `fullsync_on_connect` to `false`. Then, using cron or something
similar, execute the commands below at the start of the sync window.
In these examples, the commands are combined in a `.sh` or analogous
file:

```bash
#!/bin/sh

## Resume from where we left off

riak-repl resume-fullsync

## Start fullsync if nothing is running

riak-repl start-fullsync
```

At the end of the sync window:

```bash
#!/bin/sh

## Stop fullsync until start of next sync window

riak-repl pause-fullsync
```
