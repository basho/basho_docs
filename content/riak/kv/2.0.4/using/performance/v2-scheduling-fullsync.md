---
title: "V2 Scheduling Fullsync"
description: ""
project: "riak_kv"
project_version: "2.0.4"
lastmod: 2015-01-10T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.0.4:
    name: "V2 Scheduling Fullsync"
    identifier: "performance_v2_scheduling_fullsync"
    weight: 103
    parent: "managing_performance"
toc: true
commercial_offering: true
---

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
