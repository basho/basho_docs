---
title: "V2 Multi-Datacenter Replication Reference: Scheduling Fullsync"
description: ""
project: "riak_kv"
project_version: "2.9.1"
lastmod: 2020-02-16T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-2.9.1:
    name: "Scheduling Fullsync"
    identifier: "managing_ref_v2_fullsync"
    weight: 101
    parent: "managing_ref_v2"
toc: true
aliases:
  - /riak/2.9.1/ops/mdc/v2/scheduling-fullsync
  - /riak/kv/2.9.1/ops/mdc/v2/scheduling-fullsync
---

{{% note title="Deprecation Warning" %}}
v2 Multi-Datacenter Replication is deprecated and will be removed in a future version. Please use [v3]({{<baseurl>}}riak/kv/2.9.1/using/reference/v3-multi-datacenter/scheduling-fullsync/) instead.
{{% /note %}}

## Scheduling Fullsync Operation

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
