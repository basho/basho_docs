---
title: "Runtime Interaction Reference"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Runtime Interaction"
    identifier: "managing_ref_runtime_interaction"
    weight: 104
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.9.2/ops/advanced/runtime
  - /riak/kv/2.9.2/ops/advanced/runtime
---

[config reference]: {{<baseurl>}}riak/kv/2.9.2/configuring/reference
[concept clusters]: {{<baseurl>}}riak/kv/2.9.2/learn/concepts/clusters

Riak's [configuration files][config reference] provide a variety of parameters that
enable you to fine-tune how Riak interacts with two important elements
of the underlying operating system: distribution ports and OS
processes/garbage collection.

## Ports

Distribution ports connect Riak nodes within a [cluster][concept clusters]. The
following port-related parameters are available:

* `runtime_health.triggers.distribution_port` --- Whether distribution
  ports with full input buffers will be counted as busy.
  * Default: `on`
* `runtime_health.triggers.port` --- Whether ports with full input
  buffers will be counted as busy. Ports can represent open files or network sockets.
  * Default: `on`
* `runtime_health.thresholds.busy_ports` --- The threshold at which a
  warning will be triggered about the number of ports that are overly
  busy. Ports with full input buffers count toward this threshold.
  * Default: `2`

## Processes

Riak will log warnings related to busy operating system processes and
garbage collection. You can specify the conditions in which warnings are
triggered using the following parameters:

* `runtime_health.thresholds.busy_processes` --- The threshold at which
  a warning will be triggered about the number of processes that are
  overly busy. Processes with large heaps or that take a long time to
  garbage collect will count toward this threshold.
  * Default: `30`
* `runtime_health.triggers.process.heap_size` --- A process will be
  marked as busy when its size exceeds this size (in bytes).
  * Default: `160444000`
* `runtime_health.triggers.process.garbage_collection` --- A process
  will be marked as busy when it exceeds this amount of time doing
  garbage collection. Enabling this setting can cause performance
  problems on multi-core systems.
  * Default: `off`
  * Example when enabled: `50ms`
* `runtime_health.triggers.process.long_schedule` --- A process will
  become busy when it exceeds this length of time during a single
  process scheduling and execution cycle.
  * Default: `off`
  * Example when enabled: `20ms`
