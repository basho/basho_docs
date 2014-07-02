---
title: Runtime Interaction
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [runtime, ops, troubleshooting]
---

Riak's [[configuration system|Configuration Files]] provides a variety
of parameters that enable you to fine-tune how Riak interacts with
elements of the underlying operating system.

## Ports

Riak is 

`runtime_health.triggers.distribution_port`

Whether distribution ports with full input buffers will be counted as
busy. Distribution ports connect Riak nodes within a single cluster.

Default: `on`

`runtime_health.triggers.port`

Whether ports with full input buffers will be counted as busy. Ports can
represent open files or network sockets.

Default: `on`

`runtime_health.thresholds.busy_ports`

The threshold at which a warning will be triggered about the number of
ports that are overly busy. Ports with full input buffers count toward
this threshold.

Default: `2`

## Processes

`runtime_health.triggers.process.heap_size`

A process will become busy when its size exceeds this size (in bytes).

Default: `160444000`

`runtime_health.triggers.process.garbage_collection`

A process will become busy when it exceeds this amount of time doing
garbage collection. Enabling this setting can cause performance
problems on multi-core systems.

Default: `off`

`runtime_health.thresholds.busy_processes`

The threshold at which a warning will be triggered about the number of
processes that are overly busy. Processes with large heaps or that take
a long time to garbage collect will count toward this threshold.

Default: `30`
