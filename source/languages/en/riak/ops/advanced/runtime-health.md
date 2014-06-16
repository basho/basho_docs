---
title: Runtime Interaction
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [runtime, ops, troubleshooting]
---

Riak's [[configuration system|Configuration Files]] provides a variety
of parameters that enable you to fine-tune how Riak interacts with the
underlying operating system.

## Ports

`runtime_health.triggers.distribution_port`
`runtime_health.triggers.port`
`runtime_health.thresholds.busy_ports`

## Memory Usage

`runtime_health.triggers.process.heap_size`
`runtime_health.triggers.process.garbage_collection`
`runtime_health.thresholds.busy_processes`