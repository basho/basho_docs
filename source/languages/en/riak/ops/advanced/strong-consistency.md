---
title: Managing Strong Consistency
project: riak
version: 2.0.0+
document: guide
audience: advanced
keywords: [operators, strong-consistency]
---

Riak's [[strong consistency]] feature provides you with a variety of
tunable parameters that can be used to manage the subsystem
undergirding strongly consistent data stored in Riak.

Documentation for developers using strong consistency can be found in
[[Using Strong Consistency]], while a more theoretical treatment of the
feature can be in found in [[Strong Consistency]].

## Timeouts

A variety of timeout settings are available for managing performance.

Parameter | Description | Default
:---------:-------------:--------
`peer_get_timeout` | The timeout used internally (in milliseconds) for reading consistent data. Longer timeouts will decrease the likelihood that some reads will fail, while shorter timeouts will entail shorter wait times for connecting clients but a greater risk of failed operations. | 60000 (60 seconds)
`peer_put_timeout` | The analogous timeout for writes. As with the `peer_get_timeout` setting, longer timeouts will decrease the likelihood that some reads will fail, while shorter timeouts entail shorter wait times for connecting clients but a greater risk of failed operations. | 60000 (60 seconds)

## Workers and Leaders

Ensemble leaders rely upon one or more concurrent workers to service
requests. You can choose how many workers are assigned to leaders using
the `peer_workers` setting. The default is 1. Increasing the number of
workers will make strong consistency system more computationally
expensive but can improve performance in some cases, depending on the
workload.

## Syncing

The consensus subsystem delays syncing to disk when performing certain
operations, which enables it to combine multiple operations into a
single write to disk.
