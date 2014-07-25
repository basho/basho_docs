---
title: Managing Strong Consistency
project: riak
version: 2.0.0+
document: guide
audience: advanced
keywords: [operators, strong-consistency]
---

Riak's [[strong consistency]] feature provides you with a variety of
tunable parameters.

Documentation for developers using strong consistency can be found in
[[Using Strong Consistency]], while a more theoretical treatment of the
feature can be in found in [[Strong Consistency]].

All of the parameters listed below must be set in each node's
`advanced.config` file, _not_ in `riak.conf`. More information on the
syntax and usage of `advanced.config` can be found in our documentation
on [[advanced configuration|Configuration Files#Advanced-Configuration]],
as well as a full listing of [[strong consistency-related parameters|
Configuration Files#Strong-Consistency]].

## Timeouts

A variety of timeout settings are available for managing the performance
of strong consistency.

Parameter | Description | Default
:---------:-------------:--------
`peer_get_timeout` | The timeout used internally (in milliseconds) for reading consistent data. Longer timeouts will decrease the likelihood that some reads will fail, while shorter timeouts will entail shorter wait times for connecting clients but a greater risk	of failed operations. | 60000 (60 seconds)
`peer_put_timeout` | The analogous timeout for writes. As with the `peer_get_timeout` setting, longer timeouts will decrease the likelihood that some reads will fail, while shorter timeouts entail shorter wait times for connecting clients but a greater risk of failed operations. | 60000 (60 seconds)

## Worker and Leader Behavior

Ensemble leaders rely upon one or more concurrent workers to service
requests. You can choose how many workers are assigned to leaders using
the `peer_workers` setting. The default is 1. Increasing the number of
workers will make strong consistency system more computationally
expensive but can improve performance in some cases, depending on the
workload.

Parameter | Description | Default
:---------|:------------|:-------
`trust_lease` | | Determines whether leader leases are used to optimize reads. When set to `true`, a leader with a valid lease can handle reads directly without needing to contact any followers.
`ensemble_tick` | Determines how frequently, in milliseconds, leaders perform their periodic duties, including refreshing the leader lease. This setting must be lower than both `lease_duration` and `follower_timeout`. | 500
`lease_duration` | Determines how long a leader lease remains valid without being refreshed. This setting _should_ be higher than the `ensemble_tick` setting to ensure that leaders have time to refresh their leases before they time out, and it _must_ be lower than `follower_timeout`. | `ensemble_tick` * 2/3

## Merkle Tree Settings

All peers in Riak's strong consistency system maintain persistent
[Merkle trees](http://en.wikipedia.org/wiki/Merkle_tree) for all data
stored by that peer. These trees 

## Syncing

The consensus subsystem delays syncing to disk when performing certain
operations, which enables it to combine multiple operations into a
single write to disk.
