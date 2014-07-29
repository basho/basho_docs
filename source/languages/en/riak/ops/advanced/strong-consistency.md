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

## ensemble-status

This command is used to provide insight into the current status of the
consensus subsystem undergirding Riak's [[strong consistency]] feature.

```bash
riak-admin ensemble-status
```

If this subsystem is not currently enabled, you will see `Note: The
consensus subsystem is not enabled.` in the output of the command.

If the consensus subsystem is enabled, you will see output like this:

```
============================== Consensus System ===============================
Enabled:     true
Active:      true
Ring Ready:  true
Validation:  strong (trusted majority required)
Metadata:    best-effort replication (asynchronous)

================================== Ensembles ==================================
 Ensemble     Quorum        Nodes      Leader
-------------------------------------------------------------------------------
   root       4 / 4         4 / 4      riak@riak1
    2         3 / 3         3 / 3      riak@riak2
    3         3 / 3         3 / 3      riak@riak4
    4         3 / 3         3 / 3      riak@riak1
    5         3 / 3         3 / 3      riak@riak2
    6         3 / 3         3 / 3      riak@riak2
    7         3 / 3         3 / 3      riak@riak4
    8         3 / 3         3 / 3      riak@riak4
```

### Interpreting ensemble-status Output

The following table provides a guide to `ensemble-status` output:

Item | Meaning
:----|:-------
`Enabled` | Whether the consensus subsystem is enabled on the current node, i.e. whether the `strong_consistency` parameter in `[[riak.conf|Configuration Files#Strong-Consistency]]` has been set to `on`. If this reads `false` and you wish to enable strong consistency, see our documentation on [[enabling strong consistency|Using Strong Consistency#Enabling-Strong-Consistency]].
`Active` | Whether the consensus subsystem is active, i.e. whether there are enough nodes in the cluster to use strong consistency, which requires at least three nodes.
`Ring Ready` | If `true`, then all of the vnodes in the cluster have seen the current [[ring|Clusters#The-Ring]], which means that the strong consistency subsystem can be used; if `false`, then the system is not yet ready. If you have recently added or removed a node to/from the cluster, it may take some time for `Ring Ready` to change.
`Validation` | This will display `strong` if the `tree_validation` setting in `[[riak.conf|Configuration Files#Strong-Consistency]]` has been set to `on` and `weak` if set to `off`.
`Metadata` | This depends on the value of the `synchronous_tree_updates` setting in `[[riak.conf|Configuration Files#Strong-Consistency]]`, which determines whether strong consistency-related Merkle trees are updated synchronously or asynchronously. If `best-effort replication (asynchronous)`, then `synchronous_tree_updates` is set to `false`; if `guaranteed replication (synchronous)` then `synchronous_tree_updates` is set to `true`.
`Ensembles` | This displays a list of all of the currently existing ensembles active in the cluster.<br /><br /><ul><li></li><li></li><li></li><li></li></ul>

### Inspecting Specific Ensembles

The `ensemble-status` command enables you to inspect any currently
ensembles, i.e. the ensembles listed under `Ensembles` in the sample
`ensemble-status` output displayed above.

To inspect a specific ensemble, specify the ID:

```bash
riak-admin ensemble-status <id>
```

The following would inspect ensemble `2`:

```bash
riak-admin ensemble-status 2
```

Below is sample output:

```
================================= Ensemble #2 =================================
Id:           {kv,0,3}
Leader:       riak@riak2 (2)
Leader ready: true

==================================== Peers ====================================
 Peer  Status     Trusted          Epoch         Node
-------------------------------------------------------------------------------
  1    following    yes             1            riak@riak1
  2     leading     yes             1            riak@riak2
  3    following    yes             1            riak@riak2
```

The table below provides a guide to the output:

Item | Meaning
:----|:-------
`Id` | The ID for the ensemble used internally by Riak
`Leader` | Identifies the ensemble's leader
`Leader ready` | States whether the ensemble's leader is ready to respond to requests. If not, requests to the ensemble will fail.
`Peers` | A list of peer [[vnodes|Riak Glossary#vnode]] associated with the ensemble.<br /><br /><ul><li>**Peer** --- The ID of the peer</li><li>**Status** --- Whether the peer is a leader or a follower</li><li>**Trusted** --- Whether the peer's Merkle tree is currently considered trusted or not</li><li>**Epoch** --- The current consensus epoch for the peer. The epoch is incremented each time the leader changes.</li><li></li></ul>
