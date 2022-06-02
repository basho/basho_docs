---
title: "Default Configuration For Handoff May Cause Data Loss"
description: ""
menu:
  community:
    name: "Data Loss KV 2.1.0"
    identifier: "dataloss"
    weight: 325
    parent: "productadvisories"
toc: true
---


Info | Value
:----|:-----
Date issued | May 1, 2015
Product | Riak
Affected versions | 2.1.0

>**UPDATE:**
>
>Riak 2.1.1 has been released to address this issue in 2.1.0. Please upgrade.

## Overview

Default configuration for handoff may cause data loss in Riak 2.1.0.

## Description

In Riak 2.1.0, the default configuration for handoff.ip causes vnodes marked for transfer during handoff to be removed without transferring data to their new destination nodes. A mandatory change to configuration (riak.conf) will resolve this issue. While not all users are impacted by this issue, we recommend that all 2.1.0 
users upgrade to 2.1.1 which will be released shortly. 

NOTE: This is known to occur for ownership handoff. Investigation as to whether hinted handoff is affected is ongoing and this advisory will be updated when more information is available.

**UPDATE:**
Further investigation has shown that fallback transfers (hinted handoffs) are affected in the same way as ownership transfers.

## Affected Users
All users of 2.1.0 using riak.conf to configure their clusters are potentially impacted. Users that are using app.config and vm.args to configure their clusters are unaffected but should upgrade to 2.1.1 upon release. 
To verify whether you are affected, the below command must be run on each node in your cluster:

```
riak config effective | grep handoff.ip
```

Affected nodes will have a handoff ip of 127.0.0.1

```
handoff.ip = 127.0.0.1
```

## Impact

This bug impacts vnodes that are in process of handoff. Handoff data will be looped back to the source node during ownership handoff rather than being transferred to the destination node. Once ownership handoff is completed the data is removed from the source node. In the event of significant ownership handoff, which can happen during cluster expansion or contraction, all replicas of an object may be lost. Data loss occurs if all replicas of an object are lost as a result of this configuration issue. Replica loss can be triggered by cluster membership changes or other Riak cluster activity that triggers handoff behavior. Data loss is mitigated as long as at least one replica still exists and the below steps are followed. 

## Mitigation

You can immediately mitigate the issue by setting transfer limit to zero across the cluster by issuing the following on any node:

```
riak-admin transfer-limit 0
```

Then configure handoff.ip in riak.conf to an external IP address or 0.0.0.0 on all nodes.

Perform a rolling restart of Riak across your cluster to activate the new setting.

After correcting the configuration and restarting the nodes, you should run Riak KV repair on each cluster member as documented at [{{< baseurl >}}riak/latest/ops/running/recovery/repairing-partitions/]({{< baseurl >}}riak/latest/ops/running/recovery/repairing-partitions/) to recreate any missing replicas from available replicas elsewhere in the cluster.  We recommend performing the Riak KV repair in a round-robin fashion on each node of your cluster (node0, node1, node2, etc). Repeat this round-robin repair “n_val - 1” times. For example: the default configuration for n_val is 3, which means you would run Riak KV repair twice across the entire cluster. 

> NOTE: It is important to ensure that you execute in a round-robin fashion: node0, node1, node2 and then repeat.
A forthcoming 2.1.1 release will provide an updated default configuration.
