---
title: "Default Configuration For Handoff May Cause Data Loss in TS"
description: ""
menu:
  community:
    name: "Data Loss TS 1.3.0"
    identifier: "tsdataloss"
    weight: 100
    parent: "productadvisories"
toc: true
---


Info | Value
:----|:-----
Date issued | June 30, 2016
Product | Riak TS (Open Source)
Affected versions | 1.3.0

{{% note title="UPDATE" %}} Riak TS 1.3.1 has been released. Please download it [here]({{< baseurl >}}riak/ts/1.3.1/downloads/)
{{% /note %}}

## Overview

Default configuration for handoff may cause data loss in the OSS release of Riak TS 1.3.0. If you are using Riak TS Enterprise, you are not impacted by this bug but you **SHOULD** upgrade to [Riak TS Enterprise 1.3.1]({{< baseurl >}}riak/ts/1.3.1/downloads/) for other handoff bug fixes.


## Description

In Riak TS 1.3.0, the default configuration for handoff.ip causes vnodes marked for transfer during handoff to be removed without transferring data to their new destination nodes. A mandatory change to configuration (in riak.conf) will resolve this issue. All open source users are impacted by this issue and we strongly recommend that all 1.3.0 users [upgrade to 1.3.1]({{< baseurl >}}riak/ts/1.3.1/downloads/). 

**NOTE:** This is known to occur for ownership handoff and fallback transfers (hinted handoffs).


## Affected Users

All open source users of TS 1.3.0 using riak.conf to configure their clusters are potentially impacted.


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

Perform a [rolling restart]({{< baseurl >}}riak/kv/2.1.4/using/repair-recovery/rolling-restart/) of Riak across your cluster to activate the new setting.

For additional repair work, you will need to have Riak TS 1.3.1 or higher installed across your cluster.


### After Riak TS 1.3.1 is installed

{{% note title="Warning" %}}Please do not conduct the rest of the mitigation process until you have 1.3.1 installed.

Handoffs should remain disabled until that point in time. Do not add or remove nodes until you have upgraded your cluster to Riak TS 1.3.1 or higher.
{{% /note %}}

You should run Riak repair on each cluster member as documented at [{{< baseurl >}}riak/latest/ops/running/recovery/repairing-partitions/]({{< baseurl >}}riak/latest/ops/running/recovery/repairing-partitions/) to recreate any missing replicas from available replicas elsewhere in the cluster.  We recommend performing the Riak repair in a round-robin fashion on each node of your cluster (node0, node1, node2, etc). Repeat this round-robin repair “n_val - 1” times. For example: the default configuration for n_val is 3, which means you would run Riak repair twice across the entire cluster. 

> **NOTE:** It is important to ensure that you execute in a round-robin fashion: node0, node1, node2 and then repeat.
