---
title: "Failure & Recovery"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "Failure & Recovery"
    identifier: "repair_recover_failure"
    weight: 100
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/2.0.6/ops/running/recovery/failure-recovery
  - /riak/kv/2.0.6/ops/running/recovery/failure-recovery
---

Riak was built to withstand---or at the very least reduce the severity
of---many types of system failure. Nonetheless, bugs are a reality,
hardware does break, and occasionally Riak itself will fail. Here, we'll
list some steps that can be taken to minimize the harm caused by a general
cluster failure.

## Forensics

When a failure occurs, collect as much information as possible. Check
monitoring systems, backup log and configuration files if they are
available, including system logs like `dmesg` and `syslog`. Make sure
that the other nodes in the Riak cluster are still operating normally and
are not affected by a wider problem like a virtualization or network outage.
Try to determine the cause of the problem from the data you have collected.

## Data Loss

Many failures incur no data loss or minimal loss that can be
repaired automatically, without intervention. Outage of a single node
does not necessarily cause data loss, as other replicas of every key are
available elsewhere in the cluster. Once the node is detected as down,
other nodes in the cluster will take over its responsibilities
temporarily and transmit the updated data to it when it eventually
returns to service (also called [hinted handoff]({{<baseurl>}}riak/kv/2.0.6/learn/glossary/#hinted-handoff)).

More severe data loss scenarios usually relate to hardware failure.
If data is lost, several options are available for restoring it.

1.  **Restore from backup** --- A daily backup of Riak nodes can be helpful.
    The data in this backup may be stale depending on the time at which
    the node failed, but it can be used to partially restore data from
    lost storage volumes. If running in a RAID configuration, rebuilding
    the array may also be possible.
2.  **Restore from multi-cluster replication** --- If replication is enabled
    between two or more clusters, the missing data will gradually be
    restored via realtime replication and fullsync replication. A
    fullsync operation can also be triggered manually via the `riak-repl`
    command.
3.  **Restore using intra-cluster repair** --- Riak versions 1.2 and greater
    include a repair feature which will restore lost partitions with
    data from other replicas. Currently, this must be invoked manually
    using the Riak console and should be performed with guidance from a
    Basho Client Services Engineer.

Once data has been restored, normal operations should continue. If
multiple nodes completely lose their data, consultation and assistance
from Basho are strongly recommended.

## Data Corruption

Data at rest on disk can become corrupted by hardware failure or other
events. Generally, the Riak storage backends are designed to handle
cases of corruption in individual files or entries within files, and can
repair them automatically or simply ignore the corrupted parts.
Otherwise, clusters can recover from data corruption in roughly the same
way that they recover from data loss.

## Out-of-Memory

Sometimes, Riak will exit when it runs out of available RAM. While this
does not necessarily cause data loss, it may indicate that the cluster
needs to be scaled out. If free capacity is low on the rest of the cluster while the node is out, other nodes may also be at risk, so monitor carefully.

Replacing the node with one that has greater RAM capacity may temporarily
alleviate the problem, but out-of-memory (OOM) issues tend to be an indication
that the cluster is under-provisioned.

## High Latency / Request Timeout

High latencies and timeouts can be caused by slow disks or networks or an
overloaded node. Check `iostat` and `vmstat` or your monitoring system to
determine the state of resource usage. If I/O utilization is high but
throughput is low, this may indicate that the node is responsible for
too much data and growing the cluster may be necessary. Additional RAM
may also improve latency because more of the active dataset will be
cached by the operating system.

Sometimes extreme latency spikes can be caused by [sibling explosion]({{<baseurl>}}riak/kv/2.0.6/developing/usage/conflict-resolution#siblings). This condition occurs when the client application does not resolve conflicts properly or in a timely fashion. In that scenario, the size of the value on disk grows in proportion to
the number of siblings, causing longer disk service times and slower
network responses.

Sibling explosion can be detected by examining the `node_get_fsm_siblings`
and `node_get_fsm_objsize` statistics from the `riak-admin status` command.
To recover from sibling explosion, the application should be throttled and
the resolution policy might need to be invoked manually on offending keys.

A Basho CSE can assist in manually finding large values, i.e. those that
potentially have a sibling explosion problem, in the storage backend.

MapReduce requests typically involve multiple I/O operations and are
thus the most likely to time out. From the perspective of the client
application, the success of MapReduce requests can be improved by reducing the
number of inputs, supplying a longer request timeout, and reducing the usage
of secondary indexes. Heavily loaded clusters may experience more MapReduce
timeouts simply because many other requests are being serviced as well. Adding
nodes to the cluster can reduce MapReduce failure in the long term by
spreading load and increasing available CPU and IOPS.

## Cluster Recovery From Backups

See [Changing Cluster Information]({{<baseurl>}}riak/kv/2.0.6/using/cluster-operations/changing-cluster-info/#clusters-from-backups) for instructions on cluster recovery.

{{% note title="Tip" %}}
If you are a licensed Riak Enterprise or CS customer and require assistance or
further advice with a cluster recovery, please file a ticket with the
<a href="https://help.basho.com">Basho Helpdesk</a>.
{{% /note %}}