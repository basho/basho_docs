---
title: Failure and Recovery
project: riak
version: 1.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator, troubleshooting]
---

Riak's design protects against or reduces the severity of many types of
failures, but software bugs do happen and hardware does break.
Occasionally, Riak itself will fail.

## Forensics
When a failure occurs, collect as much information as possible. Check
monitoring systems, backup log and configuration files if they are
available, including system logs like `dmesg` and syslog. Make sure that
the other nodes in the Riak cluster are still operating normally and are
not affected by a wider problem like a virtualization or network outage.
Try to determine the cause of the problem from the data you have collected.

## Data Loss
Many failures do not incur data loss, or have minimal loss that can be
repaired automatically, without intervention. Outage of a single node
does not necessarily cause data loss, as other replicas of every key are
available elsewhere in the cluster. Once the node is detected as down,
other nodes in the cluster will take over its responsibilities
temporarily, and transmit the updated data to it when it eventually
returns to service (also called hinted handoff).

The more severe data loss scenarios usually relate to hardware failure.
In the cases where data is lost, several options are available for
restoring the data.

1.  Restore from backup. A daily backup of Riak nodes can be helpful.
    The data in this backup may be stale depending on the time  at which
    the node failed, but can be used  to partially-restore data from
    lost storage volumes. If running in a RAID configuration, rebuilding
    the array may also be possible.
2.  Restore from multi-cluster replication. If replication is enabled
    between two or more clusters, the missing data will gradually be
    restored via streaming replication and full-synchronization. A
    full-synchronization can also be triggered manually via the
    `riak-repl` command.
3.  Restore using intra-cluster repair. Riak versions 1.2 and greater
    include a repair feature which will restore lost partitions with
    data from other replicas. This currently has to be invoked manually
    using the Riak console and should be performed with guidance from a
    Basho CSE.

Once data has been restored, normal operations should continue. If
multiple nodes completely lose their data, consultation and assistance
from Basho is strongly recommended.

## Data Corruption
Data at rest on disk can become corrupted by hardware failure or other
events. Generally, the Riak storage backends are designed to handle
cases of corruption in individual files or entries within files, and can
repair them automatically or simply ignore the corrupted parts.
Otherwise, data corruption can be recovered from similarly to data loss.

## Out-of-Memory
Sometimes Riak will exit when it runs out of available RAM. While this
does not necessarily cause data loss, it may indicate that the cluster
needs to be scaled out. While the Riak node is out, if free capacity is
low on the rest of the cluster, other nodes may also be at risk, so
monitor carefully.

Replacing the node with one that has greater RAM capacity may temporarily
alleviate the problem, but out of memory (OOM) tends to be an indication
that the cluster is under-provisioned.

## High Latency / Request Timeout
High latencies and timeouts can be caused by slow disk, network, or an
overloaded node. Check `iostat` and `vmstat` or your monitoring system to
determine the state of resource usage. If I/O utilization is high but
throughput is low, this may indicate that the node is responsible for
too much data and growing the cluster may be necessary. Additional RAM
may also improve latency because more of the active dataset will be
cached by the operating system.

Sometimes extreme latency spikes can be caused by sibling-explosion.
This condition occurs  when the client application does not resolve
conflicts properly or in a timely fashion. In that scenario, the size of
the value on disk grows in proportion to the number of siblings, causing
longer disk service times and slower network responses.

Sibling-explosion can be detected by examining the
`node_get_fsm_siblings` and `node_get_fsm_objsize` statistics from the
`riak-admin status` command. To recover from sibling explosion, the
application should be throttled and the resolution policy might need to
be invoked manually on offending keys.

A Basho CSE can assist in manually finding large values (ones
potentially with siblings) in the storage backend.

MapReduce requests typically involve multiple I/O operations and are
thus the most likely to timeout. From the client application, reducing
the number of inputs, supplying a longer request timeout, and reducing
the usage of secondary indexes and key-listing can all improve the
success of MapReduce requests. Heavily-loaded clusters may experience
more MapReduce timeouts simply because many other requests are being
serviced as well. Adding nodes to the cluster can reduce MapReduce
failure in the long-term by spreading load and increasing available CPU
and IOPS.

## Cluster Recovery From Backups
The general procedure for recovering a cluster from catastrophic failure
involves installation of Riak on the new nodes to establish a cluster with
the same number of nodes, restoration of the original configuration, and
restoration of previous data.

Specifically, you should follow this basic process, ensuring that Riak
is **not started** on any node during steps 1-7:

1. Establish replacement cluster configured with the same number of nodes.
2. Restore the Riak configuration to each of the nodes.
3. Ensure that Riak is not running on any of the nodes.
4. Remove any previous Riak data (e.g., from `/var/lib/riak`) to ensure that
   the node is not started with no data present.
5. Restore the backup data to each node's data root (e.g, `/var/lib/riak`)
6. If you are restoring in an environment where the new nodes will have new
   network addresses (such as with AWS for example) or you will otherwise
   need to give the nodes new names, you need to execute `riak-admin reip`
   to change the network name for every node in the cluster from each node.
7. After renaming the nodes with `riak-admin reip` if necessary, you should
   check the `vm.args` configuration file to ensure that each node has the
   updated name.
8. Start the first node and check that its name is correct; one way to do
   this is to start the node, then attach to the node with `riak attach`.
   You should see the node name as part of the prompt as in the example
   below. Once you've verified the correct node name, exit the console
   with CTRL-D.
9. Execute `riak-admin member-status` on the node and verify that it
   returns expected output.
10. Start each of the remaining nodes, verifying the details in the same
    manner as the first node.

<div class="info"><div class="title">Tip</div> If you are a licensed
  Riak EDS or CS customer and require assistance or further advice with a
  cluster recovery, please file a ticket with the
  <a href="https://help.basho.com">Basho Helpdesk</a>.</div>



