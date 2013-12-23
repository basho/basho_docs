---
title: Linux Performance Tuning
project: riak
version: 1.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator, performance, os]
moved: {
    '1.4.0-': '/cookbooks/File-System-Tuning'
}
---

This guide describes recommended performance and
tuning, failure and recovery, and benchmarking information useful for
operators of new and existing Riak clusters.

<div class="note"><div class="title">Note</div>
The system tuning recommendations specified here should be considered as a starting point. Ideal tunings can vary significantly from system to system and be influenced by many variables. It is important to make note of what changes are made, when they are made, and to measure the impact those changes have.
</div>

Riak's primary bottleneck is disk and network I/O. Riak's I/O
pattern tends to operate on small blobs from many places on the disk. The
negative effects of this pattern can be mitigated by adding RAID over
multiple volumes, using solid state drives (SSD), and/or choosing the
Bitcask backend if secondary indexes are not needed for the application.

In any case, proper benchmarking and tuning are needed to achieve the desired
level of performance. Following the information in this guide should help you
with both.

<div class="info"><div class="title">Tip</div>For performance and tuning recommendations specific to running Riak clusters on the Amazon Web Services EC2 environment, see [[AWS Performance Tuning]].</div>

## Linux Tuning

While Linux can be tuned to be a good server operating system, many
distributions come out of the box tuned for desktop or light use.
Many of the kernel, network, and disk settings should be altered
to run as a production database server.

### Open Files Limit

Riak and the tools can consume a large number of open file handles during
normal operation.  For stability, increasing the number of open-files limit
is necessary.  See [[Open Files Limit]] for the details.

### Kernel and Network Tuning

The following settings are minimally sufficient to improve many aspects
of Riak usage on Linux, and should be added or updated in `/etc/sysctl.conf`:

<div class="note"><div class="title">Note</div>
In general, these recommended values should be compared with the system defaults
and only changed if benchmarks or other performance metrics indicate that
networking is the bottleneck.
</div>

```text
vm.swappiness = 0
net.ipv4.tcp_max_syn_backlog = 40000
net.core.somaxconn=40000
net.ipv4.tcp_sack = 1
net.ipv4.tcp_window_scaling = 1
net.ipv4.tcp_fin_timeout = 15
net.ipv4.tcp_keepalive_intvl = 30
net.ipv4.tcp_tw_reuse = 1
net.ipv4.tcp_moderate_rcvbuf = 1
```

The following settings are optional, but may improve performance on
a 10Gb network:

```text
net.core.rmem_max = 134217728
net.core.wmem_max = 134217728
net.ipv4.tcp_mem  = 134217728 134217728 134217728
net.ipv4.tcp_rmem = 4096 277750 134217728
net.ipv4.tcp_wmem = 4096 277750 134217728
net.core.netdev_max_backlog = 300000
```

Certain network interfaces ship with on-board features that have been shown
to hinder Riak network performance.  These features can be disabled via
`ethtool`.

For an Intel chipset NIC using the "ixgbe" driver running as **eth0**, for example,
run the following command:

```bash
ethtool -K eth0 lro off
```

For a Broadcom chipset NIC using the "bnx" or "bnx2" driver, run:

```bash
ethtool -K eth0 tso off
``` 

`ethtool` settings can be persisted across reboots by adding the above command 
to the /etc/rc.local script.

<div class="info"><div class="title">Tip</div>Tuning of these values
will be required if they are changed, as they affect all network
operations.</div>

### Swap Space

Due to the heavily I/O-focused profile of Riak, swap usage can result in
the entire server becoming unresponsive. Disable swap or otherwise
implement a solution for ensuring Riak's process pages are not swapped.

Basho recommends that the Riak node be allowed to be killed by the kernel
if it uses too much RAM. If swap is completely disabled, Riak will
simply exit when it is unable to allocate more RAM and leave a crash dump
(named `erl_crash.dump`) in the `/var/log/riak` directory which can be used
for forensics (by Basho Client Services Engineers if you are a customer).

### Mounts and Scheduler

Riak makes heavy use of disk I/O for its storage operations. It is
important to mount volumes that Riak will be using for data storage with
the **noatime** flag, meaning that filesystem [inodes](http://en.wikipedia.org/wiki/Inode)
on the volume will not be touched when read. This flag can be set temporarily
using the following command:

```bash
mount -o remount,noatime <riak_data_volume>
```

Replace &lt;riak_data_volume&gt; in the above example with your actual
Riak data volume. The **noatime** can be set in `/etc/fstab` to
mount permanently.

The default disk I/O scheduler (elevator) on Linux is completely fair
queuing or `cfq`, which is designed for desktop use. Basho recommends
using the `noop` or `deadline` schedulers for Riak data volumes,
which are better for server loads.

To check the scheduler in use for block device **sda**, for example, use the
following command:

```bash
cat /sys/block/sda/queue/scheduler
```

To set the scheduler to deadline, use the following command:

```bash
echo deadline > /sys/block/sda/queue/scheduler
```

The default I/O scheduler queue size is 128.  The scheduler queue sorts
writes in an attempt to optimize for sequential I/O and reduce seek time. 
Changing the depth of the scheduler queue to 1024 can increase the proportion 
of sequential I/O that disks perform and improve overall throughput.

To check the scheduler depth for block device **sda**, use the following command:

```bash
cat /sys/block/sda/queue/nr_requests
```

To increase the scheduler depth to 1024, use the following command:

```bash
echo 1024 > /sys/block/sda/queue/nr_requests
```

### Filesystem

Basho recommends using advanced journaling filesystems like ZFS and XFS
for greater reliability and recoverability. However, the ext3 and ext4
filesystems are sufficient on operating systems where ZFS or XFS are not
available.

<div class="note"><div class="title">Note</div>Basho
<strong>does not</strong> currently recommend production use of the
<a href="http://zfsonlinux.org/">ZFS On Linux</a> project.</div>

The ext4 file system defaults include two options that increase
integrity, but slow performance. Because Riak's integrity is based on
multiple nodes holding the same data, these two options can be changed
to boost I/O performance. We recommend setting: `barrier=0` and
`data=writeback` when using the ext4 filesystem.

Similarly, the XFS file system defaults can be optimized to improve performance.
We recommend setting: `nobarrier`, `logbufs=8`, `logbsize=256k`, and
`allocsize=2M` when using the XFS filesystem.

As with the `noatime` setting, these settings should be added to
`/etc/fstab` so that they are persisted across server restarts.

## Cluster size

Deployments of five nodes or greater will provide a foundation for the
best performance and growth as the cluster expands. Since Riak scales
linearly with the addition of more nodes, users find improved
performance, reliability, and throughput with larger clusters. Smaller
deployments can compromise the fault-tolerance of the system: with the
default (3 copies) replication requirement for availability, node
failures in smaller clusters mean that replication requirements may not
be met.

The fewer nodes you have, the more work each node must perform as a
percentage of the cluster. If you have 3 nodes, and require all 3 to
replicate, 100% of your nodes will respond to a single request. If you
have 5, only 60% need respond. Note that the recommended number of nodes
for a cluster is without regard to the size of the instance chosen,
since a large node loss still must be balanced by the remaining system.

### Partitions (vnodes)

Related to the cluster size is the number of partitions in the cluster,
which is set permanently at cluster creation time and must be a power of
2 (64, 128, 256, etc). Each partition is responsible for an
approximately equal number of keys/values as its peers (data is spread
evenly around the cluster), and contains replicas for a fixed range of
keys.

With too few partitions, a cluster is limited in the maximum size to
which it can grow and I/O concurrency is also limited because partitions
are responsible for a larger portion of the keyspace. With too many
partitions, individual nodes may become overloaded with too much
context-switching and contention for I/O resources.

Basho recommends between 8 and 64 partitions per node in the cluster.
For example, this means that a 5-node cluster should be initialized with
128 or 256 partitions. With 256 partitions, a 5 node cluster could
conceivably grow to 32 nodes before needing replacement.

You can find more information on [[Cluster Capacity Planning]].

## Riak Configuration and Tuning
Riak uses sensible defaults for configuration, but a few items need
changing for a given deployment. Most importantly, the number of partitions
in the cluster should be applied before starting any participant node.

Place the ring_creation_size item within the riak_core section in the
`/etc/riak/app.config` file:

    {riak_core, [
       {ring_creation_size, 256},
       %% ...
       ]}

If using LevelDB as the storage backend (which maintains its own I/O
thread pool), the number of async threads in Riak's default pool can be
decreased in the `/etc/riak/vm.args` file:

```text
+A 16
```

Further information on Riak configuration is available in the [[Configuration files]] documentation.

<div class="info"><div class="title">Basho Tip</div>Questions about Riak
configuration can also be addressed to the Basho Client Services
<a href="http://help.basho.com/">help desk</a>.</div>

## Load balancer configuration
We recommend placing a load balancing solution, such as
[HAProxy](http://haproxy.1wt.eu/) between the application nodes and Riak.

Below is an example HAProxy configuration that is based on known-good
configuration values:

```text
listen riak 0.0.0.0:8087
    balance    leastconn
    mode       tcp
    option     tcplog
    option     contstats
    option     tcpka
    option     srvtcpka
    server riak-1 192.168.1.1:8087 check weight 1 maxconn 1024
    server riak-2 192.168.1.2:8087 check weight 1 maxconn 1024
    server riak-3 192.168.1.3:8087 check weight 1 maxconn 1024
    server riak-4 192.168.1.4:8087 check weight 1 maxconn 1024
    server riak-5 192.168.1.5:8087 check weight 1 maxconn 1024
```

This configuration may have to be adjusted with respect to connection,
server, and client timeouts. Where possible, use the kernel-splicing
feature (on by default) to connect clients to Riak nodes for best
performance.

## Scaling Riak
Riak can be scaled in two ways, vertically (improved hardware) and
horizontally (more nodes). Both ways can provide performance and
capacity benefits, but should be used in different circumstances. The
[[riak-admin cluster command|riak-admin Command Line#cluster]] can assist in scaling both directions.

When changing the cluster membership to scale vertically or
horizontally, follow these steps:

1.  Add, remove, or replace nodes using `riak-admin cluster [join|leave|replace]`.
2.  Check the cluster transition plan using `riak-admin cluster plan`.
    Ensure that the planned transition meets your expectations; that
    is, the correct nodes should be joining or leaving, and ring
    ownership should be transferred to the right nodes.
3.  Run `riak-admin cluster commit` to confirm the changes or `riak-admin cluster clear`
    to abort the changes.
4.  Monitor the membership transition progress using `riak-admin member-status`
    and `riak-admin ring-status`.

### Vertical Scaling
Vertical scaling, or improving the capabilities of a node/server, gives
greater capacity to the node but does not decrease the overall load on
existing members of the cluster. That is, the ability for the improved
node to handle existing load is increased, but the load itself is
unchanged. Reasons to scale vertically include increasing IOPS,
increasing CPU/RAM capacity, and increasing disk capacity.

If the improved node can be initialized with the same IP address and
data as the one it replaces, no changes to Riak are necessary. If the same
resources are not available to the new node, use `riak-admin cluster replace <oldnode>
<newnode>` after joining the new node.The node name(s) can be gathered
from the output of member-status or from the `/etc/riak/vm.args`
configuration file.

### Horizontal Scaling
Horizontal scaling, or increasing the number of nodes in the cluster,
reduces the responsibilities of each member node by spreading the
keyspace wider and providing additional endpoints for client
connections. That is, the capacity of each individual node does not
change, but its load is decreased. Reasons to scale horizontally include
increasing I/O concurrency, reducing the load on existing nodes, and
increasing disk capacity.

To scale horizontally, new nodes must be joined to the existing cluster
using the [[riak-admin cluster join command|riak-admin Command Line#cluster]]. When growing
the cluster by more than one node, all new nodes should be added at once
rather than individually. That is, do not commit the plan until all new
nodes are staged in the join state. This will reduce the total amount of
data that will need to be transferred across the network (ownership
handoff). Not doing so will likely result in the same data being moved
multiple times.

### Reducing Horizontal Scale
In the case where a Riak cluster is over-provisioned, or in response to
seasonal usage decreases, the horizontal scale of a Riak
cluster can shrink using the `riak-admin leave command` on the node to
be removed. In the interest of caution and safety, we recommend removing
one node at a time while monitoring the load, capacity and performance
of the remaining nodes.


## A Word of Caution
Cluster membership changes should not be taken lightly or performed
hastily. Redistributing the data among new members requires significant
disk and network resources and time, and the coordination of ownership
transitions takes additional time. As such, the rate of handoff is
intentionally limited in the default Riak configuration to avoid
impacting normal operations. This limit can be changed temporarily, in
cases where greater handoff throughput or lower impact is desired, using
the [[riak-admin transfer-limit command|riak-admin Command Line#transfer-limit]].

Additionally, nodes that become unavailable due to errors or maintenance
should not leave the cluster. To perform maintenance on a node, mark it
as down or stop Riak on the node, but do not have it leave the cluster.

If you encounter a failure, please read [[Failure and Recovery]] for more
information.


## Benchmarking
Using a tool such as [Basho Bench](https://github.com/basho/basho_bench),
you can generate load that simulates application operations by constructing
and communicating approximately-compatible data payloads with the Riak
cluster directly.

Benchmarking is critical to determining appropriate cluster node
resources, and is strongly recommended. More information is available
on benchmarking Riak clusters with [[Basho Bench]].

In addition to simply measuring performance, it is also important to
measure how performance degrades when the cluster is not in
steady-state. While under a simulation of live load, the following
states might be simulated:

1.  Stop one or more nodes normally and restart them after a few moments
    (simulates rolling-upgrade).
2.  Join two or more nodes to the cluster.
3.  Leave nodes from the cluster (after step #2).
4.  Hard-kill the Riak `beam.smp` process (i.e., `kill -9`) and then
    restart it.
5.  Restart a node.
6.  Hard-stop and destroy a node's instance and build a new one from
    backup.
7.  Via networking (firewall, perhaps), partition one or more nodes from
    the rest of the cluster, and then restore the original
    configuration.


## References

* [[AWS Performance Tuning]]
* [Why Your Riak Cluster Should Have At Least Five Nodes](http://basho.com/blog/technical/2012/04/27/Why-Your-Riak-Cluster-Should-Have-At-Least-Five-Nodes/)
* [[Failure and Recovery]]
* [[Configuration Files]]
* [erl - The Erlang Emulator](http://www.erlang.org/doc/man/erl.html#id154078)
* [[Configuration Files]]
* [Basho help desk](http://help.basho.com/)
* [[Command Line Tools]]
* [Basho Bench](https://github.com/basho/basho_bench)
* [[Basho Bench]]
