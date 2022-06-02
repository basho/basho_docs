---
title: "Amazon Web Services Performance Tuning"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Amazon Web Services"
    identifier: "performance_aws"
    weight: 106
    parent: "managing_performance"
toc: true
aliases:
  - /riak/2.9.7/ops/tuning/aws
  - /riak/kv/2.9.7/ops/tuning/aws
---

This guide introduces best practices for tuning Riak cluster performance
in the Amazon Web Services (AWS) Elastic Compute Cloud (EC2) environment.

> **Note:**
>
> The following guide is supplementary. Be sure to check out [Improving Performance](../) for general performance and tuning recommendations before continuing with this guide.

## EC2 Instances

EC2 instances are available as predefined types which encapsulate a
fixed amount of computing resources. For Riak, the most important of
these resources are Disk I/O, RAM, and Network I/O, followed by CPU
cores. With this in mind, Riak users have reported success with large,
extra large, and cluster compute instance types for use as cluster nodes
in the AWS EC2 environment.

The most commonly used [instance types](http://aws.amazon.com/ec2/instance-types/) for Riak cluster nodes are `large` and `xlarge` `m` class (General Purpose), such as `m4.xlarge`. In cases where 10-gigabit Ethernet networking is desired, the Cluster Compute class of EC2 instances, such as `cc2.8xlarge` can be used.

Amazon also offers a High I/O Quadruple Extra Large instance
(`hi1.4xlarge`) that is backed by solid state drives (SSD) and features
very high I/O performance.

EBS-Optimized EC2 instances, which provide between 500 Megabits per
second and 1,000 Megabits per second of throughput with [Provisioned
IOPS](http://aws.amazon.com/about-aws/whats-new/2012/07/31/announcing-provisioned-iops-for-amazon-ebs/)
EBS volumes are also available, and recommended for use with Provisioned
IOPS EBS volumes.

Riak's primary bottleneck will be disk and network I/O, meaning that in
most cases, standard EBS will incur too much latency and iowait. Riak's
I/O pattern tends to operate on small blobs from many places on the
disk, whereas EBS is best at bulk reads and writes. The negative effects
of this pattern can be mitigated by adding RAID over multiple volumes,
using Provisioned IOPS, and/or choosing the Bitcask backend if secondary
indexes are not needed for the application.

In any case, proper benchmarking and tuning are needed to achieve the
desired performance.

{{% note title="Tip" %}}
Most successful AWS cluster deployments use more EC2 instances than they would
the same number of physical nodes to compensate for the performance
variability caused by shared, virtualized resources. Plan to have more EC2
instance based nodes than physical server nodes when estimating cluster size
with respect to node count.
{{% /note %}}

## Operating System

### Clocks

NTP is configured by default on Amazon EC2 Linux instances. Please
refer to the [Set the Time for an
Instance](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/set-time.html)
section of the EC2 documentation for steps on verifying if NTP is
working properly. If NTP is not working properly, significant clock
drift can occur.

### Mounts and Scheduler

On EBS volumes, the **deadline** scheduler should be used. To check the
scheduler in use for block device xvdf, for example, use the following
command:

```bash
cat /sys/block/xvdf/queue/scheduler
```

To set the scheduler to deadline, use the following command:

```bash
echo deadline > /sys/block/xvdf/queue/scheduler
```

More information on the disk scheduler is available in [Improving Performance](../).

### Virtual Memory Subsystem

EBS volumes have considerably less bandwidth than hardware disks.  To
avoid saturating EBS bandwidth and inducing IO latency spikes, it is
recommended to tune the Linux virtual memory subsystem to flush smaller
amounts of data more often. To do so, please see [Linux system performance tuning](../#optional-i-o-settings).

### Forensics

When a failure occurs, collect as much information as possible. Check
monitoring systems, back up log and configuration files if they are
available, including system logs like `dmesg` and `syslog`. Make sure
that the other nodes in the Riak cluster are still operating normally
and are not affected by a wider problem like an AWS service outage. Try
to determine the cause of the problem from the data you have collected.
If you are paying for [TI Tokyo support services](https://www.tiot.jp/en/solutions/riak/), either directly or re-sold under Erlang Solutions, and the failure comes from Riak or is not immediately obvious, you may open a ticket on the TI Tokyo Client Services help desk.

Have your collected data ready when contacting TI Tokyo Client Services. A
Client Services Engineer (CSE) might request log files, configuration
files, or other information.

## Data Loss

Many failures either do not entail data loss or have minimal loss that
can be repaired automatically, without intervention. Outage of a single
node does not necessarily cause data loss, as other replicas of every
key are available elsewhere in the cluster. Once the node is detected as
down, other nodes in the cluster will take over its responsibilities
temporarily and transmit the updated data to it when it eventually
returns to service (also called hinted handoff).

The more severe data loss scenarios usually relate to hardware failure
(in the case of AWS, service failure or instance termination). In the
cases where data is lost, several options are available for restoring
the data:

1.  Restore from backup. A daily backup of Riak nodes can be helpful.
    The data in this backup may be stale depending on the time at which
    the node failed, but can be used to partially restore data from
    lost EBS volumes. If running in a RAID configuration, rebuilding the
    array may also be possible.
2.  Restore from Multi-Datacenter Replication. If replication is enabled
    between two or more clusters, the missing data will gradually be
    restored via realtime replication and fullsync replication. A
    fullsync operation can also be triggered manually via the
    `riak-repl` command.
3.  Restore using intra-cluster repair. Riak versions 1.2 and greater
    include a "repair" feature which will restore lost partitions with
    data from other replicas. This currently has to be invoked manually
    using the Riak console and should be performed with guidance from a
    Basho CSE.

Once data has been restored, normal operations should continue. If
multiple nodes completely lose their data, consultation and assistance
from Basho is strongly recommended.

## Benchmarking

Using a tool such as [Basho Bench](https://github.com/basho/basho_bench), you can generate load that
simulates application operations by constructing and communicating
approximately-compatible data payloads with the Riak cluster directly.

Benchmarking is critical to determining the appropriate EC2 instance
types, and strongly recommended. More information is available on
benchmarking Riak clusters with [Basho Bench](../benchmarking).

Besides running Basho Bench, we also advise that you load test Riak with
your own tests to ensure that load imparted by MapReduce queries,
full-text queries, and index queries are within the expected range.

## Simulating Upgrades, Scaling, and Failure states

In addition to simply measuring performance, it is also important to
measure how performance degrades when the cluster is not in
steady-state. While under a simulation of live load, the following
states might be simulated:

1.  Stop one or more nodes normally and restart them after a few moments
    (simulates [rolling upgrade](../../../setup/upgrading/cluster)).
2.  Join two or more nodes to the cluster.
3.  Leave nodes from the cluster (after step #2).
4.  Hard-kill the Riak `beam.smp` process (i.e., `kill -9`) and then
    restart it.
5.  Hard-reboot a node's instance using the AWS console and then
    restart it.
6.  Hard-stop and destroy a node's instance and build a new one from
    backup.
7.  Via networking, e.g. firewall, partition one or more nodes from
    the rest of the cluster and then restore the original
    configuration.

## Out-of-Memory

Sometimes, Riak will exit when it runs out of available RAM. While this
does not necessarily cause data loss, it may indicate that the cluster
needs to be scaled out. While the Riak node is out, other nodes may also
be at risk if free capacity is low on the rest of the cluster, so
monitor carefully.

Replacing the EC2 instance type with one that has greater RAM capacity
may temporarily alleviate the problem, but out of memory (OOM) tends to
be an indication that the cluster is underprovisioned.

Software bugs (memory leaks) could also be a cause of OOM, so we
recommend paid support Riak users to contact TI Tokyo Client Services
if this problem occurs.

## Dealing with IP addresses

EC2 instances that are not provisioned inside a VPC can change the
following attributes after a restart:

* Private IP address
* Public IP address
* Private DNS
* Public DNS

Because these parameters play a role in a Riak instance's node name,
ensure that you follow the steps outlined in the [Node Name Changed](../../repair-recovery/failed-node/#node-name-changed) section to replace
it.

To avoid this inconvenience, you can deploy Riak inside a
[VPC](http://aws.amazon.com/vpc/). Instances inside the VPC do not
change their private IP address on restart. In addition you get the
following benefits:

* Access control lists can be defined at multiple levels
* The instance is not automatically open to the internet
* Amazon VPC is [free](http://aws.amazon.com/vpc/pricing/)

## Choice of Storage

EC2 instances support ephemeral and EBS storage. Ephemeral is local to
the instance, generally performs better, but disappears when instances
go down.

On the other hand, EBS is effectively network attached storage that
persists after instances go down. Along with EBS you can optionally
enable [Provisioned
IOPS](http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.html)
(PIOPS) provide more stable performance.

For more information on EC2 storage options, please see their
[documentation](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Storage.html).

## References

* [Improving Performance](../)
* [Failure and Recovery](../../repair-recovery)
* [Basho Client Services Help Desk](https://help.basho.com)




