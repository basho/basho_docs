---
title: AWS Performance Tuning
project: riak
version: 1.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator, performance, aws]
---

This guide introduces some recommended best practices for performance and
tuning of Riak clusters in the Amazon Web Services (AWS) Elastic Compute
Cloud (EC2) environment.

<div class="info"><div class="title">Tip</div>Be sure to also see
[[Linux Performance Tuning]] for detailed performance and tuning
recommendations of a more general sort, which apply to Riak cluster
installations.</div>

## EC2 Instances
EC2 instances are available as predefined types which encapsulate a fixed
amount of computing resources, the most important of which to Riak are Disk
I/O, RAM, and Network I/O, followed by CPU cores. With this in mind, Riak
users have reported success with large, extra large, and cluster compute
instance types for use as cluster nodes in the AWS EC2 environment.

The most commonly used [instance types](http://aws.amazon.com/ec2/instance-types/)
for Riak cluster nodes are the
`m1.large` or `m1.xlarge`. In cases where 10 gigabit Ethernet networking is
desired, the Cluster Compute class of EC2 instances, such as `cc1.4xlarge`
or `cc2.8xlarge` can be used.

Amazon also offers a High I/O Quadruple Extra Large instance
(`hi1.4xlarge`) that is backed by solid state drives (SSD) and features
very high I/O performance.

EBS-Optimized EC2 instances, which provide between 500 Megabits per
second and 1,000 Megabits per second of throughput with [Provisioned IOPS](http://aws.amazon.com/about-aws/whats-new/2012/07/31/announcing-provisioned-iops-for-amazon-ebs/)
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

<div class="info"><div class="title">Tip</div>Most successful AWS cluster deployments use more EC2 instances than they
would the same number of physical nodes to compensate for the
performance variability caused by shared, virtualized resources. Plan to
have more EC2 instance based nodes than physical server nodes when estimating
cluster size with respect to node count.</div>

## Operating System

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

More information on the disk scheduler is available in [[Linux Performance Tuning]]
and [[File System Tuning]].

### Forensics
When a failure occurs, collect as much information as possible. Check
monitoring systems, backup log and configuration files if they are
available, including system logs like `dmesg` and syslog. Make sure that
the other nodes in the Riak cluster are still operating normally and are
not affected by a wider problem like an AWS service outage. Try to
determine the cause of the problem from the data you have collected. If you
are a licensed Riak Enterprise Edition user and the failure comes from Riak
or is not immediately obvious, you may open a ticket on the Basho Client
Services  help desk or contact the 24/7 emergency line.

Have your collected data ready when contacting Basho Client Services;
a Client Services Engineer (CSE) might request log files, configuration
files, or other information.

## Data Loss
Many failures do not incur data loss, or have minimal loss that can be
repaired automatically, without intervention. Outage of a single node
does not necessarily cause data loss, as other replicas of every key are
available elsewhere in the cluster. Once the node is detected as down,
other nodes in the cluster will take over its responsibilities
temporarily, and transmit the updated data to it when it eventually
returns to service (also called hinted handoff).

The more severe data loss scenarios usually relate to hardware failure
(in the case of AWS, service failure or instance termination). In the
cases where data is lost, several options are available for restoring
the data.

1.  Restore from backup. A daily backup of Riak nodes can be helpful.
    The data in this backup may be stale depending on the time  at which
    the node failed, but can be used  to partially-restore data from
    lost EBS volumes. If running in a RAID configuration, rebuilding the
    array may also be possible.
2.  Restore from multi-cluster replication. If replication is enabled
    between two or more clusters, the missing data will gradually be
    restored via streaming replication and full-synchronization. A
    full-synchronization can also be triggered manually via the
    riak-repl command.
3.  Restore using intra-cluster repair. Riak versions 1.2 and greater
    include a "repair" feature which will restore lost partitions with
    data from other replicas. This currently has to be invoked manually
    using the Riak console and should be performed with guidance from a
    Basho CSE.

Once data has been restored, normal operations should continue. If
multiple nodes completely lose their data, consultation and assistance
from Basho is strongly recommended.

## Benchmarking
Using a tool such as [Basho Bench](https://github.com/basho/basho_bench),
you can generate load that simulates application operations by constructing
and communicating approximately-compatible data payloads with the Riak
cluster directly.

Benchmarking is critical to determining the appropriate EC2 instance
types, and strongly recommended. More information is available on
benchmarking Riak clusters with Basho Bench in the
[[Basho Bench documentation|Benchmarking]].

Besides running basho bench it is also advisable to load test Riak with your 
own tests to ensure that load imparted by M/R queries, linking, link-walking,
 full-text queries, index queries are within the expected range.

## Simulating Upgrades, Scaling, and Failure states
In addition to simply measuring performance, it is also important to
measure how performance degrades when the cluster is not in
steady-state. While under a simulation of live load, the following
states might be simulated:

1.  Stop one or more nodes normally and restart them after a few moments
    (simulates rolling-upgrade).
2.  Join two or more nodes to the cluster.
3.  Leave nodes from the cluster (after step #2).
4.  Hard-kill the Riak **beam.smp** process (i.e., `kill -9`) and then
    restart it.
5.  Hard-reboot an node's instance using the AWS console and then
    restart it.
6.  Hard-stop and destroy a node's instance and build a new one from
    backup.
7.  Via networking (firewall, perhaps), partition one or more nodes from
    the rest of the cluster, and then restore the original
    configuration.

## Out-of-Memory
Sometimes Riak will exit when it runs out of available RAM. While this
does not necessarily cause data loss, it may indicate that the cluster
needs to be scaled out. While the Riak node is out, if free capacity is
low on the rest of the cluster, other nodes may also be at risk, so
monitor carefully.

Replacing the EC2 instance type with one that has greater RAM capacity
may temporarily alleviate the problem, but out of memory (OOM) tends to
be an indication that the cluster is under-provisioned.

Software bugs (memory leaks) could also be a cause of OOM, so we ask that
Riak Enterprise Edition users please contact Basho Client Services if
this problem occurs.

## Dealing with IP addresses

EC2 instances that are not provisioned inside a VPC change the following parameters after a restart.

* Private IP address
* Public IP address
* Private DNS
* Public DNS

Since Riak binds to an IP addresses and communicates with other nodes based on this address, 
executing certain admin commands are necessary to bring the node back up. 
Namely the following steps must be performed.

* Stop the node to rename with `riak stop`
* Mark it 'down' from another node in the cluster using `riak-admin down 'old nodename'`.
* Rename the node in vm.args.
* Delete the ring directory.
* Start the node with `riak start`.  
* It will come up as a single instance which you can verify with `riak-admin member-status`.
* Join the node to the cluster with `riak-admin cluster join 'cluster nodename' `
* Set it to replace the old instance of itself with `riak-admin cluster replace <old nodename> <new nodename>
* Plan the changes with `riak-admin cluster plan`
* Commit the changes with `riak-admin cluster commit`

To avoid this inconvenience, you can deploy riak to a [VPC](http://aws.amazon.com/vpc/). Instances inside the VPC do not change their private 
IP address on restart. In addition you get the following benefits.

* Access control lists can be defined at various levels (Load balancers / Individual servers / VPC Groups).
* The Riak instance is not open to arbitrary communication from the internet. Only nodes within a subnet can contact Riak.
* Should the private nodes need to contact the internet they can do so through a NAT instance.
* Amazon VPC is [free](http://aws.amazon.com/vpc/pricing/).

You can also explore other [solutions](http://deepakbala.me/2013/02/08/deploying-riak-on-ec2/) should setting up a VPC 
present an obstacle for you. 

## Choice of storage

EC2 instances support ephemeral and EBS storage. There are [several posts](http://riak-users.197444.n3.nabble.com/EC2-and-RIAK-td2754409.html) on the riak-users list that discuss
the pros and cons of different approaches. These posts are good starting points to help you make a decision on the type of storage you need.

## References
* [[Linux Performance Tuning]]
* [[Failure and Recovery]]
* [[File System Tuning]]
* [Basho Client Services Help Desk](https://help.basho.com)
