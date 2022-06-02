---
title: "Improving Performance"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Performance"
    identifier: "managing_performance"
    weight: 206
    parent: "managing"
toc: true
aliases:
  - /riak/kv/2.9.1/ops/tuning/linux/
  - /riak/2.9.1/ops/tuning/linux/
---

Many Unix-like operating systems and distributions are tuned for desktop
or light use out of the box and not for a production database. This
guide describes recommended system performance tunings for operators of
new and existing Riak clusters. The tunings present in this guide should
be considered as a starting point. It is important to make note of what
changes are made and when in order to measure the impact of those
changes.

For performance and tuning recommendations specific to running Riak
clusters on the Amazon Web Services EC2 environment, see [AWS Performance Tuning]({{<baseurl>}}riak/kv/2.9.1/using/performance/amazon-web-services).

{{% note title="Note on other operating systems" %}}
Unless otherwise specified, the tunings recommended below are for Linux
distributions. Users implementing Riak on BSD and Solaris distributions can
use these tuning recommendations to make analogous changes in those operating
systems.
{{% /note %}}

## Storage and File System Tuning

### Virtual Memory

Due to the heavily I/O-focused profile of Riak, swap usage can result in
the entire server becoming unresponsive. We recommend setting
`vm.swappiness` to 0 in `/etc/sysctl.conf` to prevent swapping as much
as possible:

```config
vm.swappiness = 0
```

Ideally, you should disable swap to ensure that Riak's process pages are
not swapped. Disabling swap will allow Riak to crash in situations where
it runs out of memory. This will leave a crash dump file, named
`erl_crash.dump`, in the `/var/log/riak` directory which can be used to
determine the cause of the memory usage.

### Transparent Huge Pages (THP)

Owing to the way that THP handles memory usage, disproportionately large amounts of memory can become held up in any large database application. We recommend disabling THP at boot time. Unfortunately this operation is rather OS specific. As many of our customers are running Red Hat 6, we have included instructions on how to do so underneath. If you are using a different operating system, please refer to documentation for your OS.

In Red Hat 6, you can disable THP by editing `grub.conf` and adding the following line:

```
transparent_hugepage=never
```

For the change to become effective, a server reboot is required.

{{% note title="Note on Kernel Tuning Tools" %}}
Some Kernel tuning tools such as ktune specify that THP should be enabled. This can cause THP to seem to be enabled even though `transparent_hugepage=never` has already been added to `grub.conf` and the system rebooted. Should this occur, please refer to the documentation for the Kernel tuning tool you are using as to how to disable THP.
{{% /note %}}

### Mounts

Riak makes heavy use of disk I/O for its storage operations. It is
important that you mount volumes that Riak will be using for data
storage with the `noatime` flag, meaning that filesystem
[inodes](http://en.wikipedia.org/wiki/Inode) on the volume will not be
touched when read. This flag can be set temporarily using the following
command:


```bash
mount -o remount,noatime <riak_data_volume>
```

Replace `<riak_data_volume>` in the above example with your actual Riak
data volume. The `noatime` can be set in `/etc/fstab` to mount
permanently.

### Schedulers

I/O or disk scheduling is a blanket term used to describe the method by
which an operating system chooses how to order input and output
operations to and from storage.

The default I/O scheduler (elevator) on Linux is completely fair queuing
or `cfq`, which is designed for desktop use. While a good
general-purpose scheduler, is not designed to provide the kind of
throughput expected in production database deployments.

Scheduler recommendations:

* The `noop` scheduler when deploying on iSCSI over HBAs, or any
  hardware-based RAID.
* The `deadline` scheduler when using SSD-based storage.

To check the scheduler in use for block device `sda`, for example, use
the following command:

```bash
cat /sys/block/sda/queue/scheduler
```

To set the scheduler to `deadline`, use the following command:

```bash
echo deadline > /sys/block/sda/queue/scheduler
```

The default I/O scheduler queue size is 128. The scheduler queue sorts
writes in an attempt to optimize for sequential I/O and reduce seek
time. Changing the depth of the scheduler queue to 1024 can increase the
proportion of sequential I/O that disks perform and improve overall
throughput.

To check the scheduler depth for block device `sda`, use the following
command:

```bash
cat /sys/block/sda/queue/nr_requests
```

To increase the scheduler depth to 1024, use the following command:

```bash
echo 1024 > /sys/block/sda/queue/nr_requests
```

### Filesystem

Advanced journaling filesystems like [ZFS](http://zfsonlinux.org/) and
[XFS](http://xfs.org/index.php/Main_Page) are recommended on some
operating systems for greater reliability and recoverability.

At this time, Basho can recommend using ZFS on Solaris, SmartOS, and
OmniOS. ZFS may work well with Riak on direct Solaris clones like
IllumOS, but we cannot yet recommend this. [ZFS on
Linux](http://zfsonlinux.org) is still too early in its project lifetime
to be recommendable for production use due to concerns that have been
raised about excessive memory use. ZFS on FreeBSD is more mature than
ZFS on Linux, but Basho has not yet performed sufficient performance and
reliability testing to recommend using ZFS and Riak on FreeBSD.

In the meantime, the [ext3](http://en.wikipedia.org/wiki/Ext3) and
[ext4](http://en.wikipedia.org/wiki/Ext4) filesystems are sufficient on
operating systems on which ZFS or XFS are not available or recommended.

The ext4 file system defaults include two options that increase
integrity but slow performance. Because Riak's integrity is based on
multiple nodes holding the same data, these two options can be changed
to boost I/O performance. We recommend setting `barrier=0` and
`data=writeback` when using the ext4 filesystem.

Similarly, the XFS file system defaults can be optimized to improve
performance.  We recommend setting `nobarrier`, `logbufs=8`,
`logbsize=256k`, and `allocsize=2M` when using the XFS filesystem.

As with the `noatime` setting, these settings should be added to
`/etc/fstab` so that they are persisted across server restarts.

## Kernel and Network Tuning

The following settings are minimally sufficient to improve many aspects
of Riak usage on Linux, and should be added or updated in
`/etc/sysctl.conf`:

```config
net.ipv4.tcp_max_syn_backlog = 40000
net.core.somaxconn = 40000
net.core.wmem_default = 8388608
net.core.rmem_default = 8388608
net.ipv4.tcp_sack = 1
net.ipv4.tcp_window_scaling = 1
net.ipv4.tcp_fin_timeout = 15
net.ipv4.tcp_keepalive_intvl = 30
net.ipv4.tcp_tw_reuse = 1
net.ipv4.tcp_moderate_rcvbuf = 1
```

{{% note title="Note on system default" %}}
In general, these recommended values should be compared with the system
defaults and only changed if benchmarks or other performance metrics indicate
that networking is the bottleneck.
{{% /note %}}

The following settings are optional, but may improve performance on a
10Gb network:

```config
net.core.rmem_max = 134217728
net.core.wmem_max = 134217728
net.ipv4.tcp_mem  = 134217728 134217728 134217728
net.ipv4.tcp_rmem = 4096 277750 134217728
net.ipv4.tcp_wmem = 4096 277750 134217728
net.core.netdev_max_backlog = 300000
```

Certain network interfaces ship with on-board features that have been
shown to hinder Riak network performance. These features can be disabled
via `ethtool`.

For an Intel chipset NIC using the
[ixgbe](http://www.intel.com/support/network/adapter/pro100/sb/CS-032530.htm)
driver running as `eth0`, for example, run the following command:

```bash
ethtool -K eth0 lro off
```

For a Broadcom chipset NIC using the `bnx` or `bnx2` driver, run:

```bash
ethtool -K eth0 tso off
```

`ethtool` settings can be persisted across reboots by adding the above
command to the `/etc/rc.local` script.

{{% note title="Pro tip" %}}
Tuning these values will be required if they are changed, as they affect all
network operations.
{{% /note %}}

## Optional I/O Settings

If your cluster is experiencing excessive I/O blocking, the following
settings may help prevent disks from being overwhelmed during periods of
high write activity at the expense of peak performance for spiky
workloads:

```config
vm.dirty_background_ratio = 0
vm.dirty_background_bytes = 209715200
vm.dirty_ratio = 40
vm.dirty_bytes = 0
vm.dirty_writeback_centisecs = 100
vm.dirty_expire_centisecs = 200
```

These settings have been tested and benchmarked by Basho in nodes with
16 GB of RAM.

## Open Files Limit

Riak and supporting tools can consume a large number of open file
handles during normal operation. For stability, increasing the number of
open files limit is necessary. See [Open Files Limit]({{<baseurl>}}riak/kv/2.9.1/using/performance/open-files-limit/) for more
details.

## Other Tuning Docs

* [AWS Performance Tuning]({{<baseurl>}}riak/kv/2.9.1/using/performance/amazon-web-services)
* [Erlang VM Tuning]({{<baseurl>}}riak/kv/2.9.1/using/performance/erlang)
* [Latency Reduction]({{<baseurl>}}riak/kv/2.9.1/using/performance/latency-reduction)
* [Open Files Limit]({{<baseurl>}}riak/kv/2.9.1/using/performance/open-files-limit/)
