---
title: Planning for a Riak System
project: riak
version: 0.10.0+
document: tutorials
toc: true
audience: intermediate
keywords: [planning, os]
moved: {
    '1.4.0-': '/tutorials/System-Planning'
}
---

Here are some steps and recommendations designing and configuring your
Riak cluster.

## Backend

Backends are what Riak uses to persist data. Different backends have
strengths and weaknesses, so if you are unsure of which backend you
need, read through the [[Choosing a Backend]] tutorial.

* [[Bitcask]]
* [[LevelDB]]
* [[Memory]]
* [[Multi]]

## Capacity

[[Cluster Capacity Planning]] outlines the various elements and
variables that should be considered when planning your Riak cluster.

If you have chosen [[Bitcask]] as your backend, you will also want to
run through [[Bitcask Capacity Planning]] to help you calculate a
reasonable capacity.

## Operating Systems

We recommend deploying Riak on a mainstream Unix-like operating system.
Mainstream distributions have larger support communities, making
solutions to common problems easier to find. Basho provides binary
packages of Riak for the following distributions:

* **Red Hat based:** Red Hat Enterprise Linux, CentOS, Fedora Core
* **Debian based:** Debian, Ubuntu
* **Solaris based:** Sun Solaris, OpenSolaris

## Software

If you use [[Basho's Riak packages|http://downloads.basho.com/riak/]],
there is no need for additional software packages. If you build Riak
from source, you need to have Erlang installed on your systems. See
[[Installing Erlang]] for instruction on building and installing Erlang.

## Hardware

Riak is designed to scale horizontally---i.e. to improve performance as
you add nodes---but it can always take advantage of more powerful
hardware. The following are some general hardware recommendations:

* **Multi-core 64-bit CPU** --- Because Riak is built on Erlang, more
  cores means more concurrency and thus greater performance. Riak also
  performs certain numerical computations more efficiently on 64-bit
  architectures.
* **Minimum 4 GB RAM** --- More RAM means that more data can be held in
  main memory, resulting in better read, write, and [[MapReduce|Using
  MapReduce]] performance. Insufficient RAM will increase swap
  utilization, causing performance degradation as memory operations
  begin to contend with normal disk operations. You can use tools such
  as our [[Bitcask calculator|Bitcask Capacity Planning]] to calculate
  how much memory your nodes need to fit your dataset into Bitcask. Be
  sure to read [[Cluster Capacity Planning]] for more information on
  memory and disk usage. **Note**: If you plan on using [[Riak
  Search|Using Search]], which relies on Solr (and thus a JVM
  installation), we recommend a minimum of 6 GB of RAM.
* **Multiple Fast Hard Disks (RAID and/or SSD)** --- Because many
  operations in Riak are I/O bound, it is important to have fast hard
  disks to achieve good performance. Configuring disks RAID0 for
  increased read/write performance may be helpful as well.
* **Fast Network (Gigabit +)** --- Riak uses the network heavily for
  storage operations and for cluster status (ring-state gossip, handoff,
  etc). Fast connections between nodes and between clients and the
  cluster will improve performance.

## Virtualization

Like most datastores, **Riak will run best when not virtualized**.
Virtual machines (VMs) can suffer from poor I/O and network performance,
depending on how they are configured and the environment in which they
run.  That said, here are some recommendations for running Riak in VPS
or cloud environments:

* **Choose the Largest VM You can Afford** --- Better hardware means
  better performance. Larger virtual machines are less likely to share
  hardware resources with other customers' virtual machines.
* **Deploy VMs Within the Same Datacenter or Region Where Possible** ---
  Some hosting providers allow you to choose the location of your
  servers. Choosing to provision within the same datacenter or region
  will usually reduce network latency and increase throughput, resulting
  in greater performance.

## Network Configuration / Load Balancing

There are at least two acceptable strategies for load-balancing requests
across your Riak cluster: **virtual IPs** and **reverse-proxy**.

For **virtual IPs**, we recommend using any of the various VIP
implementations. We don't recommend VRRP behavior for the VIP because
you'll lose the benefit of spreading client query load to all nodes in a
ring.

For **reverse-proxy** configurations (HTTP interface), any one of the
following should work adequately:

* haproxy
* squid
* varnish
* nginx
* lighttpd
* Apache
