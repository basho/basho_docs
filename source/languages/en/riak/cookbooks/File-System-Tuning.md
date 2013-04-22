---
title: File System Tuning
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator, os]
---

The following article suggests I/O Scheduler configurations to use when deploying Riak.

I/O or Disk Scheduling is a blanket term used to describe the method
by which an operating system chooses how to order input and output operations to and 
from storage.

There are many I/O Scheduling techniques. Some of the most common are:

* Anticipatory
* CFQ, or Completely Fair Queuing, the default in Linux since 2006
* Deadline
* FIFO
* NOOP

CFQ, while a good general purpose scheduler, is not designed to provide the kind
of throughput expected in production database deployments. For Riak, NOOP is 
considered the best choice when deploying on iSCSI over HBAs, or any hardware-based 
RAID. The Deadline scheduler is an ideal choice when using SSD based storage.

All system/workload combinations are different. Consult your operating system's
documentation for specific information about the I/O Scheduling options available to you
and the methods necessary for implementation.
