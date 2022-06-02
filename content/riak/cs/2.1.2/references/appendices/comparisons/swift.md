---
title: "Riak CS Compared To Swift"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Riak CS Compared To Swift"
    identifier: "compare_swift"
    weight: 100
    parent: "theory_comparisons"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/appendices/comparisons/Riak-Compared-to-Swift/
  - /riak/cs/2.1.2/references/appendices/comparisons/Riak-Compared-to-Swift/
  - /riak/cs/latest/references/appendices/comparisons/swift/
---

Riak CS and Swift---the object storage component of OpenStack---are both cloud storage systems that hold many design and implementation details in common. The purpose of this document is not to serve as an introduction to Riak CS and Swift or to their commonalities, but rather to enumerate important differences between the two systems. This document is intended for those who already have a basic understanding of both systems.

If you feel that this comparison is unfair to either of these technologies, please [submit an issue on GitHub](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.


## Feature/Capability Comparison

The table below gives a high-level comparison of Riak CS and Swift features and capabilities.  For low-level details, please refer to the Riak CS and [Swift docs](http://docs.openstack.org/developer/swift/).

<table>
    <tr>
        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">Swift</th>
    </tr>
    <tr>
        <td>Anti-Entropy</td>
        <td>Riak CS supports Active Anti-Entropy, which monitors and repairs inconsistencies between divergent replicas. Riak CS also supports "passive" read-time anti-entropy, which provides repair of inconsistencies immediately at client-read time. Swift does not perform repair at read or write time, but rather resolves such issues during its next rsync cycle.</td>
        <td>Swift has a continuous anti-entropy process via frequent invocation of "rsync" for repairing any inconsistencies between data node file systems.</td>
    </tr>
    <tr>
        <td>Write-Time Communication &amp; Host Failures</td>
        <td>Riak CS always writes to the full number of desired hosts, using fallback nodes to perform hinted handoff and stand in for any missing or failing hosts in order to immediately reach full redundancy. As soon as the primary Riak CS nodes are once again reachable, copies on the fallbacks will be sent to them, quickly repairing the state of the cluster.</td>
        <td>Swift will write at least a majority/quorum of replicas before declaring success, and will allow anti-entropy to bring the number of replicas up to the full count later if needed due to node failures.</td>
    </tr>
    <tr>
        <td>Quorum Models</td>
        <td>Riak CS’s underlying quorum model is not only about availability, it also provides a latency-smoothing effect by replying to the user without the need to block on the slowest host in the replication set. This prevents brief per-host performance problems from affecting end-users.</td>
        <td>Swift, despite replying only with the "best" single response, will wait for all relevant storage nodes to finish before sending a response to a write request. This can adversely impact latency. However, Swift's read requests do not wait for a quorum; they simply try one replica at a time at random until they get a response with a fairly short timeout before moving on to try another. There are plans to improve the latency of Swift’s write requests.</td>
    </tr>
    <tr>
        <td>Full-Stack Integration</td>
        <td>Riak CS stands alone as a storage service that has no specific related services for compute, VM image management, etc.</td>
        <td>Though it can run on its own, Swift is part of the OpenStack project, a highly regarded and well-defined "stack" of services.</td>
    </tr>
  <tr>
        <td>Languages</td>
        <td>Riak CS is written in Erlang, a language and platform engineered for extremely high availability, making it easier to build Riak CS on industry-tested distributed systems components, and to attract engineers that specialize in such systems.</td>
    <td>Swift is written in Python, a language with a very large, accessible developer community who could readily contribute to Swift without the need to learn a new language.</td>
    </tr>
  </tr>
      <td>Installation</td>
      <td>Riak CS is designed for easy installation, with a relatively small number of independent components to manage. A minimal installation requires installing just three components and editing fewer than 10 lines of configuration data.</td>
        <td>Swift’s "toolbox" approach requires the installation and ongoing operational supervision of various components, including Memcached, SQLite, and Keystone (the OpenStack authentication server), each of which have deep dependency trees of their own. An upside of this approach is that the system’s overall behavior is extremely modifiable by changing the behavior of any of the many dependencies.</td>
  </tr>
    <tr>
        <td>Operations</td>
        <td>With Riak CS, a single administrative command on a newly provisioned host tells the system to automatically integrate the new device. Well-defined underlying system components ensure correct behavior during transitions.</td>
        <td>Swift requires a high degree of manual management. Devices are added to the definition of the ring by defining their node, name and zone. To change the definitions, mapping must be regenerated and new definitions must be pushed out to every node with whichever means is available (rsync appears to be the most common). When these files fall out of sync, the system will experience strange behavior or cease to function altogether.</td>
    </tr>
    <tr>
        <td>Support For Amazon S3 API</td>
        <td>Riak CS directly and natively supports the widely adopted S3 API, including such commonly used aspects as S3-keyed ACLs, hostname-to-bucket translation, etc.</td>
        <td>Swift has its own custom (non-S3) API with its own strengths. Optional, externally developed middleware that emulates the S3 API on top of Swift is, however, available.</td>
    </tr>
    <tr>
        <td>Governance</td>
    <td>Riak CS is open source and is managed by Basho. It is available under the Apache 2 License.</td>      
        <td>Swift is entirely open source and is managed by the OpenStack Foundation. No license is required in any way and no single company can either block or cause any changes to it on their own.</td>
    </tr>
    
</table>
