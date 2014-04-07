---
title: Capability Negotiation
project: riak
version: 1.2.0+
document: appendix
audience: advanced
keywords: [appendix, concepts, capability]
---

In versions of Riak prior to 1.2.0, [[rolling upgrades]] involved disabling new features during the upgrade and then re-enabling those features once all of the nodes were upgraded.

This process has been simplified in versions 1.2.0 and later with the addition of a **capability negotiation** subsytem in Riak that automatically manages the addition of new features. Using this subsystem, nodes negotiate with each other to determine which versions are supported on which nodes, which allows clusters to maintain normal operations even when divergent versions of Riak are present in the cluster.