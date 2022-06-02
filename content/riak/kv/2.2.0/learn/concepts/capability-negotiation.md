---
title: "Capability Negotiation"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Capability Negotiation"
    identifier: "learn_concepts_cap_negot"
    weight: 102
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.2.0/theory/concepts/capability-negotiation
  - /riak/kv/2.2.0/theory/concepts/capability-negotiation
---


[glossary vnode]: {{<baseurl>}}riak/kv/2.2.0/learn/glossary/#vnode
[upgrade cluster]: {{<baseurl>}}riak/kv/2.2.0/setup/upgrading/cluster
[usage mapreduce]: {{<baseurl>}}riak/kv/2.2.0/developing/usage/mapreduce


In early versions of Riak KV, [rolling upgrades][upgrade cluster] from an older version to a newer involved (a) disabling all new features associated with the newer version, and then (b) re-enabling those features once all nodes in the cluster were upgraded.

Rolling upgrades no longer require you to disable and then re-enable features due to the *capability negotiation* subsystem that automatically manages the addition of new features. Using this subsystem, nodes negotiate with each other to automatically determine which versions are supported on which nodes, which allows clusters to maintain normal operations even when divergent versions of Riak KV are present in the cluster.

{{% note title="Note on Mixed Versions" %}}
The capability negotiation subsystem is used to manage mixed versions of Riak KV within a cluster ONLY during rolling upgrades. We strongly recommend not running mixed versions during normal operations.
{{% /note %}}


