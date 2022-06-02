---
title: "Capability Negotiation"
description: ""
project: "riak_kv"
project_version: "2.1.1"
menu:
  riak_kv-2.1.1:
    name: "Capability Negotiation"
    identifier: "learn_concepts_cap_negot"
    weight: 102
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.1.1/theory/concepts/capability-negotiation
  - /riak/kv/2.1.1/theory/concepts/capability-negotiation
---


[glossary vnode]: {{<baseurl>}}riak/kv/2.1.1/learn/glossary/#vnode
[upgrade cluster]: {{<baseurl>}}riak/kv/2.1.1/setup/upgrading/cluster
[usage mapreduce]: {{<baseurl>}}riak/kv/2.1.1/developing/usage/mapreduce


In versions of Riak prior to 1.2.0, [rolling upgrades][upgrade cluster] from an older version of Riak to a newer involved (a) disabling all new features associated with the newer version, and then (b) re-enabling those features once all nodes in the cluster were upgraded.

This process has been simplified in versions 1.2.0. Rolling upgrades no longer require you to disable and then re-enable features, as Riak versions 1.2.0 and later now feature a **capability negotiation** subsystem that automatically manages the addition of new features. Using this subsystem, nodes negotiate with each other to automatically determine which versions are supported on which nodes, which allows clusters to maintain normal operations even when divergent versions of Riak are present in the cluster.

>**Note on mixed versions:**
>
>The capability negotiation subsystem is used to manage mixed versions of Riak within a cluster *solely* during rolling upgrades. We strongly recommend not running mixed versions during normal operations.


## Configuration Changes

With the addition of automatic capability negotiation, there are some configuration settings that applied to versions of Riak prior to 1.2.0 that no longer need to be set if you are upgrading to a version later than 1.2.0. You can safely remove the following settings from each node's `app.config`, as they will be ignored in newer versions of Riak:

Setting | Description
:-------|:-----------
`riak_core/legacy_vnode_routing` | Uses the newer [vnode][glossary vnode] routing layer when supported; otherwise defaults to the legacy routing protocol
`riak_kv/legacy_keylisting` | Uses coverage-based keylisting (introduced in Riak 1.0) when supported; otherwise defaults to the legacy keylisting behavior
`riak_kv/listkeys_backpressure` | Enables listkeys backpressure (introduced in Riak 1.1) when supported
`riak_kv/mapred_2i_pipe` | Use parallel secondary-index input to [MapReduce][usage mapreduce] jobs (introduced in Riak 1.1) when supported
`riak_kv/mapred_system` | Use `riak_pipe` for [MapReduce][usage mapreduce] jobs (introduced in Riak 1.0) when supported; otherwise default to the legacy `luke` system

Although is not recommended, you can override capability negotiation if you wish. This must be done on a per-component basis in each node's `app.config`. You can either instruct Riak not to use capability negotiation for a specific component by setting `use` to `false` as in this example, which turns off capability negotiation for the `listkeys_backpressure` setting:

```appconfig
[{override_capability,
    [{listkeys_backpressure, [{use, false}]
}]
```

The following setting would both override the `listkeys_backpressure` setting, as in the example above, _and_ override the `mapreduce_system` setting to use `legacy` if all nodes in the cluster support `legacy`. Otherwise, the built-in default setting will be used:

```appconfig
[{override_capability,
    [{listkeys_backpressure, [{use, false}]},
     {mapred_system,         [{prefer, legacy}]}]
}]
```
