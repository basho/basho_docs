---
title: Capability Negotiation
project: riak
version: 1.4.8+
document: appendix
audience: advanced
keywords: [appendix, concepts, capability]
---

In versions of Riak prior to 1.2.0, [[rolling upgrades]] from an older version of Riak to a newer involved (a) disabling all new features associated with the newer version, and then (b) re-enabling those features once all nodes in the cluster were upgraded.

This process has been simplified in versions 1.2.0. Rolling upgrades no longer require you to disable and then re-enable features, as Riak versions 1.2.0 and later now feature a **capability negotiation** subsytem that automatically manages the addition of new features. Using this subsystem, nodes negotiate with each other to automatically determine which versions are supported on which nodes, which allows clusters to maintain normal operations even when divergent versions of Riak are present in the cluster.

<div class="note">
<div class="title">Note on mixed versions</div>
The capability negotiation subsystem is used to manage mixed versions of Riak within a cluster <em>solely</em> during rolling upgrades. We strongly recommend not running mixed versions during normal operations.
</div>

## Configuration Changes

With the addition of automatic capability negotiation, there are some configuration settings that applied to versions of Riak prior to 1.2.0 that no longer need to be set if you are upgrading to a version later than 1.2.0. You can safely remove the following settings from each node's `app.config`, as they will be ignored in newer versions of Riak:

Setting | Description
:-------|:-----------
`riak_core/legacy_vnode_routing` | Uses the newer <a href="/theory/concepts/glossary/#vnode">vnode</a> routing layer when supported; otherwise defaults to the legacy routing protocol
`riak_kv/legacy_keylisting` | Uses coverage-based keylisting (introduced in Riak 1.0) when supported; otherwise defaults to the legacy keylisting behavior
`riak_kv/listkeys_backpressure` | Enables listkeys backpressure (introduced in Riak 1.1) when supported
`riak_kv/mapred_2i_pipe` | Use parallel secondary-index input to <a href="/riak/latest/dev/using/mapreduce">MapReduce</a> jobs (introduced in Riak 1.1) when supported
`riak_kv/mapred_system` | Use `riak_pipe` for <a href="/riak/latest/dev/using/mapreduce">MapReduce</a> jobs (introduced in Riak 1.0) when supported; otherwise default to the legacy `luke` system

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
