---
title_supertext: "Configuring:"
title: "TicTac Active Anti-Entropy"
description: ""
project: "riak_kv"
project_version: 3.0.12
menu:
  riak_kv-3.0.12:
    name: "TicTac AAE"
    identifier: "configuring_tictac_aae"
    weight: 101
    parent: "configuring-active-anti-entropy"
toc: true
version_history:
  in: "2.9.0p5+"
since: 2.9.0p5
aliases:
---

[configure next-gen-repl]: ../../next-gen-replication

The configuration for TicTac AAE is kept in
 the `riak.conf` configuration file.

## Validate Settings

Once your configuration is set, you can verify its correctness by
running the `riak` command-line tool:

```bash
riak chkconfig
```

## riak.conf Settings

Setting | Options | Default | Description
:-------|:--------|:--------|:-----------
`tictacaae_active` | `active`, `passive` | `passive` | Enable or disable tictacaae. Note that disabling tictacaae will set the use of tictacaae_active only at startup - setting the environment variable at runtime will have no impact.
`aae_tokenbucket` | `enabled`, `disabled` | `enabled` | To protect against unbounded queues developing and subsequent timeouts/crashes of the AAE process, back-pressure signalling is used to block the vnode should a backlog develop on the AAE process. This can be disabled.
`tictacaae_dataroot` | `` | `"$platform_data_dir/tictac_aae"` | Set the path for storing tree caches and parallel key stores. Note that at startup folders may be created for every partition, and not removed when that partition hands off (although the contents should be cleared).
`tictacaae_parallelstore` | `leveled_ko`, `leveled_so` | `leveled_so` | On startup, if tictacaae is enabled, then the vnode will detect of the vnode backend has the capability to be a "native" store. If not, then parallel mode will be entered, and a parallel AAE keystore will be started. There are two potential parallel store backends - leveled_ko, and leveled_so.
`tictacaae_rebuildwait` | `` | `336` | This is the number of hours between rebuilds of the Tictac AAE system for each vnode. A rebuild will invoke a rebuild of the key store (which is a null operation when in native mode), and then a rebuild of the tree cache from the rebuilt store.
`tictacaae_rebuilddelay` | `` | `345600` | Once the AAE system has expired (due to the rebuild wait), the rebuild will not be triggered until the rebuild delay which will be a random number up to the size of this delay (in seconds).
`tictacaae_storeheads` | `enabled`, `disabled` | `disabled` | By default only a small amount of metadata is required for AAE purposes, and with storeheads disabled only that small amount of metadata is stored. Enabling storeheads will allow for greater functionality (notably with [`aae_fold`](../../../using/cluster-operations/tictac-aae-fold/)) at the cost of disk space and memory.
`tictacaae_exchangetick` | `` | `240000` | Exchanges are prompted every exchange tick, on each vnode. By default there is a tick every 4 minutes. Exchanges will skip when previous exchanges have not completed, in order to prevent a backlog of fetch-clock scans developing.
`tictacaae_rebuildtick` | `` | `3600000` | Rebuilds will be triggered depending on the riak_kv.tictacaae_rebuildwait, but they must also be prompted by a tick. The tick size can be modified at run-time by setting the environment variable via riak attach.
`tictacaae_maxresults` | `` | `256` | The Merkle tree used has 4096 * 1024 leaves. When a large discrepancy is discovered, only part of the discrepancy will be resolved each exchange - active anti-entropy is intended to be a background process for repairing long-term loss of data, hinted handoff and read-repair are the short-term and immediate answers to entropy. How much of the tree is repaired each pass is defined by the tictacaae_maxresults.

## See also

[Next Gen Replication][configure next-gen-repl] makes extensive use of TicTac AAE, and has some replication-specific TicTac AAE settings.