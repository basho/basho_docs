---
tile_supertext: "Configuring:"
title: "Next Gen Replication"
description: ""
project: "riak_kv"
project_version: "2.9.1"
menu:
  riak_kv-2.9.1:
    name: "Next Gen Replication"
    identifier: "nextgen_rep"
    weight: 200
    parent: "configuring"
version_history:
  in: "2.9.1+"
toc: true
commercial_offering: true
---

The configuration for Next Gen Replication is kept in
 the `riak.conf` configuration file. 

## Settings

Once your configuration is set, you can verify its correctness by
running the `riak` command-line tool:

```bash
riak chkconfig
```

## riak.conf Settings

Setting | Options | Default | Description
:-------|:--------|:--------|:-----------
`ttaaefs_scope` | `{disabled, all, bucket, type}` | **REQUIRED** | For Tictac full-sync does all data need to be sync'd, or should a specific bucket be sync'd (bucket), or a specific bucket type (type).Note that in most cases sync of all data is lower overhead than sync of a subset of data - as cached AAE trees will be used.
`ttaaefs_queuename` | `text` | `q1_ttaaefs` | For tictac full-sync what registered queue name on this cluster should be use for passing references to data which needs to be replicated for AAE full-sync. This queue name must be defined as a `riak_kv.replq<n>_queuename`, but need not be exlusive to full-sync (i.e. a real-time replication queue may be used as well).
`ttaaefs_maxresults` | `any` (integer) | `64` | or tictac full-sync what is the maximum number of AAE segments to be compared per exchange. Reducing this will speed up clock compare queries, but will increase the number of exchanges required to complete a repair.
`ttaaefs_rangeboost` | `any` (integer) | `8` | For tictac full-sync what is the maximum number of AAE segments to be compared per exchange. When running a range_check query this will be the ttaaefs_max results * ttaaefs_rangeboost.
`ttaaefs_bucketfilter_name` | `any`, (text)| `` | For Tictac bucket full-sync which bucket should be sync'd by this node. Only ascii string bucket definitions supported (which will be converted using list_to_binary).
`ttaaefs_bucketfilter_type` | `any` (text) | `default` | For Tictac bucket full-sync what is the bucket type of the bucket name. Only ascii string type bucket definitions supported (these definitions will be converted to binary using list_to_binary)
`ttaaefs_localnval` | `any` (integer) | `3` | For Tictac all full-sync which NVAL should be sync'd by this node. This is the `local` nval, as the data in the remote cluster may have an alternative nval.
`ttaaefs_remotenval` | `any` (integer) | `3` | For Tictac all full-sync which NVAL should be sync'd in the remote cluster.
`ttaaefs_peerip` | `127.0.0.1` (text) | `` | The network address of the peer node in the cluster with which this node will connect to for full_sync purposes. If this peer node is unavailable, then this local node will not perform any full-sync actions, so alternative peer addresses should be configured in other nodes.
`ttaaefs_peerport` | `8898` (integer) | `` | The port to be used when connecting to the remote peer cluster.
`ttaaefs_peerprotocol` | `http`, `pb` | `http` | The protocol to be used when conecting to the peer in the remote cluster. Could be http or pb (but only http currently being tested).
`ttaaefs_allcheck` | `any` (integer) | `24` | How many times per 24hour period should all the data be checked to confirm it is fully sync'd. When running a full (i.e. nval) sync this will check all the data under that nval between the clusters, and when the trees are out of alignment, will check across all data where the nval matches the specified nval.
`ttaaefs_nocheck` | `any` (integer) | `0` | How many times per 24hour period should no data be checked to confirm it is fully sync'd. Use nochecks to align the number of checks done by each node - if each node has the same number of slots, they will naurally space their checks within the period of the slot.
`ttaaefs_hourcheck` | `any` (integer) | `0` | How many times per 24hour period should the last hours data be checked to confirm it is fully sync'd.
`ttaaefs_daycheck` | `any` (integer) | `0` | How many times per 24hour period should the last 24-hours of data be checked to confirm it is fully sync'd.
`ttaaefs_rangecheck` | `any` (integer) | `0` | How many times per 24hour period should the a range_check be run.
`ttaaefs_logrepairs` | `enabled`, `disabled` | `enabled` | If Tictac AAE full-sync discovers keys to be repaired, should each key that is repaired be logged
`tictacaae_active` | `active`, `passive` | `passive` | Enable or disable tictacaae. Note that disabling tictacaae will set the use of tictacaae_active only at startup - setting the environment variable at runtime will have no impact.
`aae_tokenbucket` | `enabled`, `disabled` | `enabled` | To protect against unbounded queues developing and subsequent timeouts/crashes of the AAE process, back-pressure signalling is used to block the vnode should a backlog develop on the AAE process. This can be disabled.
`tictacaae_dataroot` | `` | `"$platform_data_dir/tictac_aae"` | Set the path for storing tree caches and parallel key stores. Note that at startup folders may be created for every partition, and not removed when that partition hands off (although the contents should be cleared).
`tictacaae_parallelstore` | `leveled_ko`, `leveled_so` | `leveled_so` | On startup, if tictacaae is enabled, then the vnode will detect of the vnode backend has the capability to be a "native" store. If not, then parallel mode will be entered, and a parallel AAE keystore will be started. There are two potential parallel store backends - leveled_ko, and leveled_so.
`tictacaae_rebuildwait` | `` | `336` | This is the number of hours between rebuilds of the Tictac AAE system for each vnode. A rebuild will invoke a rebuild of the key store (which is a null operation when in native mode), and then a rebuild of the tree cache from the rebuilt store.
`tictacaae_rebuilddelay` | `` | `345600` | Once the AAE system has expired (due to the rebuild wait), the rebuild will not be triggered until the rebuild delay which will be a random number up to the size of this delay (in seconds).
`tictacaae_storeheads` | `enabled`, `disabled` | `disabled` | By default when running a parallel keystore, only a small amount of metadata is required for AAE purposes, and with store heads disabled only that small amount of metadata is stored.
`tictacaae_exchangetick` | `` | `240000` | Exchanges are prompted every exchange tick, on each vnode. By default there is a tick every 4 minutes. Exchanges will skip when previous exchanges have not completed, in order to prevent a backlog of fetch-clock scans developing.
`tictacaae_rebuildtick` | `` | `3600000` | Rebuilds will be triggered depending on the riak_kv.tictacaae_rebuildwait, but they must also be prompted by a tick. The tick size can be modified at run-time by setting the environment variable via riak attach.
`tictacaae_maxresults` | `` | `256` | The Merkle tree used has 4096 * 1024 leaves. When a large discrepancy is discovered, only part of the discrepancy will be resolved each exchange - active anti-entropy is intended to be a background process for repairing long-term loss of data, hinted handoff and read-repair are the short-term and immediate answers to entropy. How much of the tree is repaired each pass is defined by the tictacaae_maxresults.