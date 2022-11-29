---
title_supertext: "Configuring:"
title: "Active Anti-Entropy"
description: ""
project: "riak_kv"
project_version: 2.9.10
menu:
  riak_kv-2.9.10:
    name: "Active Anti-Entropy (AAE)"
    identifier: "configuring-active-anti-entropy"
    weight: 210
    parent: "configuring"
toc: true
commercial_offering: false
version_history:
  in: "2.9.0p5+"
since: 2.9.0p5
aliases:
---

[config legacy]: ./legacy-aae/
[config tictac]: ./tictac-aae/
[config tictac-repl]: ../next-gen-replication/
[using aaefold]: ../../using/cluster-operations/tictac-aae-fold/
[learn aae]: ../../learn/concepts/active-anti-entropy/

Riak's [active anti-entropy][learn aae] \(AAE) subsystem is a set of background processes that repair object inconsistencies stemming from missing or divergent object values across nodes. Riak operators can turn AAE on and off and configure and monitor its functioning.

Both Legacy and TicTac AAE systems can be used seperately or together.

If you are using the legacy AAE system, it is recommended that you migrate to the TicTac AAE system.

## TicTac AAE system

The version of TicTac AAE included in 2.9 releases is a working prototype with limited testing. The intention is to full integrate the library into the KV 3.0 release.

TicTac Active Anti-Entropy makes two changes to the way Anti-Entropy has previously worked in Riak. The first change is to the way Merkle Trees are contructed so that they are built incrementally. The second change allows the underlying Anti-entropy key store to be key-ordered while still allowing faster access to keys via their Merkle tree location or the last modified date of the object.


#### [Configuring TicTac AAE][config tictac]

A guide covering commonly adjusted parameters for the TicTac AAE system.

[Learn More >>][config tictac]

#### [Configuring TicTac AAE's Next Gen Replication][config tictac-repl]

A guide covering commonly adjusted parameters for TicTac AAE's enhanced FullSync replication system.

[Learn More >>][config tictac-repl]

#### Other documentation

- [How to use `aae_fold`][using aaefold] to efficiently find, list and mangage keys.

## Legacy AAE system

The legacy AAE system is still present, and works exactly as before.

### [Configuring Legacy AAE][config legacy]

A guide covering commonly adjusted parameters for the legacy AAE system.

[Learn More >>][config legacy]

