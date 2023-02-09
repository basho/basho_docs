---
title: "Riak KV 2.9.10 Release Notes"
description: ""
project: "riak_kv"
project_version: 2.9.10
menu:
  riak_kv-2.9.10:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/2.9.10/community/release-notes
  - /riak/kv/2.9.10/intro-v20
  - /riak/2.9.10/intro-v20
  - /riak/kv/2.9.10/introduction
---

Released Oct 06, 2021.


## Overview

Fix to critical issue in leveled when using (non-default, but recommended, option): [leveled_reload_recalc = enabled](https://github.com/basho/riak_kv/blob/33add2a29b6880b680a407dc91828736f54c7911/priv/riak_kv.schema#L1156-L1174)

If using this option, it is recommended to rebuild the ledger on each vnode at some stage after updating.

## Previous Release Notes

Please see the KV 2.9.9 release notes [here]({{<baseurl>}}riak/kv/2.9.9/release-notes/).




