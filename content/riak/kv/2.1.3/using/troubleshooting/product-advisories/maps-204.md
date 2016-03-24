---
title: "Map Data Type Disk Incompatibility"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Map Data Type Disk Incompatibility"
    identifier: "product_advisories_maps_204"
    weight: 103
    parent: "troubleshooting_product_advisories"
toc: true
---

Info | Value
:----|:-----
Date issued | January 21, 2015
Product | Riak
Affected versions | Riak 2.0.4
Symptoms | Request timeouts, inability to access data in Riak maps
Cause | Change in the on-disk format of Riak maps

## Overview

On January 20th, 2015, a user
[reported](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2015-January/016568.html)
issues with [Riak Data Types](/riak/kv/2.1.3/developing/data-types) upon upgrading from Riak
2.0.2 to 2.0.4. It was discovered that keys storing [Riak maps](/riak/kv/2.1.3/developing/data-types/#Maps) are unreadable after upgrading due to a change in the
on-disk format of maps that was introduced as a performance improvement.

## Identification

You can verify whether this issue is affecting your cluster by checking
your cluster's [logs](/riak/kv/2.1.3/using/cluster-operations/logging). You will see errors along the following
lines if your cluster is affected:

```
2015-01-21 13:01:00.441 [error]
<0.1033.0>@riak_core_vnode:vnode_command:348 riak_kv_vnode command
failed
{{badrecord,dict},[{dict,filter_dict,2,[{file,"dict.erl"},{line,464}]},{riak_dt_map,'-filter_unique/4-fun-1-',4,[{file,"src/riak_dt_map.erl"},{line,466}]},{sets,fold_bucket,3,[{file,"sets.erl"},{line,313}]},{sets,fold_seg,4,[{file,"sets.erl"},{line,309}]},{sets,fold_segs,4,[{file,"sets.erl"},{line,305}]},{riak_dt_map,merge,2,[{file,"src/riak_dt_map.erl"},{line,454}]},{riak_kv_crdt,'-merge_value/2-fun-0-',5,[{file,"src/riak_kv_crdt.erl"},{line,204}]},{orddict,update,4,[{file,"orddict.erl"},{line,170}]}]}
```

## Affected Users

Users will be affected under the following conditions:

1. They use Riak maps in their cluster
1. They have upgraded to Riak version 2.0.4

If your cluster does not use Riak maps you may upgrade to 2.0.4 as
normal.

## Mitigation Strategy

The recommended mitigation strategy is to upgrade to [Riak 2.0.5 or
later](http://docs.basho.com/riak/latest/downloads/).
