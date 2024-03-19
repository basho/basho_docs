---
title: "Riak KV 3.0.14 Release Notes"
description: ""
project: "riak_kv"
project_version: "3.0.14"
lastmod: 2023-02-13T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.14:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.14/community/release-notes
  - /riak/kv/3.0.14/intro-v20
  - /riak/3.0.14/intro-v20
  - /riak/kv/3.0.14/introduction
---

Released Feb 13, 2023.

## Overview

This release fixes an issue whereby a failure to signal and handle back-pressure correctly by the leveled backend can cause a backlog within the store. In particular this can be triggered by handoffs (e.g. due to cluster admin operations), and lead to partition transfers stalling almost completely. The issue existed in previous releases, by may have been exacerbated by refactoring in Riak KV 3.0.13.

An additional minor improvement has been made to handoffs. Previously requests to reap tombstones after deletions (where the `delete_mode` is not keep), would not be forwarded during handoffs. These tombstones would then need to be corrected by AAE (which may result in a permanent tombstone). There is now a configuration option `handoff_deletes` which can be enabled to ensure these reap requests are forwarded, reducing the AAE work required on handoff completion.

Desipite the handoff improvements in Riak KV 3.0.13, handoff timeouts are still possible. If handoff timeouts do occur, then the first stage should be to reduce the handoff batch threshold count to a lower number than that of the item_count in the handoff sender log.

## Previous Release Notes

Please see the KV 3.0.13 release notes [here]({{<baseurl>}}riak/kv/3.0.13/release-notes/).

