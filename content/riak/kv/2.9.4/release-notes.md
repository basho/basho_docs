---
title: "Riak KV 2.9.4 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.9.4"
lastmod: 2020-07-03T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-2.9.4:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/2.9.4/community/release-notes
  - /riak/kv/2.9.4/intro-v20
  - /riak/2.9.4/intro-v20
  - /riak/kv/2.9.4/introduction
---

Released Jul 03, 2020.

## Overview

This release replaces the Riak KV 2.9.3 release, extending the issue resolution in kv_index_tictactree to detect other files where file truncation means the CRC is not present.

This release has a key outstanding issue when Tictac AAE is used in parallel mode. On larger clusters, this has been seen to cause significant issues, and so this feature should not be used other than in native mode.

TicTac AAE has some useful new functions. [Learn More >>](../using/cluster-operations/tictac-aae-fold).

[Previous Release Notes](#previous-release-notes)

## Previous Release Notes

Please see the KV 2.9.2 release notes [here]({{<baseurl>}}riak/kv/2.9.2/release-notes/), the KV 2.9.1 release notes [here]({{<baseurl>}}riak/kv/2.9.1/release-notes/), and the KV 2.9.0p5 release notes [here]({{<baseurl>}}riak/kv/2.9.4/release-notes/).

