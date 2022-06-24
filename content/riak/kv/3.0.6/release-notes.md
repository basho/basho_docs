---
title: "Riak KV 3.0.6 Release Notes"
description: ""
project: "riak_kv"
project_version: 3.0.6
menu:
  riak_kv-3.0.6:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.6/community/release-notes
  - /riak/kv/3.0.6/intro-v20
  - /riak/3.0.6/intro-v20
  - /riak/kv/3.0.6/introduction
---

Released May 10, 2021.


## Overview

Release 3.0.6 adds location-awareness to Riak cluster management. The broad aim is to improve data diversity across locations (e.g. racks) to reduce the probability of data-loss should a set of nodes fail concurrently within a location. The location-awareness does not provide firm guarantees of data diversity that will always be maintained across all cluster changes - but testing has indicated it will generally find a cluster arrangement which is close to optimal in terms of data protection.

If location information is not added to the cluster, there will be no change in behaviour from previous releases.

There may be some performance advantages when using the location-awareness feature in conjunction with the leveled backend when handling GET requests. With location awareness, when responding to a GET request, a cluster is more likely to be able to fetch an object from a vnode within the location that received the GET request, without having to fetch that object from another location (only HEAD request/response will commonly travel between locations).

This release is tested with OTP 20 and OTP 22; but optimal performance is likely to be achieved when using OTP 22.

## Previous Release Notes

Please see the KV 3.0.4 release notes [here]({{<baseurl>}}riak/kv/3.0.4/release-notes/).





