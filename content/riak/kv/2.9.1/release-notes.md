---
title: "Riak KV 2.9.1 Release Notes"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/2.9.1/community/release-notes
  - /riak/kv/2.9.1/intro-v20
  - /riak/2.9.1/intro-v20
  - /riak/kv/2.9.1/introduction
---

Released Feb 15, 2020.


## Overview

This release adds a number of features built on top of the Tictac AAE feature made available in 2.9.0. The new features depend on Tictac AAE being enabled, but are backend independent. The primary features of the release are:

* A new combined full-sync and real-time replication system nextgenrepl, that is much faster and more efficient at reconciling overall state of clusters (e.g. full-sync).

* A mechanism for requesting mass deletion of objects on expiry, and mass reaping of tombstones after a time to live. This is not yet an automated, scheduled, set of garbage collection processes, it is required to be triggered by an operational process.

* A safe method of listing buckets regardless of backend chosen. Listing buckets had previously not been production safe, but can still be required in production environments - it can now be managed safely via an `aae_fold`.

* A version uplift of the internal ibrowse client, a minor riak_dt fix to resolve issues of unit test reliability, a fix to help build (the now deprecated) erlang_js in some environments, and the removal of hamcrest as a dependency.

[Previous Release Notes](#previous-release-notes)


## Previous Release Notes

Please see the KV 2.9.0p5 release notes [here]({{<baseurl>}}riak/kv/2.9.0p5/release-notes/), and the KV 2.2.6 release notes [here]({{<baseurl>}}riak/kv/2.2.6/release-notes/).
