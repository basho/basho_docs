---
title: "Riak KV 2.0.1 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.0.1"
lastmod: 2014-09-25T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.0.1:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.0.1/community/release-notes
---

## Client certificate authentication

As of the recently released 2.0, authentication and authorization
are now available in Riak.

Under 2.0.0, it was possible that malformed client certificates would
be accepted. This bug has been corrected with 2.0.1.

## Merged PRs

* bitcask/186: [Bugfix/key transform crash](https://github.com/basho/bitcask/pull/186)
* bitcask/189: [Refresh efile port if gone](https://github.com/basho/bitcask/pull/189)
* bitcask/190: [Fix scan error deadlock](https://github.com/basho/bitcask/pull/190)
* bitcask/192: [Fix remove expired on read race](https://github.com/basho/bitcask/pull/192)
* bitcask/197: [Fix extra tombstones on update](https://github.com/basho/bitcask/pull/197)
* bitcask/198: [Fix race listing readable files](https://github.com/basho/bitcask/pull/198)
* riak_kv/1008: [Use SC bucket types and buckets to know ensembles](https://github.com/basho/riak_kv/pull/1008)
* riak_kv/1026: [Update to use new breadth-first AAE exchange](https://github.com/basho/riak_kv/pull/1026)
* riak_core/626: [Allow handoff sender to abort handoff by throw'ing from fold fun](https://github.com/basho/riak_core/pull/626)
* riak_core/627: [Handoff sender sends sync periodically](https://github.com/basho/riak_core/pull/627)
* riak_core/629: [Add breadth-first AAE exchange](https://github.com/basho/riak_core/pull/629)
* riak_api/66: [Do not treat errors as success](https://github.com/basho/riak_api/pull/66)
* riak_repl/618: [Added a worker pool for fullsync sinks.](https://github.com/basho/riak_repl/pull/618)
* riak_repl/619: [Small user experience fixes.](https://github.com/basho/riak_repl/pull/619)
* riak_repl/620: [Improved AAE fullsync integration/2.0 pull request](https://github.com/basho/riak_repl/pull/620)
