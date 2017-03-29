---
title: "Riak KV 2.2.3 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.2.3"
menu:
  riak_kv-2.2.3:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.2.3/community/release-notes
  - /riak/kv/2.2.3/intro-v20
  - /riak/2.2.3/intro-v20
  - /riak/kv/2.2.3/introduction
---


Released March [Day], 2017.

In fixing a `riak-admin` issue in [KV 2.2.3](/riak/kv/2.2.3/release-notes/), we inadvertently used a Bash-specific script variable (`$RANDOM`), causing the riak attach and riak top commands to fail on Ubuntu when /bin/sh is aliased to Dash.

## Bugs Fixed

* [[riak PR 909](https://github.com/basho/riak/pull/909) & [node_package PR 217](https://github.com/basho/node_package/pull/217)] Replace `$RANDOM` with PID to support non-Bash.

## Previous Release Notes

Please see the KV 2.2.3 release notes [here](/riak/kv/2.2.3/release-notes/), and the KV 2.2.1 release notes [here](/riak/kv/2.2.1/release-notes/).