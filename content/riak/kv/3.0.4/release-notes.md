---
title: "Riak KV 3.0.4 Release Notes"
description: ""
project: "riak_kv"
project_version: "3.0.4"
lastmod: 2021-03-24T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.4:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.4/community/release-notes
  - /riak/kv/3.0.4/intro-v20
  - /riak/3.0.4/intro-v20
  - /riak/kv/3.0.4/introduction
---

Released April 2nd, 2021.

## Overview

There are two fixes provided in Release 3.0.4:

* An issue with leveled application dependencies has been resolved, and so lz4 can now again be used as the compression method.

* The riak clients are now compatible with systems that require semantic versioning.

This release is tested with OTP 20, OTP 21 and OTP 22; but optimal performance is likely to be achieved when using OTP 22.

## Previous Release Notes

Please see the KV 3.0.3 release notes [here]({{<baseurl>}}riak/kv/3.0.3/release-notes/).

