---
title: "Riak KV 3.0.3 Release Notes"
description: ""
project: "riak_kv"
project_version: "3.0.3"
lastmod: 2021-01-14T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.3:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.3/community/release-notes
  - /riak/kv/3.0.3/intro-v20
  - /riak/3.0.3/intro-v20
  - /riak/kv/3.0.3/introduction
---

Released Jan 14, 2021.

## Overview

There are two fixes provided in Release 3.0.3:

* A performance issue with OTP 22 and leveled has been corrected. This generally did not have a significant impact when running Riak, but there were some potential cases with Tictac AAE and AAE Folds where there could have been a noticeable slowdown.

* An issue with console commands for bucket types has now been fully corrected, having been partially mitigated in 3.0.2.

This release is tested with OTP 20, OTP 21 and OTP 22; but optimal performance is likely to be achieved when using OTP 22.

## Previous Release Notes

Please see the KV 3.0.2 release notes [here]({{<baseurl>}}riak/kv/3.0.2/release-notes/).

