---
title: "Riak KV 3.2.0 Release Notes"
description: ""
project: "riak_kv"
project_version: "3.2.0"
lastmod: 2022-12-30T00:00:00-00:00
sitemap:
  priority: 0.9
menu:
  riak_kv-3.2.0:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.2.0/community/release-notes
  - /riak/kv/3.2.0/intro-v20
  - /riak/3.2.0/intro-v20
  - /riak/kv/3.2.0/introduction
---

Released Dec 29, 2022.

## Overview

This release is an OTP uplift release. Whereas release 3.0.1 supports OTP 22; the intention is for Release 3.2.n to support OTP 22, OTP 24, and OTP 25. There are potential throughput benefits of up to 10% when using OTP 24/25 rather than OTP 22 where load is CPU bound. OTP 25 is currently the preferred platform for this release.

There are specific risks associated with OTP uplift releases due to the large volume of underlying changes inherited. It is advised that Riak users should take specific care to test this release in a pre-production environment. Please raise any issues discovered via Github.

**Important logging changes**

As part of this change, the lager dependancy has been removed, with OTP's internal logger used instead. Any logging configuration should be updated as a part of the migration, using the new options made available via riak.conf. Support for logging using syslog has been removed. The leveled backend will still write directly to erlang.log files, but this will be addressed in a future release.

There has been a significant overhaul of the release and packaging scripts in order to adopt changes within relx. Note that due to the updates in relx, riak daemon should be used instead of riak start. Some riak and riak-admin commands may also now return an additional ok output. Going forward, both riak admin and riak-admin should work for admin commands. Packaging suport has now been added for Alpine Linux and FreeBSD.

Note that this release of Riak is packaged with a bespoke build of rebar3, this alters mainstream rebar3/relx to allow us control over deprecation warnings in relx.

When building from source, the snappy dependancy is now made rather than fetched using a cached package, so support for cmake is required to build. Note that on older versions of OSX the current version of snappy will not compile. This will be resolved when their is a formal release version of snappy containing this fix.

In this release, tagging of individual dependencies has not been used. Building consistently with the correct versions of dependencies is therefore dependent on the commit references being used from within the rebar.lock file.

## Previous Release Notes

Please see the KV 3.0.12 release notes [here]({{<baseurl>}}riak/kv/3.0.12/release-notes/).

