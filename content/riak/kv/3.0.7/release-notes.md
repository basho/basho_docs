---
title: "Riak KV 3.0.7 Release Notes"
description: ""
project: "riak_kv"
project_version: "3.0.7"
lastmod: 2021-07-17T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.7:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.7/community/release-notes
  - /riak/kv/3.0.7/intro-v20
  - /riak/3.0.7/intro-v20
  - /riak/kv/3.0.7/introduction
---

Released Jul 16, 2021.

## Overview

The primary change in 3.0.7 is that Riak will now run the erlang runtime system in interactive mode, not embedded mode. This returns Riak to the default behaviour prior to Riak KV 3.0, in order to resolve a number of problems which occurred post 3.0 when trying to dynamically load code.

The mode used is controlled in a pre-start script, changing this script or overriding the referenced environment variable for the riak user should allow the runtime to be reverted to using embedded mode, should this be required.

This release also extends a prototype API to support for the use of the nextgenrepl API by external applications, for example to reconcile replication to an external non-riak database. The existing fetch api function has been extended to allow for a new response format that includes the Active Anti-Entropy Segment ID and Segment Hash for the object (e.g. to be used when recreating the Riak merkle tree in external databases).
A new push function has been added to the api, this will push a list of object references to be queued for replication.
## Previous Release Notes

Please see the KV 3.0.6 release notes [here]({{<baseurl>}}riak/kv/3.0.6/release-notes/).

