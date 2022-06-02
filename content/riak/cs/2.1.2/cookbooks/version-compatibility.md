---
title: "Version Compatibility"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Version Compatibility"
    identifier: "reference_version_compat"
    weight: 101
    parent: "reference"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/Version-Compatibility/
  - /riak/cs/latest/cookbooks/version-compatibility/
---

If you are deploying Riak CS in combination with an existing Riak
cluster, you should verify that the version of Riak that you are using
is compatible with the version of Riak CS that you intend to use.

It is important to note that not all versions of Riak are compatible
with Riak CS, but a number of version combinations have been tested, are
known to function together, and can be recommended for use.

The following details combinations of Riak and Riak CS versions which
are known to function together and provides some general tips about Riak
versions for use with Riak CS.

## Unsupported Riak Versions

Riak versions prior to version 1.2.0 are known to have performance
issues and are not tested, recommended, or supported for use with Riak
CS. Additionally, Riak versions prior to 1.0.0 are missing essential
functionality, such as Secondary Indexes or LevelDB support, required by
Riak CS.

## Working Version Combinations

Basic functional testing has been performed with the following combinations of
Riak and Riak CS. These versions are also known to be functioning in production environments.

Riak version  | Stanchion version | Riak CS version
--------------|-------------------|----------------
1.2.1         | 1.2.2             | 1.2.2
1.2.1         | 1.3.0             | 1.3.0
1.3.0         | 1.2.2             | 1.2.2
1.3.0         | 1.3.0             | 1.3.0
1.4.0         | 1.4.0             | 1.4.0
1.4.1         | 1.4.0             | 1.4.0
1.4.8         | 1.4.3             | 1.4.5
1.4.10        | 1.5.0             | 1.5.0
1.4.10        | 1.5.0             | 1.5.1
1.4.10        | 1.5.0             | 1.5.2
1.4.12        | 1.5.0             | 1.5.3
1.4.12        | 1.5.0             | 1.5.4
2.0.5         | 2.0.0             | 2.0.0
2.0.5         | 2.0.0             | 2.0.1
2.1.2         | 2.1.0             | 2.1.0

**Note**: While Riak CS versions 1.5.0 and later will work with Riak
1.4.x, we highly recommend running CS with at least Riak 1.4.8,
preferably 1.4.10.

Basic functionality testing consists of account creation, object storage and
retrieval, bucket listing operations, and Access Control List (ACL) enforcement
verification.

Note that functional testing of Riak CS clusters operating with mixed versions
(e.g., a combination of Riak CS version 1.2.2 and version 1.3.0 nodes) has not
been performed, and cannot be recommended at this time.
