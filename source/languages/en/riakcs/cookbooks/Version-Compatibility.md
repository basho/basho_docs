---
title: Version Compatibility
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [compatibility]
interest: false
---

If you are deploying Riak CS in combination with an existing Riak cluster, you
should verify that the version of Riak you are using is compatible with the
version of Riak CS that you intend to use.

It is important to note that not all versions of Riak are compatible with
Riak CS, but a number of version combinations have been tested, are known to
function together, and can be recommended for use.

The following details combinations of Riak and Riak CS versions which are
known to function together and provides some general tips about Riak versions
for use with Riak CS.

## Unsupported Riak Versions

Riak versions prior to version 1.2.0 are known to have performance issues and
are not tested, recommended, or supported for use with Riak CS. Additionally,
Riak versions prior to 1.0.0 are missing essential functionality such as
Secondary Indexes or LevelDB support required by Riak CS.

## Working Version Combinations

Basic functional testing has been performed with the following combinations of
Riak and Riak CS. These versions are also known to be functioning in
production environments.

Riak version  | Stanchion version | Riak CS version
--------------|-------------------|----------------
1.2.1         | 1.2.2             | 1.2.2
1.2.1         | 1.3.0             | 1.3.0
1.3.0         | 1.2.2             | 1.2.2
1.3.0         | 1.3.0             | 1.3.0
1.4.0         | 1.4.0             | 1.4.0
1.4.1         | 1.4.0             | 1.4.0

Basic functionality testing consists of account creation, object storage and
retrieval, bucket listing operations, and Access Control List enforcement
verification.

Note that functional testing of Riak CS clusters operating with mixed versions
(e.g., a combination of Riak CS version 1.2.2 and version 1.3.0 nodes) has not
been performed, and cannot be recommended at this time.
