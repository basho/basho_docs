---
title: Backend Migration
project: riak
version: 2.0.0+
document: tutorials
audience: advanced
keywords: [backends, migration]
---

Riak offers four storage backend options: [[Bitcask]] (the default),
[[LevelDB]], [[Memory]], and [[Multi]] (which enables you to configure
and use multiple backends in a single cluster).

Because each backend has its own strengths and weaknesses and because
not all features are available in all backends---e.g.
[[secondary indexes|Using Secondary Indexes]] are available only in
LevelDB, while object expiry is available only in Bitcask---it may be
necessary under certain conditions to migrate some or all of your data
from one storage backend to another.