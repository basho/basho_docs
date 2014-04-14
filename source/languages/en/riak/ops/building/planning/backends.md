---
title: Choosing a Backend
project: riak
version: 0.10.0+
document: tutorials
toc: true
audience: intermediate
keywords: [backends, planning]
next: "[[Bitcask]]"
interest: false
moved: {
    '1.4.0-': '/tutorials/choosing-a-backend'
}
---

Pluggable storage backends are a key feature of Riak KV. These allow the
choice of a low-level storage engine that suits specific operational
needs. For example, if one needs maximum throughput, data persistence,
and a bounded keyspace then Bitcask is a good choice. On the other hand,
if one needs to store a large number of keys or needs secondary indexes,
then LevelDB would be a better backend recommendation.

These backends are supported:

* [[Bitcask]]
* [[LevelDB]]
* [[Memory]]
* [[Multi]]

Riak supports the use of custom storage backends as well. See the
storage [[Backend API]] for more details.

|Feature or Characteristic                      |Bitcask|LevelDB|Memory|
|-----------------------------------------------|:-----:|:-----:|:----:|
|Default Riak backend                           |✓      |       |      |
|Persistent                                     |✓      |✓      |      |
|Keyspace in RAM                                |✓      |       |✓     |
|Keyspace can be greater than available RAM     |       |✓      |      |
|Keyspace loaded into RAM on startup<sup>1</sup>|✓      |       |      |
|Objects in RAM                                 |       |       |✓     |
|Object Expiration                              |✓      |       |✓     |
|Secondary Indexes                              |       |✓      |✓     |

<sup>1</sup>Noted here since this can affect Riak start times for large
keyspaces.
