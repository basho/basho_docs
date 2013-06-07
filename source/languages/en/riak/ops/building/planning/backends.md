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
    '1.4.0-': '/tutorials/choosing-a-backend/'
}
---

A key feature of Riak KV is the pluggable storage backends. These allow the
ability to choose a low-level storage engine that suits specific operational
needs. For example, if one needs maximum throughput coupled with data
persistence and has a bounded keyspace, Bitcask is a good choice. On the other hand, if one needs to store a large number of keys, then LevelDB would be a better backend recommendation.

These backends are supported:

* [[Bitcask]]
* [[LevelDB]]
* [[Memory]]
* [[Multi]]
* [[Innostore]] {{1.2.0-}}

Riak supports the use of custom storage backends as well. See the storage [[Backend API|Backend API]] for more details.
