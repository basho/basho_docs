---
title: Cluster Metadata
project: riak
toc: true
---

Subsystem inside of Riak
Used by `riak_core` applications wishing to work with information stored cluster wide. It is useful for storing application metadata or any information that needs to be read without blocking on communication over the network.

CMD is a key/value store