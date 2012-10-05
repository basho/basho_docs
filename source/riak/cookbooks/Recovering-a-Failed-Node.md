---
title: Recovering a Failed Node
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: advanced
keywords: [troubleshooting]
---

Restarting a node after a failure may result in a slower than normal
startup time. The longer startup time associated with
recovery may lead to other problems;
[[bug #134|https://issues.basho.com/show_bug.cgi?id=134]], for
example, describes an issue where Riak may begin sending requests
(get, put, delete) to nodes before they are ready to accept
requests. To avoid problems when recovering a failed node the
following technique should be followed.

## General Recovery Notes

When a Riak node is to be recovered, general rules of recovery apply
depending on what failed in particular. Check for RAID and file system
consistency, faulty memory, fully functional network connections, etc.

In general, when a failed node comes back up, make sure it has the
same node name as before it crashed.  Changing the node name makes the
cluster assume this is an entirely new node, leaving the old one still
as part of the ring, until you remove it manually using `riak-admin
remove riak@node-name.host`.

When the node is recovering, hinted handoff will kick in and update
the data on the recovered node with updates from the rest of the
cluster. Your cluster may temporarily return "not found" for objects
that are currently being handed off (see our page on
[[Eventual Consistency]] for more details on these scenarios, in
particular how the system behaves while the failed node is not part of
the cluster).

There may be additional steps for recovery that depend on your storage
backend.

## Bitcask

A failed node that's using Bitcask as storage backend can be started
normally using `riak start` or the Riak init.d scripts and should
recover on its own.

More information can be found on [[Failure and Recovery]].
