---
title: "Conflict Resolution: Python"
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, python]
---

For reasons explained in the [[introduction to conflict
resolution|Conflict Resolution]], we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak Python
client](https://github.com/basho/riak-python-client).

## How the Python Client Handles Conflict Resolution
