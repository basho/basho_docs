---
title: Session Storage
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

## Simple Case

Riak was originally created to serve as a highly scalable session store.   This is an ideal use case for Riak, which is a fundamentally a key value store.  Since user/session IDs are usually stored in cookies or otherwise known at lookup time, Riak is able to serve these requests with predictably low latency.  Riak's content-type agnosticism also imposes no restrictions on the value, so session data can be encoded in many ways and can evolve without administrative changes to schemas.


## Complex Case

Riak has other features that enable more complex session storage use cases.  The bitcask storage backend supports automatic expiry of keys, which frees application developers from implementing manual session expiry.  Riak's MapReduce system can also be used to perform analysis on large bodies of session data, for example to compute the average number of active users.  If sessions must be retrieved using multiple keys (e.g. a UUID or email address), Secondary Indices (2I) provides an easy solution.
