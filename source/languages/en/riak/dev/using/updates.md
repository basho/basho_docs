---
title: Updating Values
project: riak
version: 2.0.0+
document: tutorials
audience: beginner
keywords: [developers, updating, kv]
---

While Riak supports a variety of querying mechanisms, such as [[Riak
Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]],
we always recommend sticking to basic **C**reate, **R**read, **U**pdate,
and **D**elete (CRUD) operations as much as possible, as these
operations are generally the most performant and reliable operations
that Riak offers. A complete guide to making decisions about Riak
features can be found in our [[Application Guide|Application
Guide#Which-Features-Should-You-Consider]].

Amongst the four CRUD operations, object updates in Riak tend to be the
least straightforward and to require a bit more subtle reasoning on the
application side than the others. In this document, we'll discuss some
best practices for updating Riak objects and provide code examples for
each of our official [[client libraries]]: Java, Ruby, Python, and
Erlang.

## The Ideal Object Update Cycle


