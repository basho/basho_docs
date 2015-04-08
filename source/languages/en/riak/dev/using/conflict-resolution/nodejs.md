---
title: "Conflict Resolution: NodeJS"
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, nodejs, node, javascript]
---

For reasons explained in the [[introduction to conflict resolution|Conflict
Resolution]], we strongly recommend adopting a conflict resolution strategy that
requires applications to resolve siblings according to use-case-specific
criteria. Here, we'll provide a brief guide to conflict resolution using the
official [Riak NodeJS client][riak_dotnet_client].

## How the NodeJS Client Handles Conflict Resolution

In the Riak NodeJS client, the result of a fetch can possibly return an array
of sibling objects.  If there are no siblings, that property will return an
array with one value in it.

[*Example:* creating object with siblings](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L21-L68)

So what happens if the length of `rslt.values` is greater than 0, as in the case
above?

In order to resolve siblings, you need to either fetch, update and store a
canonical value, or choose a sibling from the `values` array and store that as
the canonical value.

## Basic Conflict Resolution Example

In this example, you will ignore the contents of the `values` array and will
fetch, update and store the definitive value.

[*Example:* resolving siblings via store](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L72-L92)

### Choosing a value from `rslt.values`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value.

[*Example:* resolving siblings via first](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L94-L113)

### Using `conflictResolver`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value via a conflict resolution function.

[*Example:* resolving siblings via `conflictResolver](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L117-L151)

