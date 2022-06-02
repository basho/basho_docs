---
title_supertext: "Conflict Resolution:"
title: "NodeJS"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "NodeJS"
    identifier: "usage_conflict_resolution_nodejs"
    weight: 104
    parent: "usage_conflict_resolution"
toc: true
aliases:
  - /riak/2.1.4/dev/using/conflict-resolution/nodejs
  - /riak/kv/2.1.4/dev/using/conflict-resolution/nodejs
---

For reasons explained in the [Introduction to conflict resolution]({{<baseurl>}}riak/kv/2.1.4/developing/usage/conflict-resolution), we strongly recommend adopting a conflict resolution strategy that
requires applications to resolve siblings according to use-case-specific
criteria. Here, we'll provide a brief guide to conflict resolution using the
official [Riak Node.js client](https://github.com/basho/riak-nodejs-client).

## How the Node.js Client Handles Conflict Resolution

In the Riak Node.js client, the result of a fetch can possibly return an array
of sibling objects.  If there are no siblings, that property will return an
array with one value in it.

[*Example:* creating object with siblings](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L21-L68)

So what happens if the length of `rslt.values` is greater than 1, as in the case
above?

In order to resolve siblings, you need to either fetch, update and store a
canonical value, or choose a sibling from the `values` array and store that as
the canonical value.

## Basic Conflict Resolution Example

In this example, you will ignore the contents of the `values` array and will
fetch, update and store the definitive value.

[*Example:* resolving siblings via store](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L91-L111)

### Choosing a value from `rslt.values`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value.

[*Example:* resolving siblings via first](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L113-L133)

### Using `conflictResolver`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value via a conflict resolution function.

[*Example:* resolving siblings via `conflictResolver](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L135-L170)
