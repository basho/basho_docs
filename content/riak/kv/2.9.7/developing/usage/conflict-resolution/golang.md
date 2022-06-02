---
title_supertext: "Conflict Resolution:"
title: "Go"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Go"
    identifier: "usage_conflict_resolution_golang"
    weight: 106
    parent: "usage_conflict_resolution"
toc: true
aliases:
  - /riak/2.9.7/dev/using/conflict-resolution/golang
  - /riak/kv/2.9.7/dev/using/conflict-resolution/golang
---

For reasons explained in the [Introduction to conflict resolution]({{<baseurl>}}riak/kv/2.9.7/developing/usage/conflict-resolution), we strongly recommend adopting a conflict resolution strategy that
requires applications to resolve siblings according to usecase-specific
criteria. Here, we'll provide a brief guide to conflict resolution using the
official [Riak Go client](https://github.com/basho/riak-go-client).

## How the Go Client Handles Conflict Resolution

In the Riak Go client, it is possible that the result of a fetch will return an array
of sibling objects. If there are no siblings, that property will return an
array with one value in it.

[*Example:* creating object with siblings](https://github.com/basho/riak-go-client/blob/master/examples/dev/using/conflict-resolution/main.go#L68-L70)

So what happens if the length of `Values` is greater than 1, as in the case
above?

In order to resolve siblings, you need to either: fetch, update, and store a
canonical value; or choose a sibling from the `Values` slice and store that as
the canonical value.

## Basic Conflict Resolution Example

In this example, you will ignore the contents of the `Values` slice and will
fetch, update and store the definitive value.

[*Example:* resolving siblings via store](https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/using/conflict-resolution.js#L125-L146)

### Choosing a value from `Values`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value.

[*Example:* resolving siblings using the first value](https://github.com/basho/riak-go-client/blob/master/examples/dev/using/conflict-resolution/main.go#L148-L167)

### Using `ConflictResolver`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value via a conflict resolution type.

[*Example:* resolving siblings via `ConflictResolver`](https://github.com/basho/riak-go-client/blob/master/examples/dev/using/conflict-resolution/main.go#L169-L210)




