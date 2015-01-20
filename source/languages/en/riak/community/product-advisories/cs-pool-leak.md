---
title: Riak CS Pool Leak
project: riak
version: 1.0.0+
versions: false
document: reference
---

Info | Value
:----|:----
Author | John Caprice
Date issued | October 7, 2014, 12:42
Product | Riak CS
Affected versions | 1.5.0-1.5.1

## Overview

A connection pool leak, affecting Riak CS 1.5.0 and 1.5.1, which can
cause Riak nodes to be unable to service requests once the connection
pool has been exhausted.

## Description

#### Symptoms

Inability to service requests once pools are exhausted

#### Cause

A bug introduced with the new [[multibag feature|Riak CS Multibag
Support]] in Riak CS 1.5.0

#### Identification

* When the issue is occuring, user authentication will fail, returning a
    403 status code. In the Riak logs, these 403 errors will display the
    following reason: `all_workers_busy`.
* The issue can be preëmptively identified via `[[riak attach|riak
    Command Line#attach]]`, using the code snippet below to output pool
    information, specifically `pbc_pool_master`. If `pbc_pool_master` is
    increasing and `request_pool` plus `bucket_list_pool` is not equal
    to `pbc_pool_master`, then the leak is occurring.

    ```erlang
    [{P, poolboy:status(P)} || P <- [request_pool, bucket_list_pool, pbc_pool_master]].
    ```

## Mitigation Strategy

There is currently no way to prevent this issue from occurring. Once
identified, as detailed in the description section above, the symptoms
can be addressed by restarting Riak CS on the affected nodes.
