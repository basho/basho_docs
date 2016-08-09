---
title: "Security Checklist"
description: "Overview of security in Riak TS."
menu:
  riak_ts-1.4.0:
    name: "Security Checklist"
    identifier: "security_checklist"
    weight: 100
    parent: "security"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/security/checklist
canonical_link: "https://docs.basho.com/riak/ts/latest/using/security/checklist/"
---

Before turning on Riak TS security there are key steps all applications need to take. Missing one of these steps may break your application, so make sure you have done each of the following **before** enabling security:

1. Ensure the original Riak Search (version 1) and link
   walking are not required. Enabling security will break this
   functionality. If you wish to use security and Search together, you
   will need to use the [new Search feature](/riak/kv/2.1.4/developing/usage/search/).
1. Because Riak TS security requires a secure SSL connection, you will need
   to generate appropriate SSL certs, [enable SSL](../sources-management/#enabling-ssl) and establish a [certification configuration](../sources-management/#certificate-configuration) on each node. **If you
   enable security without having established a functioning SSL
   connection, all requests to Riak TS will fail**.
1. Define [users](../user-management)
   and, optionally, groups
1. Define an [authentication source](../sources-management) for each user
1. Grant the necessary [permissions](../user-management/#managing-permissions) to each user (and/or group)
1. Check any Erlang MapReduce code for invocations of Riak modules other
   than `riak_kv_mapreduce`. Enabling security will prevent those from
   succeeding unless those modules are available via the `add_path`
   mechanism documented in [Installing Custom Code](/riak/kv/2.1.4/using/reference/custom-code).
1. Make sure that your client software will work properly:
    * It must pass authentication information with each request
    * It must support encrypted [Protocol Buffers](/riak/kv/2.1.4/developing/api/protocol-buffers/)
      traffic
    * Code that uses Riak's deprecated Link Walking feature **will
      not work** with security enabled
1. If you have applications that rely on an already existing Riak
   cluster, make sure that those applications are prepared to gracefully
   transition into using Riak security once security is enabled.

After all of the above steps have been performed and your security setup has been properly checked, visit the [Riak TS Security Basics](../basics) page for instructions on enabling security.
