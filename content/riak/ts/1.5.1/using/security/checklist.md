---
title_supertext: "Security"
title: "Checklist"
description: "Overview of security in Riak TS."
menu:
  riak_ts-1.5.1:
    name: "Security Checklist"
    identifier: "security_checklist"
    weight: 100
    parent: "security"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/using/security/checklist
---

[enable ssl]: ../enable-disable/#enabling-ssl
[cert config]: ../sources-management/#certificate-configuration
[security users]: ../user-management
[security sources]: ../sources-management
[manage permissions]: ../user-management/#managing-permissions
[pbc]: {{<baseurl>}}riak/kv/2.2.0/developing/api/protocol-buffers/
[security enable disable]: ../enable-disable

Before turning on Riak TS security there are key steps all applications need to take. Missing one of these steps may break your application, so make sure you have done each of the following BEFORE enabling security:

1. Because Riak TS security requires a secure SSL connection, you will need
   to generate appropriate SSL certs, [enable SSL][enable ssl] and establish a [certification configuration][cert config] on each node. **If you
   enable security without having established a functioning SSL
   connection, all requests to Riak TS will fail**.
1. Define [users][security users]
   and, optionally, groups
1. Define an [authentication source][security sources] for each user
1. Grant the necessary [permissions][manage permissions] to each user (and/or group)
1. Make sure that your client software will work properly:
    * It must pass authentication information with each request
    * It must support encrypted [Protocol Buffers][pbc]
      traffic
1. If you have applications that rely on an already existing Riak TS
   cluster, make sure that those applications are prepared to gracefully
   transition into using Riak security once security is enabled.

After all of the above steps have been performed and your security setup has been properly checked, visit the [Riak TS Security: Enabling][security enable disable] page for instructions on enabling security.
