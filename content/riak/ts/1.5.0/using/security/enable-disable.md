---
title_supertext: "Security"
title: "Enable & Disable"
description: "Enabling, disabling, and checking security in Riak TS."
menu:
  riak_ts-1.5.0:
    name: "Security: Enable & Disable"
    identifier: "security_enable_disable"
    weight: 110
    parent: "security"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/security/enable-disable
---

Riak TS security may be [checked](#checking-security-status), [enabled](#enabling-security), or [disabled](#disabling-security) through the command line, allowing an administrator to change security settings for the whole cluster without needing to go node-by-node.

## Enabling SSL

SSL is disabled by default. In order to use any authentication or authorization features, you must enable SSL for Riak TS.

Enabling SSL on a given node requires you to specify a host and port for the node:

```riak.conf
listener.protobuf.$name = {"127.0.0.1",8087}
```

## Enabling Security

{{% note title="Warning: Enable security with caution" %}}
Enabling security will change the way your client libraries and
your applications interact with Riak TS.

Once security is enabled, all client connections must be encrypted and all permissions will be denied by default. Do not enable this in production until you have worked through the [security checklist](../checklist) and tested everything in a non-production environment.
{{% /note %}}

Riak TS security is disabled by default. To enable it:

```bash
riak-admin security enable
```

All users, groups, authentication sources, and permissions can be
configured while security is disabled. This lets you create a
security configuration without prematurely impacting the service. Keep this in mind when you are [managing users](../user-management) and [managing sources](../sources-management).

## Disabling Security

If you disable security, all of the various permissions checks that take place when executing operations against Riak TS will be disabled. Users, groups, and other security attributes remain available for configuration while security is disabled, and will be applied if and when security is re-enabled.

```bash
riak-admin security disable
```

While security is disabled, clients will need to be reconfigured to no
longer require TLS and send credentials.

## Checking Security Status

To check whether security is currently enabled for the cluster, use the
`status` command:

```bash
riak-admin security status
```

This command will return `Enabled` or `Disabled`.

## Next Steps

For further guides on security in Riak TS, check out:

- [Security: User Management](../user-management)
- [Security: Sources Management](../sources-management)
