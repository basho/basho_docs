---
title: "Security Basics"
description: "Enabling, disabling, and checking security in Riak TS."
menu:
  riak_ts-1.4.0:
    name: "Security Basics"
    identifier: "security_basics"
    weight: 110
    parent: "security"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/security/basics
canonical_link: "https://docs.basho.com/riak/ts/latest/using/security/basics/"
---

Riak TS security may be [checked](#checking-security-status), [enabled](#enabling-security), or [disabled](#disabling-security) by an administrator
through the command line. This allows an administrator to change
security settings for the whole cluster quickly without needing to
change settings on a node-by-node basis.

{{% note %}}
Currently, Riak TS security commands can be run only through
the command line, using the `riak-admin security` command. In future
versions of Riak TS, administrators may have the option of issuing
those commands through the Protocol Buffers interface.
{{% /note %}}

## Enabling Security

> **Warning: Enable security with caution**
>
> Enabling security will change the way your client libraries and
your applications interact with Riak TS.
>
> Once security is enabled, all client connections must be encrypted and all permissions will be denied by default. Do not enable this in production until you have worked through the [security checklist](../checklist) and tested everything in a non-production environment.

Riak TS security is disabled by default. To enable it:

```bash
riak-admin security enable
```

All users, groups, authentication sources, and permissions can be
configured while security is disabled, allowing you to create a
security configuration of any level of complexity without prematurely
impacting the service. Keep this in mind when you are
[managing users](../user-management) and [managing sources](../sources-management).

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

This command will usually return `Enabled` or `Disabled`, but if
security is enabled on a mixed-mode cluster (running a combination of
Riak 2.0 and older versions) it will indicate that security is enabled
but not yet available.

## Managing Users and Security Sources

For further guides on security in Riak TS, check out:

- [Security: User Management](../user-management)
- [Security: Sources Management](../sources-management)
