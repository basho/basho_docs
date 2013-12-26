---
title: Rolling Upgrade to Enterprise
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [installing, upgrading]
---

Instructions for upgrading to Riak Enterprise:

  1. Back up your `etc` (`app.config` and `vm.args`) and `data` directories.
  2. Shutdown the node you are going to upgrade.
  3. Uninstall your Riak package.
  4. Install the `riak_ee` package.
  5. A standard package uninstall should not have removed your data directories. If it did, move your backup to where the data directory should be.
  6. Copy any customizations from your backed-up `vm.args` to the `riak_ee`installed `vm.args` file, these files may be identical.
  7. The `app.config` file from `riak_ee` will be significantly different from your backed-up file. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original `app.config` file into the appropriate sections in the new one.
  8. Start Riak on the upgraded node.

## Basho Patches

After upgrading, you should ensure that any custom patches contained in the
`basho-patches` directory are examined to determine their application to
the upgraded version. If you find that patches no longer apply to the upgraded
version, you should remove them from the `basho-patches` directory prior to
operating the node in production.

The following table lists locations of the `basho-patches` directory for each
supported operating system:

| OS | Directory
|----|-----------
| CentOS & RHEL Linux | `/usr/lib64/riak/lib/basho-patches`
| Debian & Ubuntu Linux | `/usr/lib/riak/lib/basho-patches`
| FreeBSD | `/usr/local/lib/riak/lib/basho-patches`
| SmartOS | `/opt/local/lib/riak/lib/basho-patches`
| Solaris 10 | `/opt/riak/lib/basho-patches`

{{#1.3.0+}}
## Riaknostic

It is a good idea to also verify some basic configuration and general health
of the Riak node after upgrading by using Riak's built-in diagnostic
utility *Riaknostic*.

Ensure that Riak is running on the node, and issue the following command:

```
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal node
operation.
{{/1.3.0+}}
