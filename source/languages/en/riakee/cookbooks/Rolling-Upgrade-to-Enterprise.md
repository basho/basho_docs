---
title: Rolling Upgrade to Enterprise
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [installing, upgrading]
---

Instructions for upgrading:

  1. Backup your etc (app.config and vm.args) and data directories.
  2. Shutdown the node you are going to upgrade.
  3. Uninstall your riak package.
  4. Install the riak_ee package.
  5. A standard package uninstall should not have removed your data directories. If it did, move your backup to where the data directory should be.
  6. Copy any customizations from your backed up vm.args to the riak_ee installed one, these files may be identical.
  7. The app.config file from riak_ee will be significantly different from your backed up one. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original app.config file into the sections in the new one.
  8. Start Riak on the upgraded node.

## Basho Patches

After upgrading, you should ensure that any custom patches contained in the
`basho-patches` directory are examined to determine their application to
the upgraded version. If you find that patches no longer apply to the upgraded
version, you should remove them from the `basho-patches` directory prior to
operating the node in production.

The following table lists locations of the `basho-patches` directory for each
supported operating system:

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>CentOS &amp; RHEL Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/lib64/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Debian &amp; Ubuntu Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/lib/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>FreeBSD</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/local/lib/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>SmartOS</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/opt/local/lib/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Solaris 10</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/opt/riak/lib/basho-patches</tt></p>
</td>
</tr>
</tbody>
</table>

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
