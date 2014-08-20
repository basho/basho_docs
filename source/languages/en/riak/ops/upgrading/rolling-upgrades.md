---
title: Rolling Upgrades
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [upgrading]
moved: {
    '1.4.0-': '/cookbooks/Rolling-Upgrades'
}
---


<div class="note">
<div class="title">Note on upgrading Riak from Older Versions</div>
Riak upgrades are tested and supported for two feature release versions.
For example, upgrades from 1.1.x to 1.3.x are tested and supported,
while upgrades from 1.1.x to 1.4.x are not. When upgrading to a new
version of Riak that is more than two feature releases ahead, we
recommend first upgrading to an intermediate version. For example, in an
upgrade from 1.1.x to 1.4.x, we recommend upgrading from 1.1.x to 1.3.x
before upgrading to 1.4.x.

If you run [[Riak Control]], you should disable it during the rolling
upgrade process.
</div>

Riak nodes now negotiate with each other to determine supported
operating modes. This allows clusters containing mixed versions of Riak
to properly interoperate without special configuration, and simplifies
rolling upgrades.

In previous Riak versions, users were required to disable new features
during the rolling upgrade process, and then enable them after all nodes
were upgraded. This is now handled automatically by Riak. For more on
this process, see our documentation on [[capability negotiation]].

## Disk Space Usage Following Backups

When upgrading Riak, it is important to back up each node's `/etc` and
`/data` directories. Following a successful upgrade, you may notice that
disk usage has changed between versions, i.e. each node may end up
storing more or less that it did in a previous version. Please be aware
that this is normal behavior. Any changes in disk usage are likely
attributable to code changes made to [[Bitcask]] or [[LevelDB]] or to
routine effects of automated processes such as [[active anti-entropy]].

## Debian/Ubuntu

The following example demonstrates upgrading a Riak node that has been
installed with the Debian packages provided by Basho.

1\. Stop Riak

```bash
riak stop
```


2\. Back up the Riak node's `/etc` and `/data` directories

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```


3\. Upgrade Riak

```bash
sudo dpkg -i <riak_package_name>.deb
```


4\. Restart Riak

```bash
riak start
```


5\. Verify Riak is running the new version

```bash
riak-admin status
```


6\. Wait for the `riak_kv` service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```

`<target_node>` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```

While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

<div class="info">
<div class="title">Note for Secondary Index users</div>
If you use Riak's Secondary Indexes and are upgrading from a version
prior to Riak version 1.3.1, you need to reformat the indexes using the
<code>[[riak-admin reformat-indexes|riak-admin Command
Line#reformat-indexes]]</code> command. More details about reformatting
indexes are available in the [release
notes](https://github.com/basho/riak/blob/master/RELEASE-NOTES.md).
</div>

## RHEL/CentOS

The following example demonstrates upgrading a Riak node that has been
installed with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak

```bash
riak stop
```

2\. Back up Riak's `/etc` and `/data` directories

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Upgrade Riak

```bash
sudo rpm -Uvh <riak_package_name>.rpm
```

4\. Restart Riak

```bash
riak start
```

5\. Verify that Riak is running the new version

```bash
riak-admin status
```


6\. Wait for the `riak_kv` service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```

Replace `<target_node>` with the node which you have just upgraded
(e.g. `riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```

While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

<div class="note">
<div class="title">Note for Secondary Index users</div>
If you use Riak's Secondary Indexes and are upgrading from a version
prior to Riak version 1.3.1, you need to reformat the indexes using the
<code>[[riak-admin reformat-indexes|riak-admin Command
Line#reformat-indexes]]</code> command. More details about reformatting
indexes are available in the [release
notes](https://github.com/basho/riak/blob/master/RELEASE-NOTES.md).
</div>


## Solaris/OpenSolaris

The following example demonstrates upgrading a Riak node that has been
installed with the Solaris/OpenSolaris packages provided by Basho.

1\. Stop Riak

```bash
riak stop
```

<div class="note">
<div class="title">Note on SMF</div>
If you are using the service management facility (SMF) to manage Riak,
you will have to stop Riak via <code>svcadm</code> instead of using
<code>riak stop</code>:

```bash
sudo svcadm disable riak
```
</div>

2\. Back up Riak's `/etc` and `/data` directories

```bash
sudo gtar -czf riak_backup.tar.gz /opt/riak/data /opt/riak/etc
```

3\. Uninstall Riak

```bash
sudo pkgrm BASHOriak
```

4\. Install the new version of Riak

```bash
sudo pkgadd -d <riak_package_name>.pkg
```

4\. Restart Riak

```bash
riak start
```

<div class="note">If you are using the SMF you should start Riak via <code>svcadm</code>:

<code>sudo svcadm enable riak</code>
</div>

5\. Verify that Riak is running the new version

```bash
riak-admin status
```

6\. Wait for the `riak_kv` service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```

`<target_node>` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```

While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

<div class="note">
<div class="title">Note for Secondary Index users</div>
If you use Riak's Secondary Indexes and are upgrading from a version
prior to Riak version 1.3.1, you need to reformat the indexes using the
<code>[[riak-admin reformat-indexes|riak-admin Command
Line#reformat-indexes]]</code> command. More details about reformatting
indexes are available in the [release
notes](https://github.com/basho/riak/blob/master/RELEASE-NOTES.md).
</div>


## Rolling Upgrade to Enterprise

1. Back up your `etc` (`app.config` and `vm.args`) and `data`
   directories.
2. Shutdown the node you are going to upgrade.
3. Uninstall your Riak package.
4. Install the `riak_ee` package.
5. A standard package uninstall should not have removed your data
   directories. If it did, move your backup to where the data directory
   should be.
6. Copy any customizations from your backed-up `vm.args` to the
   `riak_ee` installed `vm.args` file, these files may be identical.
7. The `app.config` file from `riak_ee` will be significantly different
   from your backed-up file. While it will contain all of the same
   sections as your original, it will have many new ones. Copy the
   customizations from your original `app.config` file into the
   appropriate sections in the new one. Ensure that the following
   sections are present in `app.config`:
  * `riak_core` --- the `cluster_mgr` setting must be present. See [[MDC
    v3 Configuration|Multi Data Center Replication v3 Configuration]]
    for more information.
  * `riak_repl` --- See [[MDC v3 Configuration|Multi Data Center
    Replication v3 Configuration]] for more information.
  * `riak_jmx` --- See [[JMX Monitoring]] for more information.
  * `snmp` --- See [[SNMP]] for more information.
8. Start Riak on the upgraded node.

## Basho Patches

After upgrading, you should ensure that any custom patches contained in
the `basho-patches` directory are examined to determine their
application to the upgraded version. If you find that patches no longer
apply to the upgraded version, you should remove them from the
`basho-patches` directory prior to operating the node in production.

The following table lists locations of the `basho-patches` directory for
each supported operating system:

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>CentOS &amp; RHEL Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><code>/usr/lib64/riak/lib/basho-patches</code></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Debian &amp; Ubuntu Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><code>/usr/lib/riak/lib/basho-patches</code></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>FreeBSD</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><code>/usr/local/lib/riak/lib/basho-patches</code></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>SmartOS</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><code>/opt/local/lib/riak/lib/basho-patches</code></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Solaris 10</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><code>/opt/riak/lib/basho-patches</code></p>
</td>
</tr>
</tbody>
</table>

## Riaknostic

It is a good idea to also verify some basic configuration and general
health of the Riak node after upgrading by using Riak's built-in
diagnostic utility Riaknostic.

Ensure that Riak is running on the node, and issue the following
command:

```bash
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal
node operation.
