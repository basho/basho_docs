---
title: "Upgrading a Cluster"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Upgrading a Cluster"
    identifier: "upgrading_cluster"
    weight: 102
    parent: "upgrading"
toc: true
aliases:
  - /riak/2.0.7/ops/upgrading/rolling-upgrades/
  - /riak/kv/2.0.7/ops/upgrading/rolling-upgrades/
canonical_link: "https://docs.basho.com/riak/kv/latest/setup/upgrading/cluster"
---

[use admin riak control]: /riak/kv/2.0.7/using/admin/riak-control
[use admin commands]: /riak/kv/2.0.7/using/admin/commands
[use admin riak-admin]: /riak/kv/2.0.7/using/admin/riak-admin
[usage secondary-indexes]: /riak/kv/2.0.7/developing/usage/secondary-indexes
[release notes]: https://github.com/basho/riak/blob/master/RELEASE-NOTES.md
[riak enterprise]: http://basho.com/products/riak-kv/
[cluster ops mdc]: /riak/kv/2.0.7/using/cluster-operations/v3-multi-datacenter
[config v3 mdc]: /riak/kv/2.0.7/configuring/v3-multi-datacenter



>**Note on upgrading Riak KV from older versions**
>
>Riak KV upgrades are tested and supported for two feature release versions.
For example, upgrades from 1.1.x to 1.3.x are tested and supported,
while upgrades from 1.1.x to 1.4.x are not. When upgrading to a new
version of Riak KV that is more than two feature releases ahead, we
recommend first upgrading to an intermediate version. For example, in an
upgrade from 1.1.x to 1.4.x, we recommend upgrading from 1.1.x to 1.3.x
before upgrading to 1.4.x.
>
>If you run [Riak Control][use admin riak control], you should disable it during the rolling upgrade process.

Riak KV nodes negotiate with each other to determine supported
operating modes. This allows clusters containing mixed-versions of Riak KV
to properly interoperate without special configuration, and simplifies
rolling upgrades.

In Riak KV versions 1.4 and earlier, users were required to disable new features during the rolling upgrade process and then enable them after all nodes were upgraded. In versions 2.0 and up, this is handled automatically by Riak KV.

## Debian/Ubuntu

The following example demonstrates upgrading a Riak KV node that has been
installed with the Debian packages provided by Basho.

1\. Stop Riak KV

```bash
riak stop
```


2\. Back up the Riak node's `/etc` and `/data` directories

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```


3\. Upgrade Riak KV

```bash
sudo dpkg -i <riak_package_name>.deb
```


4\. Restart Riak KV

```bash
riak start
```


5\. Verify Riak KV is running the new version

```bash
riak-admin status
```


6\. Wait for the `riak_kv` service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```

* `<target_node>` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```

* While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

>**Note for secondary index (2i) users**
>
>If you use Riak KV's [secondary indexes][usage secondary-indexes] and are
upgrading from a version prior to Riak KV version 1.3.1, you need to
reformat the indexes using the [`riak-admin reformat-indexes`][use admin riak-admin] command. More details about reformatting indexes are available in the [release notes][release notes].


## RHEL/CentOS

The following example demonstrates upgrading a Riak KV node that has been
installed with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak KV

```bash
riak stop
```

2\. Back up Riak KV's `/etc` and `/data` directories

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Upgrade Riak KV

```bash
sudo rpm -Uvh <riak_package_name>.rpm
```

4\. Restart Riak KV

```bash
riak start
```

5\. Verify that Riak KV is running the new version

```bash
riak-admin status
```


6\. Wait for the `riak_kv` service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```

* `<target_node>` is the node which you have just upgraded (e.g.
riak@192.168.1.11)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```

* While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

>**Note for secondary index (2i) users**
>
>If you use Riak KV's [secondary indexes][usage secondary-indexes] and are
upgrading from a version prior to Riak KV version 1.3.1, you need to
reformat the indexes using the [`riak-admin reformat-indexes`][use admin riak-admin] command. More details about reformatting indexes are available in the [release notes][release notes].

## Solaris/OpenSolaris

The following example demonstrates upgrading a Riak KV node that has been
installed with the Solaris/OpenSolaris packages provided by Basho.

1\. Stop Riak KV

```bash
riak stop
```

>If you are using the service management facility (SMF) to manage Riak,
you will have to stop Riak via `svcadm` instead of using `riak
stop`:
>
>`sudo svcadm disable riak`


2\. Back up Riak KV's `/etc` and `/data` directories

```bash
sudo gtar -czf riak_backup.tar.gz /opt/riak/data /opt/riak/etc
```

3\. Uninstall Riak KV

```bash
sudo pkgrm BASHOriak
```

4\. Install the new version of Riak KV

```bash
sudo pkgadd -d <riak_package_name>.pkg
```

4\. Restart Riak KV

```bash
riak start
```

>If you are using the SMF you should start Riak via `svcadm`:
>
>`sudo svcadm enable riak`

5\. Verify that Riak KV is running the new version

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

>**Note for secondary index (2i) users**
>
>If you use Riak KV's [secondary indexes][usage secondary-indexes] and are
upgrading from a version prior to Riak KV version 1.3.1, you need to
reformat the indexes using the [`riak-admin reformat-indexes`][use admin riak-admin] command. More details about reformatting indexes are available in the [release notes][release notes].

## Rolling Upgrade to Enterprise

If you would like to upgrade an existing Riak KV cluster to a commercially
supported [Riak KV Enterprise][riak enterprise] cluster with [multi-datacenter replication][cluster ops mdc], undertake the following steps:

1. Back up your `etc` (`app.config` and `vm.args`) and `data`
directories.
2. Shut down the node you are going to upgrade.
3. Uninstall your Riak KV package.
4. Install the `riak_ee` package.
5. A standard package uninstall should not have removed your data
   directories. If it did, move your backup to where the data directory
   should be.
6. Copy any customizations from your backed-up `vm.args` to the
   `riak_ee` installed `vm.args` file, these files may be identical.
7. The `app.config` file from `riak_ee` will be significantly different from your backed-up file. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original `app.config` file into the appropriate sections in the new one. Ensure that the following sections are present in `app.config`:
  * `riak_core` --- the `cluster_mgr` setting must be present. See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_repl` --- See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_jmx` --- See [JMX Monitoring](../../../using/reference/jmx) for more information.
  * `snmp` --- See [SNMP](../../../using/reference/snmp) for more information.
8. Start Riak KV on the upgraded node.

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

## Riaknostic

It is a good idea to also verify some basic configuration and general
health of the Riak node after upgrading by using Riak KV's built-in
diagnostic utility Riaknostic.

Ensure that Riak KV is running on the node, and issue the following
command:

```bash
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal
node operation.
