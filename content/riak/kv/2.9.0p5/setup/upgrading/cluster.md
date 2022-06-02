---
title: "Upgrading a Cluster"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Upgrading a Cluster"
    identifier: "upgrading_cluster"
    weight: 102
    parent: "upgrading"
toc: true
version_history:
  in: "2.0.0-2.99.999"
aliases:
  - /riak/2.9.0p5/ops/upgrading/rolling-upgrades/
  - /riak/kv/2.9.0p5/ops/upgrading/rolling-upgrades/
  - /riak/2.9.0/ops/upgrading/rolling-upgrades/
  - /riak/kv/2.9.0/ops/upgrading/rolling-upgrades/
---

[production checklist]: {{<baseurl>}}riak/kv/2.9.0p5/setup/upgrading/checklist
[use admin riak control]: {{<baseurl>}}riak/kv/2.9.0p5/using/admin/riak-control
[use admin commands]: {{<baseurl>}}riak/kv/2.9.0p5/using/admin/commands
[use admin riak-admin]: {{<baseurl>}}riak/kv/2.9.0p5/using/admin/riak-admin
[usage secondary-indexes]: {{<baseurl>}}riak/kv/2.9.0p5/developing/usage/secondary-indexes
[release notes]: https://github.com/basho/riak/blob/master/RELEASE-NOTES.md
[riak enterprise]: http://basho.com/products/riak-kv/
[cluster ops mdc]: {{<baseurl>}}riak/kv/2.9.0p5/using/cluster-operations/v3-multi-datacenter
[config v3 mdc]: {{<baseurl>}}riak/kv/2.9.0p5/configuring/v3-multi-datacenter
[jmx monitor]: {{<baseurl>}}riak/kv/2.9.0p5/using/reference/jmx
[snmp]: {{<baseurl>}}riak/kv/2.9.0p5/using/reference/snmp

{{% note title="Note on upgrading Riak KV from older versions" %}}
Riak KV upgrades are tested and supported for two feature release versions.
For example, upgrades from 1.1.x to 1.3.x are tested and supported,
while upgrades from 1.1.x to 1.4.x are not. When upgrading to a new
version of Riak KV that is more than two feature releases ahead, we
recommend first upgrading to an intermediate version. For example, in an
upgrade from 1.1.x to 1.4.x, we recommend upgrading from 1.1.x to 1.3.x
before upgrading to 1.4.x.

If you run [Riak Control]({{<baseurl>}}riak/kv/2.9.0p5/using/admin/riak-control), you should disable it during the rolling upgrade process.
{{% /note %}}

Riak KV nodes negotiate with each other to determine supported
operating modes. This allows clusters containing mixed-versions of Riak KV
to properly interoperate without special configuration, and simplifies
rolling upgrades.

Before starting the rolling upgrade process on your cluster, check out the [Upgrading Riak KV: Production Checklist][production checklist] page, which covers details and questions to consider while upgrading.

## Debian/Ubuntu

The following example demonstrates upgrading a Riak KV node that has been
installed with the Debian/Ubuntu packages provided by Basho.

1\. Stop Riak KV:

```bash
riak stop
```

2\. Back up the Riak KV node's `/etc` and `/data` directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Upgrade Riak KV:

```bash
sudo dpkg -i <riak_package_name>.deb
```

4\. Restart Riak KV:

```bash
riak start
```

5\. Verify Riak KV is running the new version:

```bash
riak version
```

6\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target_node«
```

* `»target_node«` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

* While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster.


## RHEL/CentOS

The following example demonstrates upgrading a Riak KV node that has been
installed with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak KV:

```bash
riak stop
```

2\. Back up Riak KV's `/etc` and `/data` directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Upgrade Riak KV:

```bash
sudo rpm -Uvh <riak_package_name>.rpm
```

4\. Restart Riak KV:

```bash
riak start
```

5\. Verify that Riak KV is running the new version:

```bash
riak version
```


6\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target_node«
```

* `»target_node«` is the node which you have just upgraded (e.g.
riak@192.168.1.11)

7\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

* While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster.


## Solaris/OpenSolaris

The following example demonstrates upgrading a Riak KV node that has been
installed with the Solaris/OpenSolaris packages provided by Basho.

1\. Stop Riak KV:

```bash
riak stop
```

{{% note %}}
If you are using the service management facility (SMF) to manage Riak KV,
you will have to stop Riak KV via `svcadm` instead of using `riak stop`:

```bash
sudo svcadm disable riak
```
{{% /note %}}


2\. Back up Riak KV's `/etc` and `/data` directories:

```bash
sudo gtar -czf riak_backup.tar.gz /opt/riak/data /opt/riak/etc
```

3\. Uninstall Riak KV:

```bash
sudo pkgrm BASHOriak
```

4\. Install the new version of Riak KV:

```bash
sudo pkgadd -d <riak_package_name>.pkg
```

4\. Restart Riak KV:

```bash
riak start
```

{{% note %}}
If you are using the service management facility (SMF) to manage Riak KV,
you will have to start Riak KV via `svcadm` instead of using `riak start`:

```bash
sudo svcadm enable riak
```
{{% /note %}}

5\. Verify that Riak KV is running the new version:

```bash
riak version
```

6\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target_node«
```

`»target_node«` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster.


## Rolling Upgrade to Enterprise

If you would like to upgrade an existing Riak KV cluster to a commercially
supported [Riak KV Enterprise][riak enterprise] cluster with [multi-datacenter replication][cluster ops mdc], undertake the following steps:

1. Shut down the node you are going to upgrade.
2. Back up your `etc` (app.config and vm.args) and `data`
directories.
3. Uninstall your Riak KV package.
4. Install the `riak_ee` package.
5. A standard package uninstall should not have removed your data
   directories. If it did, move your backup to where the data directory
   should be.
6. Copy any customizations from your backed-up vm.args to the
   `riak_ee` installed vm.args file, these files may be identical.
7. The app.config file from `riak_ee` will be significantly different from your backed-up file. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original app.config file into the appropriate sections in the new one. Ensure that the following sections are present in app.config:
  * `riak_core` --- the `cluster_mgr` setting must be present. See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_repl` --- See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_jmx` --- See [JMX Monitoring][jmx monitor] for more information.
  * `snmp` --- See [SNMP][snmp] for more information.
8. Start Riak KV on the upgraded node.

## Basho Patches

After upgrading, you should ensure that any custom patches contained in
the `basho-patches` directory are examined to determine their
application to the upgraded version. If you find that patches no longer
apply to the upgraded version, you should remove them from the
`basho-patches` directory prior to operating the node in production.

The following lists locations of the `basho-patches` directory for
each supported operating system:

- CentOS & RHEL Linux: `/usr/lib64/riak/lib/basho-patches`
- Debian & Ubuntu Linux: `/usr/lib/riak/lib/basho-patches`
- FreeBSD: `/usr/local/lib/riak/lib/basho-patches`
- SmartOS: `/opt/local/lib/riak/lib/basho-patches`
- Solaris 10: `/opt/riak/lib/basho-patches`

## Riaknostic

It is a good idea to also verify some basic configuration and general
health of the Riak KV node after upgrading by using Riak KV's built-in
diagnostic utility Riaknostic.

Ensure that Riak KV is running on the node, and issue the following
command:

```bash
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal
node operation.
