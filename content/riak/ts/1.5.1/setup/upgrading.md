---
title: "Upgrading Riak TS"
description: "Upgrading from Riak TS 1.4.0 to 1.5.1"
menu:
  riak_ts-1.5.1:
    name: "Upgrade"
    identifier: "upgrade"
    weight: 200
    parent: "setup"
project: "riak_ts"
project_version: "1.5.1"
toc: true
version_history:
  present_from: "1.4.0+"
aliases:
    - /riakts/1.5.1/setup/upgrading/
    - /riakts/1.5.1/upgrading/
---

[use admin commands]: {{<baseurl>}}riak/kv/2.2.0/using/admin/commands
[use admin riak-admin]: {{<baseurl>}}riak/kv/2.2.0/using/admin/riak-admin
[usage secondary-indexes]: {{<baseurl>}}riak/kv/2.2.0/developing/usage/secondary-indexes
[riak ts enterprise]: http://basho.com/products/riak-ts/
[cluster ops mdc]: {{<baseurl>}}riak/kv/2.2.0/using/cluster-operations/v3-multi-datacenter
[config v3 mdc]: {{<baseurl>}}riak/kv/2.2.0/configuring/v3-multi-datacenter
[jmx monitor]: {{<baseurl>}}riak/kv/2.2.0/using/reference/jmx
[snmp]: {{<baseurl>}}riak/kv/2.2.0/using/reference/snmp


{{% note title="**CAUTION**" %}}
At the moment, there is no downgrade path between Riak TS 1.5.1 and Riak TS 1.4.0. Any data written to TS 1.5.1 will not be readable by TS 1.4.0. Please perform backups before you upgrade and be very sure you are ready to upgrade entirely before doing so.
{{% /note %}}

This page walks you through the process of upgrading to Riak TS 1.5.1 from Riak TS 1.4.0. Find your operating system below:

* [Debian & Ubuntu](#debianubuntu)
* [RHEL & CentOS](#rhelcentos)

Riak TS nodes negotiate with each other to determine supported
operating modes. This allows clusters containing mixed-versions of Riak TS to properly interoperate without special configuration, and simplifies rolling upgrades.


## Debian/Ubuntu

The following example demonstrates upgrading a Riak TS node that has been installed with the Debian/Ubuntu packages provided by Basho.

1\. Stop Riak TS:

```bash
riak stop
```

2\. Back up your Riak TS node's /etc and /data directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Upgrade Riak TS:

```bash
sudo dpkg -i »riakts_package_name«.deb
```

4\. Restart Riak TS:

{{% note %}}
Before restarting Riak TS, check your riak.conf file and verify that your settings are configured as expected.
{{% /note %}}

```bash
riak start
```

5\. Verify Riak TS is running the new version:

```bash
riak version
```

6\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target node«
```

* `»target node«` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

7\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

{{% note %}}
While the node was offline, other nodes may have accepted writes on its behalf. This data is transferred to the node when it becomes available.
{{% /note %}}

8\. Repeat the process for the remaining nodes in the cluster.


## RHEL/CentOS

The following example demonstrates upgrading a Riak TS node that has been installed with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak TS:

```bash
riak stop
```

2\. Back up your Riak TS node's /etc and /data directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Upgrade Riak TS:

```bash
sudo rpm -Uvh »riakts_package_name«.rpm
```

4\. Restart Riak TS:

{{% note %}}
Before restarting Riak TS, check your riak.conf file and verify that your settings are configured as expected.
{{% /note %}}

```bash
riak start
```

5\. Verify that Riak TS is running the new version:

```bash
riak version
```

6\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target node«
```

* `»target node«` is the node which you have just upgraded (e.g.
riak@192.168.1.11)

7\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

{{% note %}}
While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.
{{% /note %}}

8\. Repeat the process for the remaining nodes in the cluster.


## Rolling Upgrade to Enterprise

If you would like to upgrade an existing Riak TS cluster to a [commercially supported Riak TS Enterprise][riak ts enterprise] cluster with multi-datacenter replication, undertake the following steps:

1. Back up your /etc (riak.conf and vm.args) and /data
directories.
2. Shut down the node you are going to upgrade.
3. Uninstall your Riak TS package.
4. Install the `riak-ts-ee` package.
5. A standard package uninstall should not have removed your data
   directories. If it did, move your backup to where the data directory
   should be.
6. Copy any customizations from your backed-up vm.args file to the
   `riak-ts-ee` installed vm.args file, these files may be identical.
7. The riak.conf file from `riak-ts-ee` will be significantly different from your backed-up file. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original riak.conf file into the appropriate sections in the new one. Ensure that the following sections are present in riak.conf:
  * `riak_core` --- the `cluster_mgr` setting must be present. See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_repl` --- See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_jmx` --- See [JMX Monitoring][jmx monitor] for more information.
  * `snmp` --- See [SNMP][snmp] for more information.
8. Start Riak TS on the upgraded node.

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
