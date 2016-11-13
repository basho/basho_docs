---
title: "Upgrading a Cluster"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Upgrading a Cluster"
    identifier: "upgrading_cluster"
    weight: 102
    parent: "upgrading"
toc: true
aliases:
  - /riak/2.2.0/ops/upgrading/rolling-upgrades/
  - /riak/kv/2.2.0/ops/upgrading/rolling-upgrades/
---

[production checklist]: /riak/kv/2.2.0/setup/upgrading/checklist
[use admin riak control]: /riak/kv/2.2.0/using/admin/riak-control
[use admin commands]: /riak/kv/2.2.0/using/admin/commands
[use admin riak-admin]: /riak/kv/2.2.0/using/admin/riak-admin
[usage secondary-indexes]: /riak/kv/2.2.0/developing/usage/secondary-indexes
[release notes]: /riak/kv/2.2.0/release-notes
[riak enterprise]: http://basho.com/products/riak-kv/
[cluster ops mdc]: /riak/kv/2.2.0/using/cluster-operations/v3-multi-datacenter
[config v3 mdc]: /riak/kv/2.2.0/configuring/v3-multi-datacenter
[jmx monitor]: /riak/kv/2.2.0/using/reference/jmx
[snmp]: /riak/kv/2.2.0/using/reference/snmp

## Overview

Riak KV nodes negotiate with each other to determine supported operating modes. This allows clusters containing mixed-versions of Riak KV to properly interoperate without special configuration, and simplifies rolling upgrades.

### General Process

For every node in the cluster:

1.  Stop Riak KV.
2.  Back up Riak's `/etc` and `/data` directories.
3.  Upgrade Riak KV.
4.  Start Riak KV.
5.  Verify Riak KV is running the upgraded version.
6.  Wait for the `riak_kv` service to start.

Before starting the rolling upgrade process on your cluster, check out the [Upgrading Riak KV: Production Checklist][production checklist] page, which covers details and questions to consider while upgrading.


## Data File Format Changes

[Riak KV 2.2][release notes] introduces on-disk data file format changes that can impact the upgrade/downgrade process:

* Changes to active anti-entropy related to inconsistent hashing.
* Upgrading to Solr 4.10.4 for Riak Search.

### Components That Complicate Downgrades

| Feature | automatic | required | upgrade only | Notes |
|:---|:---:|:---:|:---:|:--- |
|Migration to Solr 4.10.4 |✔ | ✔| | Applies to all clusters using Riak Search.
| Active Anti-Entropy file format changes | ✔ |  | | Can be opted out using a capability.
| LZ4 Compression in LevelDB | | | ✔ |
| Global Expiration in LevelDB | | | ✔ |
| HyperLogLog Data Type | | |✔| On downgrade data written in HLL format is unreadable.|
 

### When Downgrading is No Longer an Option

If you decide to upgrade to version 2.2, you can still downgrade your cluster to an earlier version of Riak KV if you wish, unless you perform one of the following actions in your cluster:

* Enable LZ4 Compression in LevelDB
* Enable Global Expiration in LevelDB

If you use other new features, such as the HyperLogLog data type, you can still downgrade your cluster, but you will no longer be able to use those features or access data in new formats after the downgrade.


## Debian/Ubuntu

The following examples demonstrate upgrading a Riak KV node that has been installed with the Debian/Ubuntu packages provided by Basho.

1\. Stop Riak KV:

```bash
riak stop
```

2\. Back up the Riak KV node's `/etc` and `/data` directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak /usr/lib/riak/lib/basho-patches
```

3\. Remove the `/basho-patches` directory:

```bash
sudo rm -rf /usr/lib/riak/lib/basho-patches*
```

4\. Upgrade Riak KV:

```bash
sudo dpkg -i <riak_package_name>.deb
```

5\. Keep AAE trees in a format to facilitate downgrade (Optional):

* To keep AAE trees in a format that will facilitate a downgrade, the capability override should be in the `riak_kv` proplist of the `advanced.config` file.

   ```erlang
   {riak_kv, [
     {override_capability, [
       {object_hash_version, [{use, legacy}] }
     ]}
   ]}
   ```

6\. Restart Riak KV:

```bash
riak start
```

7\. Verify Riak KV is running the new version:

```bash
riak version
```

8\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target_node«
```

* `»target_node«` is the node which you have just upgraded (e.g.
`riak@192.168.1.11`)

9\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

* While the node was offline, other nodes may have accepted writes on its behalf. This data is transferred to the node when it becomes available.

10\. Repeat the process for the remaining nodes in the cluster.


## RHEL/CentOS

The following example demonstrates upgrading a Riak KV node that has been installed with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak KV:

```bash
riak stop
```

2\. Back up Riak KV's `/etc` and `/data` directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak /usr/lib64/riak/lib/basho-patches
```

3\. Remove the `/basho-patches` directory:

```bash
sudo rm -rf /usr/lib64/riak/lib/basho-patches/*
```

4\. Upgrade Riak KV:

```bash
sudo rpm -Uvh <riak_package_name>.rpm
```

5\. Keep AAE trees in a format to facilitate downgrade (Optional):

* To keep AAE trees in a format that will facilitate a downgrade, the capability override should be in the `riak_kv` proplist of the `advanced.config` file.

   ```erlang
   {riak_kv, [
     {override_capability, [
       {object_hash_version, [{use, legacy}] }
     ]}
   ]}
   ```

6\. Restart Riak KV:

```bash
riak start
```

7\. Verify that Riak KV is running the new version:

```bash
riak version
```

8\. Wait for the `riak_kv` service to start:

```bash
riak-admin wait-for-service riak_kv »target_node«
```

* `»target_node«` is the node which you have just upgraded (e.g.
riak@192.168.1.11)

9\. Wait for any hinted handoff transfers to complete:

```bash
riak-admin transfers
```

* While the node was offline, other nodes may have accepted writes on its
behalf. This data is transferred to the node when it becomes available.

10\. Repeat the process for the remaining nodes in the cluster.

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

It is a good idea to also verify some basic configuration and general health of the Riak KV node after upgrading by using Riak KV's built-in diagnostic utility Riaknostic.

Ensure that Riak KV is running on the node, and issue the following command:

```bash
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal node operation.
