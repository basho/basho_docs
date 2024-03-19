---
title: "Upgrading to Riak KV 2.0.9"
description: ""
project: "riak_kv"
project_version: "2.0.9"
lastmod: 2017-03-15T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.0.9:
    name: "Upgrading to 2.0.9"
    identifier: "upgrading_version"
    weight: 101
    parent: "upgrading"
toc: true
aliases:
  - /riak/2.0.9/upgrade-v20/
  - /riak/kv/2.0.9/ops/upgrading/rolling-upgrades/
  - /riak/kv/2.0.9/ops/upgrading/rolling-upgrades/
  - /riak/kv/2.0.9/setup/upgrading/cluster/

---

[production checklist]: {{<baseurl>}}riak/kv/2.0.9/setup/upgrading/checklist
[use admin riak control]: {{<baseurl>}}riak/kv/2.0.9/using/admin/riak-control
[use admin commands]: {{<baseurl>}}riak/kv/2.0.9/using/admin/commands
[use admin riak-admin]: {{<baseurl>}}riak/kv/2.0.9/using/admin/riak-admin
[usage secondary-indexes]: {{<baseurl>}}riak/kv/2.0.9/developing/usage/secondary-indexes
[release notes]: {{<baseurl>}}riak/kv/2.0.9/release-notes
[riak enterprise]: http://basho.com/products/riak-kv/
[cluster ops mdc]: {{<baseurl>}}riak/kv/2.0.9/using/cluster-operations/v3-multi-datacenter
[config v3 mdc]: {{<baseurl>}}riak/kv/2.0.9/configuring/v3-multi-datacenter
[jmx monitor]: {{<baseurl>}}riak/kv/2.0.9/using/reference/jmx
[snmp]: {{<baseurl>}}riak/kv/2.0.9/using/reference/snmp
[Release Notes]: {{<baseurl>}}riak/kv/2.0.9/release-notes

## Overview

You can upgrade one node or your whole cluster to Riak KV 2.0.9 by following the instructions below.

{{% note title="Tip" %}} KV nodes negotiate with each other to determine supported operating modes. This allows clusters containing mixed-versions of Riak KV to interoperate without special configuration, and simplifies rolling upgrades.
{{% /note %}}

### General Process

For every node in the cluster:

1. Stop Riak KV.
1. Back up the Riak /etc, /data, and /basho-patches directories.
1. Remove your /basho-patches directory.
1. Upgrade Riak KV.
    * If you are upgrading from OSS to EE, uninstall your OSS KV package before upgrading.
1. (Optional) If you would like to potentially downgrade at some point, update your advanced.config file to opt-out of the AAE updates.
1. If you're upgrading from OSS to EE, apply your customized settings to vm.args and app.config
1. Start Riak KV.
1. Verify Riak KV is running the upgraded version.
1. Wait for the `riak_kv` service to start.
1. Wait for any hinted handoffs to complete.

Before starting the rolling upgrade process on your cluster, check out the [Upgrading Riak KV: Production Checklist][production checklist], which covers details and questions to consider before upgrading.

## Data File Format Changes

[Riak KV 2.0.9][release notes] introduces on-disk data file format changes that can impact the upgrade/downgrade process:

* Changes to active anti-entropy related to inconsistent hashing.

### Components That Complicate Downgrades

We do our best to make all features that change data formats on disk opt-in; however, some features may be introduced that we either believe are so important that we automatically opt-in users on upgrade or there is no way to provide direct backward compatibility. Downgrading environments with these features can require more effort or might not be possible.

* **Automatic** features alter the data format on disk, but are considered important enough for users to be automatically opted-in.
* **Required** features must be accepted as a part of the upgrade. Internal Solr version upgrades that change the data format on disk are an example of a required feature upgrade.
* **One Way** features, when enabled, will make a clean downgrade of a cluster impossible.

| Feature | Automatic | Required | One Way | Notes |
|:---|:---:|:---:|:---:|:--- |
| Active anti-entropy file format changes | ✔ |  | | Can opt-out using a capability.

## Upgrading process

1\. Stop Riak KV on the node you are going to upgrade:

```bash
riak stop
```

2\. Back up your /etc (app.config and vm.args), /data, and /basho-patches directories.

```RHEL/CentOS
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak /usr/lib64/riak/lib/basho-patches
```

```Ubuntu
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak /usr/lib/riak/lib/basho-patches
```

3\. Remove your /basho-patches directory:

```RHEL/CentOS
sudo rm -rf /usr/lib64/riak/lib/basho-patches/*
```

```Ubuntu
sudo rm -rf /usr/lib/riak/lib/basho-patches*
```

4\. Upgrade Riak KV:

{{% note title="Upgrading to KV Enterprise Edition" %}}
If you are upgrading from Riak KV OSS to Riak KV EE, you must uninstall your Riak KV package right now, before you can install the EE version.
{{% /note %}}

```RHEL/CentOS
sudo rpm -Uvh »riak_package_name«.rpm
```

```Ubuntu
sudo dpkg -i »riak_package_name«.deb
```

5\. (**Optional**) If you would like to keep your AAE trees in a format that will facilitate downgrading, the capability override should be in the `riak_kv` proplist of the advanced.config file:

   ```advanced.config
   {riak_kv, [
     {override_capability, [
       {object_hash_version, [{use, legacy}] }
     ]}
   ]}
   ```

5.5\. (**EE Only**)If you are upgrading from Riak KV OSS to Riak KV EE, you must perform the following steps before moving on:

* A standard package uninstall should not have removed your data directories, but if it did, move your backup to where the data directory should be.
* Then copy any customizations from your backed-up vm.args to the `riak_ee` installed vm.args file (these files may be identical).
* The app.config file from `riak_ee` will be significantly different from your backed-up file. It will have many new sections along with the original ones. Copy the customizations from your original app.config file into the appropriate sections in the new one. Ensure that the following sections are present in app.config:
  * `riak_core` --- the `cluster_mgr` setting must be present. See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_repl` --- See [MDC v3 Configuration][config v3 mdc] for more information.
  * `riak_jmx` --- See [JMX Monitoring][jmx monitor] for more information.
  * `snmp` --- See [SNMP][snmp] for more information.

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

* While the node was offline, other nodes may have accepted writes on its behalf. This data is transferred to the node when it becomes available.

10\. Repeat the process for the remaining nodes in the cluster.

### Basho Patches

After upgrading, you should ensure that any custom patches contained in the `basho-patches` directory are examined to determine their application to the upgraded version. You can find this information in the [Release Notes].

If you find that patches no longer apply to the upgraded version, you should remove them from the `basho-patches` directory prior to operating the node in production.

The following lists locations of the `basho-patches` directory for
each supported operating system:

- CentOS & RHEL Linux: `/usr/lib64/riak/lib/basho-patches`
- Debian & Ubuntu Linux: `/usr/lib/riak/lib/basho-patches`
- FreeBSD: `/usr/local/lib/riak/lib/basho-patches`
- SmartOS: `/opt/local/lib/riak/lib/basho-patches`
- Solaris 10: `/opt/riak/lib/basho-patches`

### Riaknostic

It is a good idea to also verify some basic configuration and general health of the Riak KV node after upgrading by using Riak KV's built-in diagnostic utility Riaknostic.

Ensure that Riak KV is running on the node, and issue the following command:

```bash
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal node operation.s