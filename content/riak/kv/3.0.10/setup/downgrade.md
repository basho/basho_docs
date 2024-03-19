---
title: "Downgrading"
description: ""
project: "riak_kv"
project_version: "3.0.10"
lastmod: 2022-05-30T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.10:
    name: "Downgrading"
    identifier: "downgrading"
    weight: 103
    parent: "setup_index"
toc: true
aliases:
  - /riak/3.0.10/ops/upgrading/rolling-downgrades/
  - /riak/kv/3.0.10/ops/upgrading/rolling-downgrades/
---

[rolling upgrade]: {{<baseurl>}}riak/kv/3.0.10/setup/upgrading/cluster
[config ref]: {{<baseurl>}}riak/kv/3.0.10/configuring/reference
[concept aae]: {{<baseurl>}}riak/kv/3.0.10/learn/concepts/active-anti-entropy/
[aae status]: {{<baseurl>}}riak/kv/3.0.10/using/admin/riak-admin/#aae-status

Downgrades of Riak KV are tested and supported for two feature release versions, with the general procedure being similar to that of a [rolling upgrade][rolling upgrade].

Depending on the versions involved in the downgrade, there are additional steps to be performed before, during, and after the upgrade on on each node. These steps are related to changes or new features that are not present in the downgraded version.

## Overview

For every node in the cluster:

1. Stop Riak KV.
2. Back up Riak's `etc` and `data` directories.
3. Downgrade the Riak KV.
4. Start Riak KV.
7. Monitor the reindex of the data.
8. Finalize process and restart Riak KV.

### Guidelines

* Riak control should be disabled throughout the rolling downgrade process.
* [Configuration Files][config ref] must be replaced with those of the version being downgraded to.

### Components That Complicate Downgrades

| Feature | automatic | required | Notes |
|:---|:---:|:---:|:---|
| Active Anti-Entropy file format changes | ✔ |  | Can be opted out using a [capability](#aae_tree_capability)

### When Downgrading is No Longer an Option

If you enabled LZ4 compression in LevelDB and/or enabled global expiration in LevelDB when you installed KV 3.0.10, you cannot downgrade.

## General Process

{{% note %}}
While the cluster contains mixed version members, if you have not set the cluster to use the legacy AAE tree format, you will see the `bad_version` error emitted to the log any time nodes with differing versions attempt to exchange AAE data (including AAE fullsync).

This is benign and similar to the `not_built` and `already_locked` errors which can be seen during normal AAE operation. These events will stop once the downgrade is complete.
{{% /note %}}

### Stop Riak KV

1\. Stop Riak KV:

```bash
riak stop
```
2\. Back up your Riak KV /etc and /data directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Downgrade Riak KV:

```RHEL/CentOS
sudo rpm -Uvh »riak_package_name«.rpm
```

```Ubuntu
sudo dpkg -i »riak_package_name«.deb
```

### Prepare to Re-index Solr Cores

5\. (**Optional**) You can increase the AAE operation concurrency and increase the number of build operations while lowering the build limit's interval. This will increase the speed at which the AAE trees are rebuilt.  However, if you have a latency sensitive application, you should adjust these settings with care.

```riak.conf
anti_entropy.concurrency_limit = 8
anti_entropy.tree.build_limit.number = 4
anti_entropy.tree.build_limit.per_timespan = 5m
```

### Start the node

6\. Start Riak KV:

```bash
riak start
```

### Monitor the reindex of the data

7\. Monitor the build and exchange progress using the `riak admin aae-status` command.

The **All** column shows how long it has been since a partition exchanged with all of its sibling replicas.  Consult the [`riak admin aae-status` documentation][aae status] for more information about the AAE status output.

Once `riak admin aae-status` shows values in the **All** column, the node will have successfully rebuilt all of the indexed data.

### Finalize process

8\. If you raised the concurrency AAE currency settings in riak.conf during **Step 5**, stop the node and remove the increased AAE thresholds.

9\. Verify that transfers have completed:

```bash
riak admin transfers
```

