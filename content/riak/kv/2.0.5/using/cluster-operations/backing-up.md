---
title: "Backing Up"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Backing Up"
    identifier: "cluster_operations_backing_up"
    weight: 106
    parent: "managing_cluster_operations"
toc: true
aliases:
  - /riak/2.0.5/ops/running/backups
  - /riak/kv/2.0.5/ops/running/backups
---

[concept clusters]: {{<baseurl>}}riak/kv/2.0.5/learn/concepts/clusters
[config reference]: {{<baseurl>}}riak/kv/2.0.5/configuring/reference
[plan backend leveldb]: {{<baseurl>}}riak/kv/2.0.5/setup/planning/backend/leveldb
[plan backend bitcask]: {{<baseurl>}}riak/kv/2.0.5/setup/planning/backend/bitcask
[use ref strong consistency]: {{<baseurl>}}riak/kv/2.0.5/using/reference/strong-consistency
[concept aae]: {{<baseurl>}}riak/kv/2.0.5/learn/concepts/active-anti-entropy/
[aae read repair]: {{<baseurl>}}riak/kv/2.0.5/learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy

Riak KV is a [clustered][concept clusters] system built to survive a wide range of failure scenarios, including the loss of nodes due to network or hardware failure. Although this is one of Riak KV's core strengths, it cannot withstand all failure scenarios.

Backing up data (duplicating the database on a different long-term storage system) is a common approach to mitigating potential failure scenarios.

This page covers how to perform backups of Riak KV data.

## Overview

Riak KV backups can be performed using operating system features or filesystems that support snapshots, such as LVM or ZFS, or by using tools like rsync or tar.

Choosing your Riak KV backup strategy will depend on your already-established backup methodologies and the backend configuration of your nodes.

The basic process for getting a backup of Riak KV from a node is as follows:

1. Stop Riak KV with `riak stop`.
2. Backup the appropriate data, ring, and configuration directories.
3. Start Riak KV.

Downtime of a node can be significantly reduced by using an OS feature or filesystem that supports snapshotting.

{{% note title="Backups and eventual consistency" %}}
Due to Riak KV's eventually consistent nature, backups can become slightly inconsistent from node to node.

Data could exist on some nodes and not others at the exact time a backup is made. Any inconsistency will be corrected once a backup is restored, either by Riak's [active anti-entropy]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/active-anti-entropy/) processes or when the object is read, via [read repair]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy).
{{% /note %}}

## OS-Specific Directory Locations

The default Riak KV data, ring, and configuration directories for each of the supported operating systems is as follows:

#### Debian and Ubuntu

Data | Directory
:----|:---------
Bitcask | `/var/lib/riak/bitcask`
LevelDB | `/var/lib/riak/leveldb`
Ring | `/var/lib/riak/ring`
Configuration | `/etc/riak`
Cluster Metadata | `/var/lib/riak/cluster_meta`
Search | `/var/lib/riak/yz`
Strong consistency | `/var/lib/riak/ensembles`

#### Fedora and RHEL

Data | Directory
:----|:---------
Bitcask | `/var/lib/riak/bitcask`
LevelDB | `/var/lib/riak/leveldb`
Ring | `/var/lib/riak/ring`
Configuration | `/etc/riak`
Cluster Metadata | `/var/lib/riak/cluster_meta`
Search | `/var/lib/riak/yz`
Strong consistency | `/var/lib/riak/ensembles`

#### FreeBSD

Data | Directory
:----|:---------
Bitcask | `/var/db/riak/bitcask`
LevelDB | `/var/db/riak/leveldb`
Ring | `/var/db/riak/ring`
Configuration | `/usr/local/etc/riak`
Cluster Metadata | `/var/db/riak/cluster_meta`
Search | `/var/db/riak/yz`
Strong consistency | `/var/db/riak/ensembles`

#### OS X

Data | Directory
:----|:---------
Bitcask | `./data/bitcask`
LevelDB | `./data/leveldb`
Ring | `./data/riak/ring`
Configuration | `./etc`
Cluster Metadata | `./data/riak/cluster_meta`
Search | `./data/riak/yz`
Strong consistency | `./data/ensembles`

**Note**: OS X paths are relative to the directory in which the package
was extracted.

#### SmartOS

Data | Directory
:----|:---------
Bitcask | `/var/db/riak/bitcask`
LevelDB | `/var/db/riak/leveldb`
Ring | `/var/db/riak/ring`
Configuration | `/opt/local/etc/riak`
Cluster Metadata | `/var/db/riak/cluster_meta`
Search | `/var/db/riak/yz`
Strong consistency | `/var/db/riak/ensembles`

#### Solaris

Data | Directory
:----|:---------
Bitcask | `/opt/riak/data/bitcask`
LevelDB | `/opt/riak/data/leveldb`
Ring | `/opt/riak/ring`
Configuration | `/opt/riak/etc`
Cluster Metadata | `/opt/riak/cluster_meta`
Search | `/opt/riak/yz`
Strong consistency | `/opt/riak/data/ensembles`

## Performing Backups

{{% note title="Deprecation notice" %}}
In previous versions of Riak KV, there was a [`riak-admin backup`]({{<baseurl>}}riak/kv/2.0.5/using/admin/riak-admin/#backup) command commonly used for
backups. This functionality is now deprecated. We strongly recommend using the backup procedure documented below instead.
{{% /note %}}

Backups can be accomplished through a variety of common methods. Standard utilities such `cp`, `rsync`, and `tar` can be used, as well as any backup system already in place in your environment.

A simple shell command, like those in the following examples, are sufficient for creating a backup of your Bitcask or LevelDB data, ring, and Riak KV configuration directories for a binary package-based Riak KV Linux
installation.

The following examples use `tar`:

{{% note %}}
Backups must be performed on while Riak KV is stopped to prevent data loss.
{{% /note %}}

### Bitcask

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/bitcask /var/lib/riak/ring /etc/riak
```

### LevelDB

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/leveldb /var/lib/riak/ring /etc/riak
```

### Cluster Metadata

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/cluster_meta
```

### Search / Solr Data

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/yz
```

### Strong Consistency Data

Persistently stored data used by Riak's [strong consistency][use ref strong consistency] feature
can be stored in an analogous fashion:

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/ensembles
```

## Restoring a Node

The method you use to restore a node will differ depending on a combination of factors, including node name changes and your network environment.

If you are replacing a node with a new node that has the same node name (typically a fully qualified domain name or IP address), then restoring the node is a simple process:

1. Install Riak on the new node.
2. Restore your old node's configuration files, data directory, and ring
   directory.
3. Start the node and verify proper operation with `riak ping`,
   `riak-admin status`, and other methods you use to check node health.

If the node name of a restored node (`-name` argument in `vm.args` or
`nodename` parameter in `riak.conf`) is different than the name of the
node that the restored backup was taken from, you will need to
additionally:

1. Mark the original instance down in the cluster using
   [`riak-admin down <node>`]({{<baseurl>}}riak/kv/2.0.5/using/admin/riak-admin/#down)
2. Join the restored node to the cluster using
   [`riak-admin cluster join <node>`]({{<baseurl>}}riak/kv/2.0.5/using/admin/riak-admin/#cluster-join)
3. Replace the original instance with the renamed instance with
   [`riak-admin cluster force-replace <node1> <node2>`]({{<baseurl>}}riak/kv/2.0.5/using/admin/riak-admin/#cluster-force-replace)
4. Plan the changes to the cluster with `riak-admin cluster plan`
5. Finally, commit the cluster changes with `riak-admin cluster commit`

{{% note %}}
For more information on the `riak-admin cluster` commands, refer to our documentation on [cluster administration]({{<baseurl>}}riak/kv/2.0.5/using/admin/).
{{% /note %}}

For example, if there are five nodes in the cluster with the original node names `riak1.example.com` through `riak5.example.com` and you wish to restore `riak1.example.com` as `riak6.example.com`, you would execute the following commands on `riak6.example.com`.

1. Join to any existing cluster node.

    ```bash
    riak-admin cluster join riak@riak2.example.com
    ```

2. Mark the old instance down.

    ```bash
    riak-admin down riak@riak1.example.com
    ```

3. Force-replace the original instance with the new one.

    ```bash
    riak-admin cluster force-replace \
        riak@riak1.example.com riak@riak6.example.com
    ```

4. Display and review the cluster change plan.

    ```bash
    riak-admin cluster plan
    ```

5. Commit the changes to the cluster.

    ```bash
    riak-admin cluster commit
    ```

Your [configuration files][config reference] should also be changed to match the new name in addition to running the commands (the `-name` setting in `vm.args` in the older config system, and the `nodename` setting in `riak.conf` in the newer system).

If the IP address of any node has changed, verify that the changes are reflected in your configuration files to ensure that the HTTP and Protocol Buffers interfaces are binding to the correct addresses.

A robust DNS configuration can simplify the restore process if the IP addresses of the nodes change, but the hostnames are used for the node names and the hostnames stay the same. Additionally, if the HTTP and Protocol Buffers interface settings are configured to bind to all IP interfaces (0.0.0.0), then no changes will need to be made to your configuration files.

When performing restore operations involving `riak-admin cluster force-replace`, we recommend that you start only one node at a time and verify that each node that is started has the correct name for itself
and for any other nodes whose names have changed:

1. Verify that the correct name is present your configuration file.
2. Once the node is started, run `riak attach` to connect to the node. The prompt obtained should contain the correct node name.
    - (It may be necessary to enter an Erlang atom by       typing `x.` and pressing Enter)
3. Disconnect from the attached session with **Ctrl-G + q**.
4. Finally, run `riak-admin member_status` to list all of the nodes and verify that all nodes listed have the correct names.

## Restoring a Cluster

Restoring a cluster from backups is documented [on its own page]({{<baseurl>}}riak/kv/2.0.5/using/repair-recovery/failure-recovery/#cluster-recovery-from-backups).
