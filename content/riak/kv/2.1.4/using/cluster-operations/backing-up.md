---
title: "Backing Up"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Backing Up"
    identifier: "cluster_operations_backing_up"
    weight: 106
    parent: "managing_cluster_operations"
toc: true
aliases:
  - /riak/2.1.4/ops/running/backups
  - /riak/kv/2.1.4/ops/running/backups
canonical_link: "https://docs.basho.com/riak/kv/latest/using/cluster-operations/backing-up"
---

[concept clusters]: /riak/kv/2.1.4/learn/concepts/clusters
[config reference]: /riak/kv/2.1.4/configuring/reference
[plan backend leveldb]: /riak/kv/2.1.4/setup/planning/backend/leveldb
[plan backend bitcask]: /riak/kv/2.1.4/setup/planning/backend/bitcask
[use ref strong consistency]: /riak/kv/2.1.4/using/reference/strong-consistency

Riak KV is a [clustered][concept clusters] system built to survive a wide range of
failure scenarios, including the loss of nodes due to network or
hardware failure. Although this is one of Riak's core strengths, it
cannot withstand _all_ failure scenarios. Like any storage system, it
remains susceptible to contingencies such as accidental or malicious
deletions. Many Riak users confront this possibility by backing up their
data, i.e. duplicating their database onto a different long-term storage
mechanism. This document covers how to perform such backups.

Riak backups can be performed using OS features or filesystems that
support snapshots, such as LVM or ZFS, or by using tools like rsync or
tar. Choosing your Riak backup strategy will largely depend on your
already-established backup methodologies, as well as the backend
configuration of your nodes. When backing up a node, it is important to
back up the data, ring, and configuration directories of your nodes.

Due to Riak's eventually consistent nature, backups can become slightly
inconsistent from node to node. Data could exist on some nodes and not
others at the exact time a backup is made. Any inconsistency will be
corrected once a backup is restored, either by Riak's [active anti-entropy](/riak/kv/2.1.4/learn/concepts/active-anti-entropy/) processes or when the object is read, via [read repair](/riak/kv/2.1.4/learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy).

Additionally, backups must be performed on a stopped node to prevent
data loss as a result of the background merging and compaction processes
of Riak's backends. Downtime of a node can be significantly reduced by
using an OS feature or filesystem that supports snapshotting.

## OS-Specific Directory Locations

The default Riak data, ring, strong consistency, and configuration
directories for each of the supported operating systems is as follows:

<div class="note">
<div class="title">Note on upgrading</div>
If you are upgrading to Riak version 2.0 or later from a pre-2.0
release, you can use either your old <code>app.config</code>
configuration file or the newer <code>riak.conf</code> if you wish.

If you have installed Riak 2.0 directly, you should use only
<code>riak.conf</code>.

More on configuring Riak can be found in the [configuration files][config reference]
doc.
</div>

#### Debian and Ubuntu

Data | Directory
:----|:---------
Bitcask | `/var/lib/riak/bitcask`
LevelDB | `/var/lib/riak/leveldb`
Ring | `/var/lib/riak/ring`
Configuration | `/etc/riak`
Strong consistency | `/var/lib/riak/ensembles`

#### Fedora and RHEL

Data | Directory
:----|:---------
Bitcask | `/var/lib/riak/bitcask`
LevelDB | `/var/lib/riak/leveldb`
Ring | `/var/lib/riak/ring`
Configuration | `/etc/riak`
Strong consistency | `/var/lib/riak/ensembles`

#### FreeBSD

Data | Directory
:----|:---------
Bitcask | `/var/db/riak/bitcask`
LevelDB | `/var/db/riak/leveldb`
Ring | `/var/db/riak/ring`
Configuration | `/usr/local/etc/riak`
Strong consistency | `/var/db/riak/ensembles`

#### OS X

Data | Directory
:----|:---------
Bitcask | `./data/bitcask`
LevelDB | `./data/leveldb`
Ring | `./data/riak/ring`
Configuration | `./etc`
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
Strong consistency | `/var/db/riak/ensembles`

#### Solaris

Data | Directory
:----|:---------
Bitcask | `/opt/riak/data/bitcask`
LevelDB | `/opt/riak/data/leveldb`
Ring | `/opt/riak/ring`
Configuration | `/opt/riak/etc`
Strong consistency | `/opt/riak/data/ensembles`

> **Note on strong consistency directories**
>
> The listings above show directories for data related to Riak's
[strong consistency][use ref strong consistency] feature. This feature is purely optional, so `/ensembles` directories will not exist in your installation if this feature is not being used. For more information, see [Using Strong Consistency](/riak/kv/2.1.4/developing/app-guide/strong-consistency) and [Managing Strong Consistency](/riak/kv/2.1.4/using/cluster-operations/strong-consistency).

## Performing Backups

> **Deprecation notice**
>
> In previous versions of Riak, there was a [`riak-admin backup`](/riak/kv/2.1.4/using/admin/riak-admin/#backup) command commonly used for
backups. This functionality is now deprecated. We strongly recommend
using the backup procedure documented below instead.

Backups of both Bitcask and LevelDB can be accomplished through a
variety of common methods. Standard utilities such `cp`, `rsync`, and
`tar` can be used as well as any backup system or method already in
place in your environment. Please remember that the node must *not* be
running while performing the backup.

A simple shell command such as the following example is sufficient for
creating a backup of your Bitcask or LevelDB data, ring, and Riak
configuration directories for a binary package-based Riak Linux
installation. The following examples use `tar`:

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

### Strong Consistency Data

Persistently stored data used by Riak's [strong consistency][use ref strong consistency] feature
can be stored in an analogous fashion:

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/ensembles
```

The basic process for getting a backup of Riak from a node is as
follows:

1. Stop the node with `riak stop`
2. Back up the appropriate data, ring, configuration, and strong consistency
   (if applicable) directories as relevant to your operating system
3. Start the node

Consult the [Bitcask][plan backend bitcask] and [LevelDB][plan backend leveldb] documentation to learn more
about these backends.

## Restoring a Node

The method you use to restore a node will differ depending on a
combination of factors, including node name changes and your network
environment.

If you are replacing a node with a new node that has the same node name
(typically a fully qualified domain name or IP address), then restoring
the node is a simple process:

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
   [`riak-admin down <node>`](/riak/kv/2.1.4/using/admin/riak-admin/#down)
2. Join the restored node to the cluster using
   [`riak-admin cluster join <node>`](/riak/kv/2.1.4/using/admin/riak-admin/#cluster-join)
3. Replace the original instance with the renamed instance with
   [`riak-admin cluster force-replace <node1> <node2>`](/riak/kv/2.1.4/using/admin/riak-admin/#cluster-force-replace)
4. Plan the changes to the cluster with `riak-admin cluster plan`
5. Finally, commit the cluster changes with `riak-admin cluster commit`

> **Further information**
>
> For more information on the `riak-admin cluster` commands,
refer to our documentation on [cluster administration](/riak/kv/2.1.4/using/admin/).

For example, if there are five nodes in the cluster with the original
node names `riak1.example.com` through `riak5.example.com` and you wish
to restore `riak1.example.com` as `riak6.example.com`, you would execute
the following commands on `riak6.example.com`.

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

Your [configuration files][config reference] should also be changed to match the new
name in addition to running the commands (the `-name` setting in
`vm.args` in the older config system, and the `nodename` setting in
`riak.conf` in the newer system). If the IP address of any node has
changed, verify that the changes are reflected in your configuration
files to ensure that the HTTP and Protocol Buffers interfaces are
binding to the correct addresses.

A robust DNS configuration can simplify the restore process if the IP
addresses of the nodes change, but the hostnames are used for the node
names and the hostnames stay the same. Additionally, if the HTTP and
Protocol Buffers interface settings are configured to bind to all IP
interfaces (0.0.0.0), then no changes will need to be made to your
configuration files.

When performing restore operations involving `riak-admin cluster
force-replace`, we recommend that you start only one node at a time and
verify that each node that is started has the correct name for itself
and for any other nodes whose names have changed.

To do this, first verify that the correct name is present your
configuration file. Then, once the node is started, run `riak attach` to
connect to the node. It may be necessary to enter an Erlang atom and
press enter to obtain a prompt, by typing `x.` and pressing enter. The
prompt obtained should contain the correct node name. Disconnect from
the attached session with **Ctrl-G q**. Finally, run `riak-admin
member_status` to list all of the nodes and verify that all nodes listed
have the correct names.

## Restoring a Cluster

Restoring a cluster from backups is documented [on its own page](/riak/kv/2.1.4/using/repair-recovery/failure-recovery/#cluster-recovery-from-backups).
