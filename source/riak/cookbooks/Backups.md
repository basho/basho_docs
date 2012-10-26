---
title: Backing up Riak
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

Choosing your Riak backup strategy will largely depend on the backend
configuration of your nodes. In many cases, Riak will conform to your
already established backup methodologies. When backing up a node, it is
important to backup both the `ring` and `data` directories that pertain
to your configured backend.

In addition to data and ring directories, it is useful to backup your
configuration directory at the same time to ease recovering from a
node failure.

The default Riak data, ring, and configuration directories for each of
the supported operating systems is as follows:

**Debian and Ubuntu**

* Bitcask data: `/var/lib/riak/bitcask`
* LevelDB data: `/var/lib/riak/leveldb`
* Ring data: `/var/lib/riak/ring`
* Configuration: `/etc/riak`

**Fedora and RHEL**

* Bitcask data: `/var/lib/riak/bitcask`
* LevelDB data: `/var/lib/riak/leveldb`
* Ring data: `/var/lib/riak/ring`
* Configuration: `/etc/riak`

**Freebsd**

* Bitcask data: `/var/db/riak/bitcask`
* LevelDB data: `/var/db/riak/leveldb`
* Ring data: `/var/db/riak/ring`
* Configuration: `/usr/local/etc/riak`

**OS X**

NOTE: OS X paths are relative to the directory in which the
package was extracted.

* Bitcask data: `./data/bitcask`
* LevelDB data: `./data/leveldb`
* Ring data: `./data/riak/ring`
* Configuration: `./etc`

**SmartOS**

* Bitcask data: `/var/db/riak/bitcask`
* LevelDB data: `/var/db/riak/leveldb`
* Ring data: `/var/db/riak/ring`
* Configuration: `/opt/local/etc/riak`

**Solaris**

* Bitcask data: `/opt/riak/data/bitcask`
* LevelDB data: `/opt/riak/data/leveldb`
* Ring data: `/opt/riak/ring`
* Configuration: `/opt/riak/etc`

<div class="info">
Due to Riak's eventually consistent nature, backups can become slightly
inconsistent from node to node. Data could exist on some nodes and not
others at the exact time a backup is made. Any inconsistency will be
corrected at read time with Riak's [[read-repair|Replication#Read-Repair]] system, however.
</div>

## Bitcask Backups
Due to its log-structured design, backups of Bitcask can be accomplished
through a variety of common methods. Standard utilities such `cp`, `rsync`,
and `tar` can be used as well as any backup system or methodology already
in place in your environment.

A simple cron job such as the following example is sufficient for creating
a backup of your Bitcask data, ring, and Riak configuration directories
for a binary package based Riak Linux installation:

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/bitcask /var/lib/riak/ring /etc/riak
```

Consult the [[Bitcask]] documentation to learn more about this backend.

## LevelDB Backups
Currently, LevelDB data and log backups require that the node
*not be running* when the backup is performed. This can present the challenge
of coordinating node shutdown and startup with the backup process, but
otherwise, backup of a LevelDB based node is similar to that of other
Riak backends.

A simple cron job such as the following example is sufficient for creating
a backup of your LevelDB data, ring, and Riak configuration directories
for a binary package based Riak Linux installation

The basic process for getting a backup of LevelDB from a node is as follows:

1. Stop the node
2. Back up the appropriate data, ring, and configuration directories as
   relevant to your operating system.
3. Start the node

<div class="info">One handy means to avoid excessive downtime is to store Riak data on a file system with snapshot capabilities, such as ZFS. The process would be to stop the node, take a snapshot of the data directory and start the node. You can then dump the snapshot and delete it later.</div>

Consult the [[LevelDB]] documentation to learn more about this backend.

## Restoring a Node
The method you use to restore a node will differ depending on a combination
of factors, including node name changes and your network environment.

If you are replacing a node with a new node that has the same node name (typically a fully qualified domain name or IP address), then restoring
the node is is a simple process.

1. Install Riak on the new node
2. Restore your old node's configuration files, data directory, and ring
   directory.
3. Start the node and verify proper operation with `riak ping`,
   `riak-admin status`, and other methods you use to check node health.

If your *vm.args* -name argument is configured to use an IP address, such as
*-name riak@10.1.1.10*, and you need to replace the host with a host that
will have a different IP address, you need to restore your data directories
and configuration files, and **prior to starting each node**,
you need to execute the *riak-admin re-ip* command on each node for each
node. For example if you have a 5 node cluster with the original node names:
*riak1.example.com* through *riak5.example.com* and you needed to change the
names to *riak101.example.com* through *riak105.example.com* then you'd run
the following `riak-admin reip` commands **on each stopped node**:

```bash
riak-admin reip riak@riak1.example.com riak@riak101.example.com
riak-admin reip riak@riak2.example.com riak@riak102.example.com
riak-admin reip riak@riak3.example.com riak@riak103.example.com
riak-admin reip riak@riak4.example.com riak@riak104.example.com
riak-admin reip riak@riak5.example.com riak@riak105.example.com
```

You can initially verify that the command was successful by examining the
node name in `vm.args` on each node.

If you have a robust DNS configuration and use hostnames for your Riak nodes
restoring requires fewer steps.

After your update your DNS to take account for the change in IP check your *app.config* the HTTP and PB interface bindings.

If you bind to all IP interfaces (0.0.0.0) no changes will need to be made.

If you are not binding to all IP interfaces, you'll need to update the node's
`app.config` to use the new addresses.

Once any needed configuration changes are made you can restore the data and
ring directories and start the node.
