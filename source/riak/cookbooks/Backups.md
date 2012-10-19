---
title: Backing up Riak
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

Choosing how to backup your Riak nodes will depend on the backend configuration of your nodes.
In many cases Riak will be able to conform to your already established backup methodologies. When backing up a node it is important to backup both the ring and data directories that pertain
to your configured backend.  In addition it is useful to backup your configuration at the same time to make
recovering from a node failure easier.

<div class="info">
Due to the eventually consistent nature of Riak backups might be slightly inconsistent node to node. Data may exist on some nodes and not others at the exact time a backup is made. Any inconsistency will be corrected at read time with Riak's [[read-repair|Replication#Read-Repair]] system.
</div>

## Bitcask Backups
Due to its log-structured design backups of Bitcask can be accomplished through several different means.
Standard utilities such cp, rsync, and tar can be used as well as any backup system or methodology already
in place in your environment.

On a standard Linux install of Riak using packages a simple cron job running the following could be used to
create a backup of your Bitcask and ring directories as well as your Riak configuration:

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/bitcask /var/lib/riak/ring /etc/riak
```

You can read more about [[Bitcask here|Bitcask]].

## LevelDB Backups
Similar to Bitcask, LevelDB uses a log-structured file format and can be backed up using a variety of methods to facilitate
backups. A copy of the leveldb data directory will be all that is needed to restore a failed node.

On a standard Linux install of Riak using packages a simple cron job running the following could be used to
create a backup of your leveldb and ring directories as well as your Riak configuration:

```bash
tar -czf /mnt/riak_backups/riak_data_`date +%Y%m%d_%H%M`.tar.gz \
  /var/lib/riak/leveldb /var/lib/riak/ring /etc/riak
```

You can read more about [[LevelDB here|LevelDB]].

## Restoring a Node

The proper way to restore a node will differ depending on the combination of what you have named your nodes
as well as your environment.

If you are replacing a node with a host that has the same hostname and IP address a restore is as simple as
installing Riak, restoring your configuration files, data directory, and ring directory. Once this is complete you can start the node.

If your *vm.args* -name argument is configured to use an IP address, such as
*-name riak@10.1.1.10*, and you need to replace the host with a host that will have a different IP address
you will need to restore your data directories and config files, make the appropriate *app.config* and
*vm.args* changes, and finally execute the *riak-admin re-ip* before starting the node.

If you have a robust DNS configuration and use hostnames for your Riak nodes restoring requires fewer steps.
After your update your DNS to take account for the change in IP check your *app.config* the HTTP and PB interface bindings.
If you bind to all IP interfaces (0.0.0.0) no changes will need to be made.  If not binding to all IP interfaces, you'll need to update them to use the new interface. Once any needed configuration changes are made you can restore the data and ring directories and start the node.
