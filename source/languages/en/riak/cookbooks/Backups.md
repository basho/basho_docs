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

**FreeBSD**

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

If you are replacing a node with a new node that has the same node name
(typically a fully qualified domain name or IP address), then restoring the
node is is a simple process.

1. Install Riak on the new node.
2. Restore your old node's configuration files, data directory, and ring
   directory.
3. Start the node and verify proper operation with `riak ping`,
   `riak-admin status`, and other methods you use to check node health.

{{#1.2.0-}}
If any node names have been changed (that is, the *-name* argument in the
`vm.args` configuration file for any node is different than the backup being
restored to that node), then all nodes will need to be updated using the
`riak-admin reip` command **prior to starting each node**.

Note that even if only one node name has changed, all nodes must be updated
to reflect the change. On each node, restore the data directories and
configuration files from the backup, and **before starting the node**,
execute the `riak-admin reip` command for each node whose name has changed.

For example, if there are 5 nodes in the cluster with the original node
names: *riak1.example.com* through *riak5.example.com* and their names are
changing to *riak101.example.com* through *riak105.example.com* then run the
following `riak-admin reip` commands **on each stopped node**:

```bash
# run these commands on every node in the cluster while the node is stopped
riak-admin reip riak@riak1.example.com riak@riak101.example.com
riak-admin reip riak@riak2.example.com riak@riak102.example.com
riak-admin reip riak@riak3.example.com riak@riak103.example.com
riak-admin reip riak@riak4.example.com riak@riak104.example.com
riak-admin reip riak@riak5.example.com riak@riak105.example.com
```
{{/1.2.0-}}

{{#1.2.0+}}
If any node names have been changed (that is, the *-name* argument in the
`vm.args` configuration file for any node is different than the backup being
restored to that node), then you will need to additionally:

1. Mark the original instance down in the cluster using 
`[[riak-admin down <node>|Command-Line-Tools---riak-admin#down]]`
2. Join the restored node to the cluster using 
`[[riak-admin cluster join <node>|Command-Line-Tools---riak-admin#cluster-join]]`
3. Replace the original instance with the renamed instance with 
`[[riak-admin cluster force-replace <node1> <node2>|Command-Line-Tools---riak-admin#cluster-force-replace]]`
4. Plan the changes to the cluster with 
`riak-admin cluster plan`
5. Finally, commit the cluster changes with 
`riak-admin cluster commit`

<div class="info">For more information about the `riak-admin cluster` commands,
refer to the [[cluster section of "Command Line Tools - riak-admin"|Command Line Tools - riak-admin#cluster]].</div>

For example, if there are five nodes in the cluster with the original node
names *riak1.example.com* through *riak5.example.com* and you wish to restore
*riak1.example.com* as *riak6.example.com*, you would execute the following
commands on *riak6.example.com*:


```bash
# Join to any existing, cluster node
riak-admin cluster join riak@riak2.example.com
# Mark the old instance down
riak-admin down riak@riak1.example.com
# Force-replace the original instance with the new one
riak-admin cluster force-replace riak@riak1.example.com riak@riak6.example.com
# Display and review the cluster change plan
riak-admin cluster plan
# Commit the changes to the cluster.
riak-admin cluster commit
```
{{/1.2.0+}}

The *-name* setting in the `vm.args` configuration file should also be changed
to match the new name in addition to running the commands. If the IP address of
any node has changed, verify that the changes are reflected in the *app.config*
file to ensure that the HTTP and PB interfaces are binding to the correct
addresses.

A robust DNS configuration can simplify the restore process if the IP addresses
of the nodes change, but the hostnames are used for the node names and the
hostnames stay the same. Additionally, if the HTTP and PB interface settings are
configured to bind to all IP interfaces (0.0.0.0), then no changes will need to
be made to the *app.config* file.

It is recommended when performing restore operations involving 
{{#1.2.0-}}`riak-admin reip`{{/1.2.0-}}
{{#1.2.0+}}`riak-admin cluster force-replace`{{/1.2.0+}} 
to start only one node at a time, and verify that each node that is
started has the correct name for itself and any other nodes whose names have
changed.

First, verify that the correct name is present in the vm.args configuration
file. Then, once the node is started, run `riak attach` to connect to the node.
It may be necessary to enter an Erlang atom and press enter to obtain a prompt,
by typing `x.` and pressing enter. The prompt obtained should contain the
correct node name. Disconnect from the attached session with `^d` (control-d).
Finally, run `riak-admin member_status` to list all of the nodes and verify
that all nodes listed have the correct names.
