---
title: Backing up Riak
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
moved: {
    '1.4.0-': '/cookbooks/Backups'
}
---

Riak is a [[clustered|Clusters]] system built to survive a wide range of
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
corrected once a backup is restored, either by Riak's [[active
anti-entropy]] processes or when the object is read, via [[read
repair|Active Anti-Entropy#Read-Repair-vs-Active-Anti-Entropy]].

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
release, you can use either your old <tt>app.config</tt> configuration
file or the newer <tt>riak.conf</tt> if you wish.

If you have installed Riak 2.0 directly, you should use only
<code>riak.conf</code>.

More on configuring Riak can be found in the [[configuration files]]
doc.
</div>

#### Debian and Ubuntu

* Bitcask data: `/var/lib/riak/bitcask`
* LevelDB data: `/var/lib/riak/leveldb`
* Ring data: `/var/lib/riak/ring`
* Configuration: `/etc/riak`
* Strong consistency data: `/var/lib/riak/ensembles`

#### Fedora and RHEL

* Bitcask data: `/var/lib/riak/bitcask`
* LevelDB data: `/var/lib/riak/leveldb`
* Ring data: `/var/lib/riak/ring`
* Configuration: `/etc/riak`
* Strong consistency data: `/var/lib/riak/ensembles`

#### FreeBSD

* Bitcask data: `/var/db/riak/bitcask`
* LevelDB data: `/var/db/riak/leveldb`
* Ring data: `/var/db/riak/ring`
* Configuration: `/usr/local/etc/riak`
* Strong consistency data: `/var/db/riak/ensembles`

#### OS X

* Bitcask data: `./data/bitcask`
* LevelDB data: `./data/leveldb`
* Ring data: `./data/riak/ring`
* Configuration: `./etc`
* Strong consistency data: `./data/ensembles`

**Note**: OS X paths are relative to the directory in which the package
was extracted.

#### SmartOS

* Bitcask data: `/var/db/riak/bitcask`
* LevelDB data: `/var/db/riak/leveldb`
* Ring data: `/var/db/riak/ring`
* Configuration: `/opt/local/etc/riak`
* Strong consistency data: `/var/db/riak/ensembles`

#### Solaris

* Bitcask data: `/opt/riak/data/bitcask`
* LevelDB data: `/opt/riak/data/leveldb`
* Ring data: `/opt/riak/ring`
* Configuration: `/opt/riak/etc`
* Strong consistency data: `/opt/riak/data/ensembles`

<div class="note">
<div class="title">Note on strong consistency directories</div>
The listings above show directories for data related to Riak's
[[strong consistency]] feature. This feature is purely optional, so
<code>/ensembles</code> directories will not exist in your installation
if this feature is not being used. For more information, see [[Using
Strong Consistency]] and [[Managing Strong Consistency]].
</div>

## Performing Backups

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

Persistently stored data used by Riak's [[strong consistency]] feature
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

Consult the [[Bitcask]] and [[LevelDB]] documentation to learn more
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
   `[[riak-admin down <node>|riak-admin Command Line#down]]`
2. Join the restored node to the cluster using
   `[[riak-admin cluster join <node>|riak-admin Command Line#cluster-join]]`
3. Replace the original instance with the renamed instance with
   `[[riak-admin cluster force-replace <node1> <node2>|riak-admin
   Command Line#cluster-force-replace]]`
4. Plan the changes to the cluster with `riak-admin cluster plan`
5. Finally, commit the cluster changes with `riak-admin cluster commit`

<div class="note">
<div class="title">Further information</div>
For more information on the <code>riak-admin cluster</code> commands,
refer to our documentation on the <code>[[riak-admin|riak-admin Command
Line#cluster]]</code> tool.
</div>

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

Your [[configuration files]] should also be changed to match the new
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
the attached session with `^d` (**Ctrl-d**). Finally, run `riak-admin
member_status` to list all of the nodes and verify that all nodes listed
have the correct names.

## Restoring a Cluster

Restoring a cluster from backups is documented [[on its own page|Failure
and Recovery#Cluster-Recovery-From-Backups]].
