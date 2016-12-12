---
title: "Hot Backups"
description: "Performing hot backups in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "Hot Backups"
    identifier: "hot_backups"
    weight: 400
    parent: "using"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
aliases:
    - /riakts/1.5.0/using/hot-backups/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/hot-backups/"
---

Riak TS enables you to perform hot backups. Hot backups are backups performed on cluster data while Riak TS is actively online.

## Perform A Hot Backup

First create the `leveldb_hot_backup` file in the `/etc/basho/` directory:

```bash
touch /etc/basho/leveldb_hot_backup
```

This file acts as a trigger. Riak TS checks for the backup file every 60 seconds, when the file is present Riak TS performs a backup of cluster data.

Once the backup process is complete Riak TS erases the `leveldb_hot_backup` file.

Next you can either leave the backup in place, or move the backup elsewhere and erase the backup directory from the production system.

Riak TS stores up to 5 backup images:

* backup
* backup.0
* backup.1
* backup.2
* backup.3
* backup.4

Each new backup request renames the existing directories to next higher number, deleting the old 'backup.4'. The new backup is placed in the 'backup' directory. Tiered storage configurations have the same directories on both the fast and slow tier.

## Limitations

This backup method does not guarantee consistency across Riak vnodes or Riak active anti-entropy (AAE) data. But standard AAE and read repair logic will create reasonable consistency.

The only backup method that guarantees full consistency across all vnodes and AAE data requires a Riak TS shutdown and backup of the static files.

Handoff and other vnode move operations do not move backup data. Also, the backup data is not deleted from the source node during handoff or other vnode move operations.

## Example crontab configuration

Create the directory for trigger file (must be root user):

```bash
mkdir /etc/basho
chown riak.riak /etc/basho
chmod 0774 /etc/basho
```

Add the following to `/etc/crontab` to initiate a backup once a week as user `riak`:

```bash
echo "40 4 * * 7 riak touch /etc/basho/leveldb_hot_backup" >>/etc/crontab
```
