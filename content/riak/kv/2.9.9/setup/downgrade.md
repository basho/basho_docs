---
title: "Downgrading"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "Downgrading"
    identifier: "downgrading"
    weight: 103
    parent: "setup_index"
toc: true
aliases:
  - /riak/2.9.9/ops/upgrading/rolling-downgrades/
  - /riak/kv/2.9.9/ops/upgrading/rolling-downgrades/
---

[rolling upgrade]: {{<baseurl>}}riak/kv/2.9.9/setup/upgrading/cluster
[config ref]: {{<baseurl>}}riak/kv/2.9.9/configuring/reference
[concept aae]: {{<baseurl>}}riak/kv/2.9.9/learn/concepts/active-anti-entropy/
[aae status]: {{<baseurl>}}riak/kv/2.9.9/using/admin/riak-admin/#aae-status

Downgrades of Riak KV are tested and supported for two feature release versions, with the general procedure being similar to that of a [rolling upgrade][rolling upgrade].

Depending on the versions involved in the downgrade, there are additional steps to be performed before, during, and after the upgrade on on each node. These steps are related to changes or new features that are not present in the downgraded version.

## Overview

For every node in the cluster:

1. Stop Riak KV.
2. Back up Riak's `etc` and `data` directories.
3. Downgrade the Riak KV.
4. Remove Riak search index and temporary data.
5. Reconfigure Solr cores.
6. Start Riak KV and disable Riak search.
7. Monitor the reindex of the data.
8. Finalize process and restart Riak KV & Riak search.

### Guidelines

* Riak control should be disabled throughout the rolling downgrade process.
* [Configuration Files][config ref] must be replaced with those of the version being downgraded to.


### Components That Complicate Downgrades

| Feature | automatic | required | Notes |
|:---|:---:|:---:|:---|
|Migration to Solr 4.10.4 |✔ | ✔| Applies to all clusters using Riak Search.
| Active Anti-Entropy file format changes | ✔ |  | Can be opted out using a [capability](#aae_tree_capability)


### When Downgrading is No Longer an Option

If you enabled LZ4 compression in LevelDB and/or enabled global expiration in LevelDB when you installed KV 2.9.9, you cannot downgrade.


## General Process

{{% note %}}
While the cluster contains mixed version members, if you have not set the cluster to use the legacy AAE tree format, you will see the `bad_version` error emitted to the log any time nodes with differing versions attempt to exchange AAE data (including AAE fullsync).

This is benign and similar to the `not_built` and `already_locked` errors which can be seen during normal AAE operation. These events will stop once the downgrade is complete.
{{% /note %}}

### Stop Riak KV and remove Riak search index & temporary data

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

4\. Remove the Riak search index data and AAE data:

  1. Remove the cached Solr web application from the yz_temp folder.  For the default package paths, this would be `/var/lib/riak/yz_temp/solr-webapp`.
  
       ```bash
     rm -rf /var/lib/riak/yz_temp/solr-webapp
     ```
  2. Delete the Solr cores located in the yz directory. If you have custom solrconfig.xml files, you will need to restore the core from backup instead.
  
      For example:

      ```bash
      rm -rf /var/lib/riak/yz/example_core1
      rm -rf /var/lib/riak/yz/example_core2
      ```
  
### Prepare to Re-index Solr Cores

5\. (**Optional**) You can increase the AAE operation concurrency and increase the number of build operations while lowering the build limit's interval. This will increase the speed at which the AAE trees are rebuilt and the search indexes are repopulated.  However, if you have a latency sensitive application, you should adjust these settings with care.

```riak.conf
anti_entropy.concurrency_limit = 8
anti_entropy.tree.build_limit.number = 4
anti_entropy.tree.build_limit.per_timespan = 5m
``` 

### Start the node and disable Yokozuna

6\. Start Riak KV:
{{% note %}}
Search results will be inconsistent until **Step 8.1** is complete.
{{% /note %}}

```bash
riak start
```
    
7\. Wait for Riak search to start by running the following command:

```bash
riak-admin wait-for-service yokozuna
```
  
8\. Run `riak attach`.

  1. Run the following snippet to prevent this node from participating in distributed Riak Search queries:

    ```
    riak_core_node_watcher:service_down(yokozuna).
    ```
    
  2.  Expire the Yokozuna AAE Trees:
      
    ```
    yz_entropy_mgr:expire_trees().
    ```
    
  3. Exit the attach session by pressing **Ctrl-G** then **q**.
  
### Monitor the reindex of the data

9\. Monitor the build and exchange progress using the `riak-admin aae-status` and `riak-admin search aae-status` commands.

The **All** column shows how long it has been since a partition exchanged with all of its sibling replicas.  Consult the [`riak-admin aae-status` documentation][aae status] for more information about the AAE status output. 

Once both riak-admin aae-status and riak-admin search aae-status show values in the **All** column, the node will have successfully rebuilt all of the indexed data.

### Finalize process and restart Yokozuna
 

10\. If you raised the concurrency AAE currency settings in riak.conf during **Step 5**, stop the node and remove the increased AAE thresholds.
 
11\. If you chose not to increase the AAE concurrency via configuration and want to start Yokozuna without restarting the node, run `riak attach` and enter the following snippet:

```erlang
riak_core_node_watcher:service_up(yokozuna,whereis(yz_solr_proc)).
```
    
12\. Exit the attach session by pressing **Ctrl-G** then **q**.

13\. Verify that transfers have completed:

```bash
riak-admin transfers
```





