---
title: "Downgrading"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Downgrading"
    identifier: "downgrading"
    weight: 103
    parent: "setup_index"
toc: true
aliases:
  - /riak/2.2.0/ops/upgrading/rolling-downgrades/
  - /riak/kv/2.2.0/ops/upgrading/rolling-downgrades/
---

[rolling upgrade]: /riak/kv/2.2.0/setup/upgrading/cluster
[config ref]: /riak/kv/2.2.0/configuring/reference
[concept aae]: /riak/kv/2.2.0/learn/concepts/active-anti-entropy/
[aae status]: /riak/kv/2.2.0/using/admin/riak-admin/#aae-status

Downgrades of Riak are tested and supported for two feature release versions, with the general procedure being similar to that of a [rolling upgrade][rolling upgrade].

Depending on the versions involved in the downgrade, there are additional steps to be performed before, during, and after the upgrade on on each node. These steps are related to changes or new features that are not present in the downgraded version.

## Overview

For every node in the cluster:

1. Stop Riak KV.
2. Back up Riak's `etc` and `data` directories.
3. Downgrade the Riak KV.
4. Remove Riak Search index and temporary data.
5. Reconfigure Solr cores.
6. Start Riak KV and disable Riak Search.
7. Monitor the reindex of the data.
8. Finalize process and restart Riak KV & Riak Search.

### Guidelines

* Riak Control should be disabled throughout the rolling downgrade process.
* [Configuration Files][config ref] must be replaced with those of the version being downgraded to.
* [Active anti-entropy][concept aae] should be disabled if downgrading to a version below 1.3.

## General Process

{{% note %}}
While the cluster contains mixed version members, if you have not set the cluster to use the legacy AAE tree format, you will see the `bad_version` error emitted to the log any time nodes with differing versions attempt to exchange AAE data (including AAE fullsync).

This is benign and similar to the `not_built` and `already_locked` errors which can be seen during normal AAE operation. These events will stop once the downgrade is complete.
{{% /note %}}

### Stop the node and remove Riak Search index and temporary data

1. Stop the Riak KV:

    ```bash
    riak stop
    ```
2. Back up the Riak KV node’s `/etc` and `/data` directories, for example:
    
    ```bash
    sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
    ```
    
3. Downgrade Riak KV, for example:

    ```bash
    sudo dpkg -i <riak_package_name>.deb
    ```

4. Remove the Riak Search index data and AAE data.

  1. Remove the cached Solr web application from the *yz_temp* folder.  For the default package paths, this would be `/var/lib/riak/yz_temp/solr-webapp`.
  
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

5. _**Optional**_: You can increase the AAE operation concurrency and increase the number of build operations while lowering the build limit's interval.  This will increase the speed at which the AAE trees are rebuilt and the search indexes are repopulated.  However, if you have a latency sensitive application, you should adjust these settings with care.

    ```riak.conf
    anti_entropy.concurrency_limit = 8
    anti_entropy.tree.build_limit.number = 4
    anti_entropy.tree.build_limit.per_timespan = 5m
    ``` 
### Start the node and disable Yokozuna

6. Start Riak KV:
    > **Note**: Search results will be inconsistent until **Step 8.1** is complete.

    ```bash
    riak start
    ```
7. Wait for Riak Search to start by running the following command:

    ```bash
    riak-admin wait-for-service yokozuna
    ```
  
8. Run `riak attach`.

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

9. Monitor the build and exchange progress using the `riak-admin aae-status` and `riak-admin search aae-status` commands.

    The **All** column shows how long it has been since a partition exchanged with all of its sibling replicas.  Consult the [`riak-admin aae-status` documentation][aae status] for more information about the AAE status output. 

    Once both riak-admin aae-status and riak-admin search aae-status show values in the **All** column, the node will have successfully rebuilt all of the indexed data.

### Finalize process and restart Yokozuna
 

10. If you raised the concurrency AAE currency settings in riak.conf during **Step 5**, stop the node and remove the increased AAE thresholds.
 
11. If you chose not to increase the AAE concurrency via configuration and want to start Yokozuna without restarting the node, run `riak attach` and enter the following snippet:

    ```erlang
    riak_core_node_watcher:service_up(yokozuna, self()).
    ```
    
12. Exit the attach session by pressing **Ctrl-G** then **q**.

13. Verify that transfers have completed:

    ```bash
    riak-admin transfers
    ```
