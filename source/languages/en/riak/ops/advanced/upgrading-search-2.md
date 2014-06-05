---
title: Upgrading Search from 1.x to 2.x
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [search, upgrading]
---

If you're using Search in a version of Riak prior to 2.0 (1.3.0 to 1.4.x), you should follow these steps to migrate your search indexes from the legacy `merge_index`, to the new Solr backed ([[yokozuna|search details]]) indexes. The legacy version of Riak Search is now deprecated, and does not support most new 2.0 features (no [[datatypes|using data types]], [[bucket types|using bucket types]], [[security|authentication and authorization]]), so we highly recommend you migrate.

The legacy `merge_index`-based search (aka legacy Search) will be removed in the future.

## Overview of an Upgrade

**Please Read This**

These migration steps are as automated as they can reasonably be, with some manual steps for safety. They are meant to be run on a live cluster, so there's no need to take all of your nodes down. Like all migration activities, you should attempt these steps at a time when your cluster is relatively light on traffic, for example, *not* the week before Christmas.

The general outline of this migration is to first create, then mirror, all legacy indexes to new Solr indexes. This is done live, and new writes will occur on both sets of indexes. This means that you can expect your index size to be at least double. You must have **plenty of disk space** to handle two sets of indexes. You'll also be doubling your index operations at this time, so you will see a **bump in network traffic**. Finally, Solr runs on a JVM process, which means it will also require **more RAM**--at least 2GB extra per node *at minimum*.

You will continue running your queries against legacy Search in this phase. Once new the indexes are up to date, you can disable legacy Search for all buckets. Finally, you can disable legacy Search on all nodes. Once you're satisfied that the new search is working as expected, you can delete the unused `merge_index` data and reclaim this disk space.

## Steps to Upgrading

1.  First, you'll perform a normal [[rolling upgrade|rolling upgrades]]. As you upgrade, enable `yokozuna` (the new Riak Search library) on each node. If you're still using `app.config` it's called `yokozuna`. If you've chosen to upgrade to the new `riak.conf` config option, it's called `search`.

    ```appconfig
     {yokozuna, [
                 {enabled, on}
                ]},
    ```
    ```riakconf
    search = on
    ```

2.  For every schema in legacy Search, you must create a comparable schema in new Search. If you want to use the default schema named [[_yz_default|search schema]], you can skip this step, but we highly recommend you create your own custom schema.

    To create a schema, you can follow the Solr [[search schema]] instructions to learn how to define your xml file. Once you've created the file, you can upload it to the cluster.

    ```curl
    curl -XPUT "http://localhost:8098/search/schema/my_schema" \
      -H'content-type:application/xml' \
      --data-binary @my_schema.xml
    ```

3.  For every index in legacy Search, you must create a comparable index in new Search, setting the appropriate schema that you created in the previous step. This index can have the same name as your legacy Search index. You can find more details about index creation under [[Using Search|Using Search#Simple-Setup]].

    ```curl
    curl -XPUT "http://localhost:8098/search/index/my_index" \
         -H'content-type:application/json' \
         -d'{"schema":"my_schema"}'
    ```

4.  For each bucket which is indexed by legacy Search, you must add the `search_index` bucket property to point to the new Search index. This new index is what we are attempting to migrate all of our index data to. You can find more details about this step under [[Using Search|Using Search#Simple-Setup]].

    ```curl
    curl -XPUT "http://localhost:8098/buckets/my_bucket/props" \
         -H'content-type: application/json' \
         -d'{"props":{"search_index":"my_index"}}'
    ```

    Once a bucket is associated with the new Search, all objects that are written or modified in Riak will be indexed by **both** legacy and new Search. However, the HTTP and client query interfaces will still continue to use the legacy Search.

5.  The new Search [[AAE|Replication#Active-Anti-Entropy-AAE-]] trees must be manually cleared so that AAE will notice the missing indexes.

    First, attach to one of the Riak nodes by calling `riak attach`. Next, paste the following code into the shell. It clears the AAE tree for each node in the cluster, which triggers index repair actions.

    ```erlang
    {ok, Ring} = riak_core_ring_manager:get_my_ring().
    Cluster = riak_core_ring:all_members(Ring).
    [ok = rpc:call(Node, yz_entropy_mgr, clear_trees, []) || Node <- Cluster].
    ```

    You can press `ctrl-C` to exit from the attached shell.

    In the background AAE will start building trees for new Search and exchange them with KV. These exchanges will notice objects are missing and index them in new Search.

    <!-- no re-index command currently exists -->

6.  Watch the [[AAE|Replication#Active-Anti-Entropy-AAE-]] status. This will show you the progress of the new search index building.

    ```curl
    riak-admin search aae-status
    ```

    Eventually all partitions will be exchanged (or buckets re-indexed). You'll know this when there are no listed statuses.

7.  Next, call the following command that will give HTTP and PB query control to the new Riak Search.

    ```curl
    riak-admin search switch-to-new-search
    ```

8.  Set the `search` bucket property to `false` for all legacy indexed buckets. This deactivates legacy Search.

    ```curl
    curl -XPUT "http://localhost:8098/buckets/my_bucket/props" \
         -H'content-type:application/json' \
         -d'{"props":{"search":false}}'
    ```

9.  You can finally disable the Riak Search process on each node by setting `riak_search` `enabled` to `false`.

    ```appconfig
     {riak_search, [
                 {enabled, false}
                ]},
    ```

Once you're convinced that legacy Search no longer needs the merge index directories, you may delete them to reclaim disk space. Don't rush into deleting them. Give yourself plenty of time to get comfortable with the new search before doing this, maybe a few weeks or more. If you run into any problems with these steps, feel free to reach out the the [[Riak community|Help and Community]] for help.
