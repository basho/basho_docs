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

The main goal of a live migration is to stand up indexes in new Search that parallel the existing ones in legacy.  New writes add entries to both indexes while AAE adds entires in the new indexes for existing data.

Parallel indexes means more disk usage.  How much more will depend on the schema but tests have shown Solr to generally use less disk.  A prudent plan will expect new Search to use as much disk as legacy. Also expect more CPU usage as analysis will temporarily be performed by both systems.  Finally, Solr runs on a JVM process, requiring its own RAM.  A good start is 2GB but more is required for heavier workloads.  On the contrary, do not make too large a heap as it could cause lengthy garbage collection pauses.

As the new Search indexes catch up with the old, incoming queries will still be serviced by legacy Search.  Once you have determined the new indexes are consistent with KV you can perform a live switch to the new system and turn off legacy Search.  Finally, you can remove the old merge index directories to reclaim disk space.

<div class="note">
<div class="title">Downgrading and Merge Index</div>
It may be tempting to keep the merge index files in case of a downgrade.  Don't do that if writes are being made to these buckets during upgrade.  Once `search: false` is set on a bucket all new KV data written will have missing indexes in merge index and overwritten data will have inconsistent indexes.  At this point a downgrade requires a full reindex of the data as legacy Search has no mechanism to cope with inconsistency (such as AAE in new Search).
</div>


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

9.  Disable the Riak Search process on each node by setting `riak_search` `enabled` to `false`.

    ```appconfig
     {riak_search, [
                 {enabled, false}
                ]},
    ```

10. Finally, delete the merge index directories to relcaim disk space.

For any questions reach out the the [[Riak community|Help and Community]].  Preferably, ask your questions up front rather than during the middle of a migration.
