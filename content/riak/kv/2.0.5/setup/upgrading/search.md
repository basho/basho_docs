---
title: "Upgrading Search from 1.x to 2.x"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Upgrading Search 1.x to 2.x"
    identifier: "upgrading_search"
    weight: 104
    parent: "upgrading"
toc: true
version_history:
  in: "2.0.0-2.99.999"
---

If you're using Search in a version of Riak prior to 2.0 (1.3.0 to
1.4.x), you should follow these steps to migrate your search indexes
from the legacy `merge_index` to the new Solr-backed ([Yokozuna](../../../using/reference/search) indexes. The legacy version of Riak Search is now deprecated
and does not support most new 2.0 features, i.e. no [Riak Data Types](../../../developing/data-types), [bucket types](../../../using/reference/bucket-types), [strong consistency](../../../using/reference/strong-consistency), or [security](../../../using/security/)), so we highly recommend that you migrate.

And please note that the legacy `merge_index`-based search (aka legacy
Search) will be removed in a future release of Riak.

## Overview of an Upgrade

The migration steps explained here are as automated as they can
reasonably be, but they do include some manual steps for safety. They
are meant to be run on a live cluster, so there's no need to take all of
your nodes down. Like all migration activities, you should undertake
these steps at a time when your cluster is relatively light on traffic,
i.e. _not_ the week before Christmas.

The main goal of a live migration is to stand up indexes in the new Riak
Search that parallel the existing ones in legacy. New writes add entries
to both indexes while AAE adds entries in the new indexes for existing
data.

Parallel indexes mean more disk usage. How much more will depend on the
schema but tests have shown Solr to generally use less disk space. A
prudent plan will expect new Search to use as much disk as legacy. You
can also expect more CPU usage as analysis will temporarily be performed
by both systems. Finally, Solr runs on a JVM process requiring its own
RAM. A good start is 2 GB but more will be required for heavier
workloads. On the contrary, do not make too large a heap as it could
cause lengthy garbage collection pauses.

As the new search indexes catch up with the old, incoming queries will
still be serviced by legacy Search. Once you have determined that the
new indexes are consistent with KV, you can perform a live switch to the
new system and turn off legacy Search. Finally, you can remove the old
merge index directories to reclaim disk space.

> **Downgrading and Merge Index**
>
> It may be tempting to keep the merge index files in case of a downgrade.
We don't recommend doing that if writes are being made to these buckets
during upgrade.  Once `search: false` is set on a bucket, all new KV
data written will have missing indexes in the merge index and
overwritten data will have inconsistent indexes. At this point, a
downgrade requires a full re-index of the data as legacy Search has no
mechanism to cope with inconsistency (such as [active anti-entropy](../../../learn/glossary/#active-anti-entropy-aae) in the new Search).

> **Active Anti-Entropy (AAE) Required**
>
>Migration requires that Riak's AAE subsystem be enabled. It's
responsible for finding all the missing index entries for existing data
and adding them. Technically speaking, the migration can be performed
without AAE, but it will require a key listing or [MapReduce](../../../developing/usage/mapreduce) job that re-indexes every object. This method will use more CPU, network, and especially disk space from merge index as its GC
algorithm is bad at getting rid of large index files.

## Steps to Upgrading

1.  First, you'll perform a normal [rolling upgrade](../cluster).
    As you upgrade, enable `yokozuna` (the new Riak Search library) on
    each node. If you're still using `app.config` it's called `yokozuna`.
    If you've chosen to upgrade to the new `riak.conf` config option, it's
    called `search`.

    ```riakconf
    search = on
    ```
    ```appconfig
    {yokozuna, [
                %% Other configs
                {enabled, true},
                %% Other configs
               ]}
    ```

    <div class="note">
    <div class="title">Upgrade First</div>
    Don't proceed until all nodes have been upgraded to the newest
    version. This way all nodes have new Search capabilities before
    running the next steps which require them.
    </div>

2. For every schema in legacy Search, you must create a comparable
schema in new Search. If you want to use the default schema named
[_yz_default](../../../developing/usage/search-schemas), you can skip this step, but we highly recommend you create your own custom schema.

    To create a schema, you can follow the Solr [search schema](../../../developing/usage/search-schemas)
    instructions to learn how to define your xml file. Once you've created
    the file, you can upload it to the cluster.

    ```curl
    curl -XPUT http://localhost:8098/search/schema/my_schema \
      -H 'Content-Type: application/xml' \
      --data-binary @my_schema.xml
    ```

3. For every index in legacy Search, you must create a comparable index
in new Search, setting the appropriate schema that you created in the
previous step. This index can have the same name as your legacy Search
index. You can find more details about index creation under [Using Search](../../../developing/usage/search/#setup).

    ```curl
    curl -XPUT http://localhost:8098/search/index/my_index \
      -H 'Content-Type: application/json' \
      -d '{"schema":"my_schema"}'
    ```

4. For each bucket which is indexed by legacy Search, you must add the
`search_index` bucket property to point to the new Search index. This
new index is what we are attempting to migrate all of our index data to.
You can find more details about this step under [Using Search](../../../developing/usage/search/#setup).

    ```curl
    curl -XPUT http://localhost:8098/buckets/my_bucket/props \
      -H 'Content-Type: application/json' \
      -d '{"props":{"search_index":"my_index"}}'
    ```

    Once a bucket is associated with the new Search, all objects that are
    written or modified in Riak will be indexed by **both** legacy and new
    Search. However, the HTTP and client query interfaces will still
    continue to use the legacy Search.

5. The new Search [AAE](../../../learn/glossary/#active-anti-entropy-aae) hash
trees must be manually cleared so that AAE will notice the missing
indexes.

    Attach to one of the Riak nodes by calling `riak attach-direct`. Paste
    the following code into the shell. It clears the Search hash trees for
    each node in the cluster.

    ```erlang
    riak_core_util:rpc_every_member_ann(yz_entropy_mgr, clear_trees, [], infinity).
    ```

    Press `Ctrl-D` to exit from the attached shell.

    In the background AAE will rebuild the hash trees and exchange them
    with KV. These exchanges will notice objects are missing and index
    them in new Search.

    <!-- no re-index command currently exists -->

6. Monitor the AAE status of every node until a full round of exchanges
have occurred on every node.

    ```bash
    riak-admin search aae-status
    ```

    First, you must wait until all trees are rebuilt. This may take a
    while as each node is configured, by default, to build a maximum of
    one tree per hour. You can determine when a tree is build by looking
    at the `Entropy Trees` section. When a tree is not built it will show
    `--` under the `Built (ago)` column. Otherwise, it will list how long
    ago the tree was built in a human friendly format. Here is an example
    of trees that are not built:

    ```
    ================================ Entropy Trees ================================
    Index                                              Built (ago)
    -------------------------------------------------------------------------------
    ...
    296867520082839655260123481645494988367611297792   --
    319703483166135013357056057156686910549735243776   --
    ...
    ```

    Here is an example of built trees:

    ```
    ================================ Entropy Trees ================================
    Index                                              Built (ago)
      -------------------------------------------------------------------------------
    ...
    296867520082839655260123481645494988367611297792   12.3 hr
    319703483166135013357056057156686910549735243776   5.3 hr
    ...
    ```

    After all the trees are built you then have to wait for a full
    exchange round to occur for every partition on every node.  That is,
    the full exchange round must be **NEWER** than the time the tree was
    built.  That way you know the exchange was based on the latest tree.
    The exchange information is found under the `Exchanges` section.
    Under that section there are two columns: `Last (ago)` and `All
    (ago)`.  In this was you want to wait until the `All (ago)` section is
    newer than the value of `Built (ago)` in the `Entropy Trees` section.
    For example, given the entropy tree output above this output would
    indicate both partitions have had a full exchange round since the
    latest tree was built:

    ```
    ================================== Exchanges ==================================
    Index                                              Last (ago)    All (ago)
    -------------------------------------------------------------------------------
    ...
    296867520082839655260123481645494988367611297792   12.1 hr       12.1 hr
    319703483166135013357056057156686910549735243776   5.1 hr        5.2 hr
    ...
    ```

    Notice that `12.1 hr` is newer than `12.3 hr` and `5.2 hr` newer than
    `5.3 hr`. Once the exchange is newer for every partition on every
    node you know that AAE has brought all new indexes up to date.

7. Next, call the following command that will give HTTP and PB query
control to the new Riak Search.

    ```curl
    riak-admin search switch-to-new-search
    ```

    <div class="note">
    <div class="title">Check Results Before Switching (Optional)</div>
    Up until this point all incoming queries are serviced by the legacy
    Search system.  After the `switch-to-new-search` is run all queries
    will be handled by new Search.  If you first want to verify the
    results of new Search before switching then you can use its dedicated
    HTTP resource at `/search/query/<index>?q=...`.
    </div>

8. Set the `search` bucket property to `false` for all legacy indexed
buckets. This deactivates legacy Search.

    ```curl
    curl -XPUT "http://localhost:8098/buckets/my_bucket/props" \
      -H 'Content-Type: application/json' \
      -d '{"props":{"search": false}}'
    ```

9. Disable the Riak Search process on each node by setting `riak_search`
`enabled` to `false`.

    ```appconfig
    {riak_search, [
                   %% Other configs
                   {enabled, false},
                   %% Other configs
                  ]},
    ```

10. Perform a rolling restart. This is needed both to stop legacy
Search as well as properly remove the legacy Search commit hooks. A bug
in the 1.4.x series allowed bucket properties to leak into what Riak
developers call the "raw ring". In the case of legacy Search it causes
the commit hooks to persist even when legacy Search is disable and the
search property is set to false.

    New Search has code to expunge the legacy hooks from the raw ring but
    it only occurs during start-up and requires that legacy Search be
    disabled in the configuration.  Thus, the easiest way to fix things is
    to disable legacy Search (in step 9) and then perform a rolling
    restart of the cluster.

11. Finally, delete the merge index directories to reclaim disk space.

For any questions reach out to the [Riak community]({{<baseurl>}}community). Preferably, ask your questions up front rather than during the middle of a migration.
