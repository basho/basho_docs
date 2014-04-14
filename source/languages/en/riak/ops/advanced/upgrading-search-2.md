---
title: Upgrading Riak Search from 1.x to 2.x
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [search, upgrading]
---

If you're using Search in a version of Riak prior to 2.0 (1.3.0 to 1.4.x), you should follow these steps to migrate your search indexes from the legacy `merge_index`, to the new Solr backed ([[yokozuna|using riak search]]) indexes. The legacy version of Riak Search is now deprecated, and does not support most new 2.0 features (no [[datatypes]], [[bucket types|using bucket types]], [[strong consistency]], [[security]]), so we highly recommend you migrate. The legacy `merge_index` search will be removed in the future.

## Overview of an Upgrade

**Please Read This**

These migration steps are as automated as they can reasonably be, with some manual steps for safety. They are means to be run on a live cluster, so there's no need to take all of your nodes down. Like all migration activities, you should attempt these steps at a time when your cluster is relatively light on traffic, for example, not the week before Christmas.

The general outline of this migration is to first create then mirror all legacy indexes to new Solr indexes. This is done live, and new writes will occur on both sets of indexes. This means that you can expect your index size to be at least double. You must have **plenty of disk space** to handle two sets of indexes. You'll also be doubling your index operations at this time, so you will see a **bump in network traffic**. Finally, Solr runs on a JVM process, which means it will also require **more RAM**--at least 2GB extra.

You will continue running your queries against Riak Search in this phase. Once new the indexes are up to date, you can disable legacy search for all buckets. Finally, you can disable legacy Riak Search on all nodes. Once you're satisfied that the new search is working as expected, you can delete the unused `merge_index` data and reclaim this disk space.

## Steps to Upgrading

1. First, you'll perform a normal [[rolling upgrade]]. As you upgrade, enable Yokozuna on each node using either `app.config` or the new `riak.conf` config option.

```appconfig
 {yokozuna, [
             {enabled, on}
            ]},
```
```riakconf
search = on
```

2. For every index in Riak Search the user must create a comparable index in Yokozuna.

```curl
# Run for each Riak node

export RIAK_HOST="http://localhost:8098"

curl -XPUT "$RIAK_HOST/search/index/famous" \
     -H'content-type:application/json' \
     -d'{"schema":"_yz_default"}'
```
```erlang
{ok, Ring} = riak_core_ring_manager:get_my_ring().
{ok, PB} = riakc_pb_socket:start_link(IP, Port),
riakc_pb_socket:create_search_index(Pid, <<"famous">>, <<"_yz_default">>, []),
```


3. For every bucket which is indexed by Riak Search the user must add the `search_index' bucket property to point to the Yokozuna index which is going to eventually be migrated to.

4. As objects are written or modified they will be indexed by both Riak Search and Yokozuna.  But the HTTP and PB query interfaces will continue to use Riak Search.

5a. The YZ AAE trees must be manually cleared so that AAE will notice the missing indexes.

5b. In the background AAE will start building trees for Yokozuna and exchange them with KV.  These exchanges will notice objects are missing and index them in Yokozuna.

5c. The user wants Yokozuna to index the missing objects as fast as possible.  A command may be used (repair? bucket map-reduce? custom fold function?) to immediately re-index data.

6. Eventually all partitions will be exchanged (or buckets re-indexed) and the user will be satisfied that queries can now migrate to Yokozuna.  This will be accomplished via the AAE status.

7. The user will call some command that hands HTTP and PB query control to Yokozuna.

8. The user must then set the `search' bucket property to `false' for all indexed buckets.

9. Then the user can disable Riak Search on all nodes.

10. Eventually, when the user is convinced the Riak Search data is no longer needed the merge index directories may be deleted to reclaim disk space.
