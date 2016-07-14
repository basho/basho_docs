---
title: "Reindexing Data"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Reindexing Data"
    identifier: "cluster_operations_reindex_data"
    weight: 118
    parent: "managing_cluster_operations"
toc: true
canonical_link: "https://docs.basho.com/riak/kv/latest/using/cluster-operations/reindexing-data"
---

After making [changes to your search schema](../updating-search-schemas) we recommend reindexing existing search data. You can reindex data by either:

- [Creating a script to 're-PUT' all of the data](#reindex-data-with-custom-script).
- [Using the Riak Data Migrator](#reindex-data-with-riak-data-migrator)
- [Rebuilding Yokozuna Active Anty Entropy (AAE) trees](#reindex-data-with-aae-rebuild)

## Reindexing Data With Custom Script

You can reindex existing data by creating a script that 're-PUTs' all of your data. Below is an example script in <<LANGUAGE>>:

```ruby
# Reindexing script example
```

## Reindexing Data With Riak Data Migrator

In order to reindex search data using the [Riak Data Migrator tool](https://github.com/dankerrigan/riak-data-migrator) you must:

1. Step One
2. Step Two
3. Step Three

## Reindexing Data With AAE Rebuild

Another option for reindexing existing data is to:

1. [Delete the Solr data from the index](#deleting-solr-data).
2. [Expire the Yokozuna (YZ) Active Anti Entropy (AAE) trees](#expiring-yokozuna-active-anti-entropy-trees).

### Deleting Solr Data

First we need to delete the Solr data from the index with the updated the search schema. You do not need to delete the configurations.

```bash
# Example of deleting Solr data
```

We can confirm the Solr data is removed by running:

```bash
# Confirm Solr data removal
```

After we've successfully removed the Solr data from our index we can move on to the following step.

### Expiring Yokozuna Active Anti Entropy Trees

Next you'll need to force the Yokozuna (YZ) Active Anti Entropy (AAE) trees to expire. This will cause the trees to rebuild with the new search index. To do this, enter a `riak attach` session and run the following:

```erlang
rpc:multicall([node() | nodes()], yz_entropy_mgr, expire_trees, []).
```

Finally, allow the trees to rebuild with the new index. You can monitor the progress by running:

```bash
riak-admin search aae-status
```

## Active Anti Entropy Tuning

By default, search reindexing through [Active Anti Entropy (AAE)][../active-anti-entropy] happens at the same rate as KV AAE. AAE trees are rebuilt at a rate of 1 tree per node per hour, and AAE trees are expired once every week. With these settings it likely will take a great deal of time for the index to rebuild after the trees expire.

The [Active Anti Entropy][../active-anti-entropy] settings are described in our documentation. Of particular interest will be the following settings in `riak.conf` format (shown with their default values):

```riakconf
anti_entropy.concurrency_limit = 2
anti_entropy.tree.build_limit.number = 1
anti_entropy.tree.build_limit.per_timespan = 1h
anti_entropy.tree.expiry = 1w
```

You can experiment with setting `anti_entropy.tree.build_limit.per_timespan` to something shorter, perhaps 15m, and/or raising the `anti_entropy.tree.build_limit.number`. You can also raise the `anti_entropy.concurrency_limit` to 4. 

These changes should be made in `riak.conf` and do require a node restart to be applied. Please monitor the impact of these changes on your cluster's performance. We cannot currently adjust the AAE settings for KV and Search separately through `riak.conf`.  Changing these settings will allow the trees to rebuild faster and thus have your data reindexed faster; however, will place additional load on your cluster.

You can monitor the progress of the re-indexing with the `riak-admin search aae_status` command. Please be aware that until all partitions have been reindexed you may see inconsistent results due to the way the query plan is accessing different Solr backends that may not yet have all the data reindexed.
