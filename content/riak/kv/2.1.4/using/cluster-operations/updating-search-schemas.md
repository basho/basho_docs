---
title: "Updating Search Schemas"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Updating Search Schemas"
    identifier: "cluster_operations_update_search_schemas"
    weight: 117
    parent: "managing_cluster_operations"
toc: true
canonical_link: "https://docs.basho.com/riak/kv/latest/using/cluster-operations/updating-search-schemas"
---

The following page provides instruction on updating [search schemas](../../../developing/usage/search-schemas).

## Update the Search Schema file

Update the schema file, which is typically found here `$RIAK_HOST/search/index/famous`.

For example, the following creates or updates an index called `famous` that uses the default schema:

```bash
curl -XPUT $RIAK_HOST/search/index/famous \ -H 'Content-Type: application/json' \ -d '{"schema":"_yz_default"}'
```

## Update the schema in Riak KV

After you have made changes to the search schema file, the new schema must be updated within Riak KV. This can be done by performing a [rolling restart](../../repair-recovery/rolling-restart) or using `riak attach` to reload the schema.

### Performing a Rolling Restart

A [rolling restart](../../repair-recovery/rolling-restart) will trigger a Solr reload of the scema.

### Using `riak attach` to reload the schema

> **Note:**
>
> Please note the `riak attach` method below is a blocking, cluster-wide call but it will trigger a live reload of the Solr schema. New data will now contain the updated fields but existing data will not have their indexes updated.

You can also use `riak attach` to trigger a reload of the Solr schema. To do this, start a `riak attach` session then run:

```erlang
yz_index:reload(<<"index_name">>).
```
