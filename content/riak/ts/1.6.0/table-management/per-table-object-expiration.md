---
title: "Configure Per Table Object Expiration"
description: "Enabling and configuring per-table object expiration for Riak TS."
menu:
  riak_ts-1.6.0:
    name: "Per Table Object Expiration"
    identifier: "table_mng_per_table_expiry"
    weight: 300
    parent: "table_management"
project: "riak_ts"
project_version: "1.6.0"
toc: true
commercial_offering: true
version_history:
  in: "1.6.0+"
  locations:
    - [">=1.6.0", "table-management/per-table-object-expiration"]
aliases:
    - /riakts/1.6.0/table-management/per-table-object-expiration/
canonical_link: "https://docs.basho.com/riak/ts/latest/table-management/per-table-object-expiration/"
---

[ttl]: https://en.wikipedia.org/wiki/Time_to_live
[global expiry]: /riak/ts/1.6.0/table-management/global-object-expiration/
[expiry retention]: /riak/ts/1.6.0/table-management/global-object-expiration/#setting-retention-time
[expiry modes]: /riak/ts/1.6.0/table-management/global-object-expiration/#expiry-modes
[create table]: /riak/ts/1.6.0/table-management/creating-activating/
[create table with]: /riak/ts/1.6.0/table-management/creating-activating/#using-with

By default, LevelDB keeps all of your data. But Riak TS allows you to configure object expiration (`expiry`) or [time to live (TTL)][ttl] for your data on a [global][global expiry] or, when using Basho's Enterprise Edition, a per table basis.

Expiration is disabled by default, but enabling it lets you expire older objects to reclaim the space used or purge data with a limited time value.

## Enabling Expiry

Enabling object expiry on a per table basis is similar to [enabling expiry globally][global expiry]. The expiration setting, `leveldb.expiration`, must be enabled in your riak.conf file in order to use per table or global expiry features:

```riak.conf
leveldb.expiration = on
```

You also have the option of setting a [retention time][expiry retention] and an [expiry mode][expiry modes] in your riak.conf. For example:

```riak.conf
leveldb.expiration = on
leveldb.expiration.retention_time = 5h
leveldb.expiration.mode = whole_file
```

These settings will apply globally. So if you enable expiry on a table and the `default_time_to_live` or `expiration_mode` table properties are not set, the table will inherit the values set in riak.conf.

See [global expiry][global expiry] for a discussion of what happens to existing data when expiry is enabled.

## Expiry Table Properties

|Property Name|Default|Values|
|---|---|---|
|expiration|`disabled|``enabled` / `disabled`|
|default_time_to_live|`unlimited`|`unlimited` or a duration string|
|expiration_mode|`whole_file`|`use_global_config` / `per_item` / `whole_file`|

Each table can have one or more of the properties listed above. If any properties are omitted on the table, the property values in riak.conf will be used. If the properties are not set within riak.conf, the default values will be used.

## Creating Tables With Expiry

To enable object expiration on a per table basis, [create a table][create table] and specify the expiry properties for objects using the optional [WITH clause][create table with]. For example:

```sql
CREATE TABLE GeoCheckin
(
    id           SINT64    NOT NULL,
    region       VARCHAR   NOT NULL,
    state        VARCHAR   NOT NULL,
    time         TIMESTAMP NOT NULL,
    weather      VARCHAR NOT NULL,
    temperature  DOUBLE,
    PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
    )
) WITH (
    expiration = enabled
    default_time_to_live = 123m
    expiration_mode = whole_file
)
```

For existing tables, bucket properties can be modified with the `ALTER TABLE` command.

```
ALTER TABLE GeoCheckin WITH (
    expiration = enabled
    default_time_to_live = 123m
    expiration_mode = whole_file
)
```
