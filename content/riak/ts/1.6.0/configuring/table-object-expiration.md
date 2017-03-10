---
title: "Configure Per Table Object Expiration"
description: "Enabling and configuring per-table object expiration for Riak TS."
menu:
  riak_ts-1.6.0:
    name: "Per Table Object Expiration"
    identifier: "config_table_expiry"
    weight: 330
    parent: "configure"
project: "riak_ts"
project_version: "1.6.0"
toc: true
commercial_offering: true
aliases:
    - /riakts/1.6.0/configuring/table-object-expiration/
canonical_link: "https://docs.basho.com/riak/ts/latest/configuring/table-object-expiration"
---

[ttl]: https://en.wikipedia.org/wiki/Time_to_live
[global expiry]: /riak/ts/1.6.0/configuring/global-object-expiration/
[create table]: /riak/ts/1.6.0/using/creating-activating/
[create table with]: /riak/ts/1.6.0/using/creating-activating/#using-with

By default, LevelDB keeps all of your data. But Riak TS allows you to configure object expiration (`expiry`) or [time to live (TTL)][ttl] for your data on a [global][global expiry] or per table basis.

Expiration is disabled by default, but enabling it lets you expire older objects to reclaim the space used or purge data with a limited time value.

## Enabling Expiry

To enable object expiry for global or per table use, add the `leveldb.expiration` setting to your riak.conf file:

```riak.conf
leveldb.expiration = on
```

{{% note %}}
Turning on object expiration will not retroactively expire previous data. Only data created while expiration is on will be scheduled for expiration.
{{% /note %}}

## Expiry Table Properties

|Property Name|Values|
|---|---|
|expiration|`enabled` / `disabled`|
|defaut_time_to_live|`unlimited` or a duration string|
|expiration_mode|`use_global_config` / `per_item` / `whole_file`|

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
    default_time_to_live = 123.4m
    expiration_mode = whole_file
)
```
