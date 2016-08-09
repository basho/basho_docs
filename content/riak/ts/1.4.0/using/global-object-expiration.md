---
title: "Configure Global Object Expiration"
description: "Enabling and configuring global object expiration for Riak TS."
menu:
  riak_ts-1.4.0:
    name: "Configure Global Object Expiration"
    identifier: "config_expiry"
    weight: 320
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/global-object-expiration/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/global-object-expiration"
---

By default, LevelDB keeps all of your data. But Riak TS allows you to configure global object expiration (`expiry`) or [time to live (TTL)](https://en.wikipedia.org/wiki/Time_to_live) for your data. 

{{% note %}}
Currently only global expiration is supported in Riak TS. We plan to add table-level expiration in future versions.
{{% /note %}}

Using expiry lets you expire older objects to reclaim the space used or purge data with a limited time value. **This feature is disabled by default**.

## Enable and Configure Expiry

To enable global object expiry, add the `leveldb.expiration` setting to your `riak.conf` file:

```riak.conf
leveldb.expiration = on
```

For `app.config`-based configuration, add the `expiry_enabled` setting:

```app.config
{leveldb, [
  ...,
  {expiry_enabled, true},
  …,
  ]}
```

You can configure global object expiry using the `leveldb.expiration.retention_time` to specify a duration using a combination of an integer and a shortcut for the supported units:

- Milliseconds - `ms`
- Seconds - `s`
- Minutes - `m`
- Hours - `h`
- Days - `d`
- Weeks - `w`
- Fortnight - `f`

Setting objects to expire after 5 hours would look like `leveldb.expiration.retention_time = 5h`. You can also combine durations, for example: `8d9h`.

After setting the retention time for your objects, you should choose an expiration mode. Global expiration supports two modes:

- `whole_file` - whole SST file is deleted when all its objects are expired
- `normal` - individual objects are removed as part of usual compaction process

We recommend using `whole_file` with time series data that has a similar lifespan, as it will be much more efficient. 

The following examples configure objects to expire after 1 day:

```riak.conf
leveldb.expiration = on
leveldb.expiration.retention_time = 1d
leveldb.expiration.mode = whole_file
```

```app.config
{leveldb, [
  ...,
  {expiry_enabled, true},
  {expiry_minutes, 1440}, %% Sets the duration to 1 day
  {whole_file_expiry, true}
  ]}
```

## Disable Expiry

To disable global object expiration, set `leveldb.expiration` to `off` in your `riak.conf` file. For `app.config`-based configuration, set `expiry_enabled` to `false`. If expiration is disabled, the other 2 settings are ignored. For example:

```riak.conf
leveldb.expiration = off
leveldb.expiration.retention_time = 1d
leveldb.expiration.mode = whole_file
```

```app.config
{leveldb, [
  ...
  {expiry_enabled, false},
  {expiry_minutes, 1440},
  {whole_file_expiry, true}
}].
```
