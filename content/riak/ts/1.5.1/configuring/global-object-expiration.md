---
title: "Configure Global Object Expiration"
description: "Enabling and configuring global object expiration for Riak TS."
menu:
  riak_ts-1.5.1:
    name: "Global Object Expiration"
    identifier: "config_expiry"
    weight: 320
    parent: "configure"
project: "riak_ts"
project_version: "1.5.1"
toc: true
version_history:
  in: "1.4.0+"
  locations:
    - [">=1.5.1", "configuring/global-object-expiration"]
    - ["<=1.4.0",  "using/global-object-expiration"]
aliases:
    - /riakts/1.5.1/configuring/global-object-expiration/
---

[ttl]: https://en.wikipedia.org/wiki/Time_to_live

By default, LevelDB keeps all of your data. But Riak TS allows you to configure global object expiration (`expiry`) or [time to live (TTL)][ttl] for your data. 

{{% note %}}
Currently only global expiration is supported in Riak TS.
{{% /note %}}

Expiration is disabled by default, but enabling it lets you expire older objects to reclaim the space used or purge data with a limited time value.

## Enabling Expiry

To enable global object expiry, add the `leveldb.expiration` setting to your riak.conf file:

```riak.conf
leveldb.expiration = on
```

{{% note %}}
Turning on global object expiration will not retroactively expire previous data. Only data created while expiration is on will be scheduled for expiration.
{{% /note %}}

## Setting Retention Time

The `retention_time` setting is used to specify the time until objects expire.
Durations are set using a combination of an integer and a shortcut for the supported units:

- Milliseconds - `ms`
- Seconds - `s`
- Minutes - `m`
- Hours - `h`
- Days - `d`
- Weeks - `w`
- Fortnight - `f`

The following example configures objects to expire after 5 hours:

```riak.conf
leveldb.expiration = on
leveldb.expiration.retention_time = 5h
```

You can also combine durations. For example, let's say you wanted objects to expire after 8 days and 9 hours:

```riak.conf
leveldb.expiration = on
leveldb.expiration.retention_time = 8d9h
```

## Expiry Modes

Global expiration supports two modes:

- `whole_file` - the whole sorted string table (`.sst`) file is deleted when all of its objects are expired.
- `normal` - individual objects are removed as part of the usual compaction process.

We recommend using `whole_file` with time series data that has a similar lifespan, as it will be much more efficient. 

The following example configure objects to expire after 1 day:

```riak.conf
leveldb.expiration = on
leveldb.expiration.retention_time = 1d
leveldb.expiration.mode = whole_file
```

## Disable Expiry

To disable global object expiration, set `leveldb.expiration` to `off` in your riak.conf file. If expiration is disabled, the other 2 settings are ignored. For example:

```riak.conf
leveldb.expiration = off
leveldb.expiration.retention_time = 1d
leveldb.expiration.mode = whole_file
```
