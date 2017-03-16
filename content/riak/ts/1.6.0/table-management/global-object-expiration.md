---
title: "Configure Global Object Expiration"
description: "Enabling and configuring global object expiration for Riak TS."
menu:
  riak_ts-1.6.0:
    name: "Global Object Expiration"
    identifier: "table_mng_global_expiry"
    weight: 200
    parent: "table_management"
project: "riak_ts"
project_version: "1.6.0"
toc: true
version_history:
  in: "1.4.0+"
  locations:
    - [">=1.6.0", "table-management/global-object-expiration"]
    - ["<=1.5.1", "configuring/global-object-expiration"]
    - ["<=1.4.0",  "using/global-object-expiration"]
aliases:
    - /riakts/1.6.0/configuring/global-object-expiration/
    - /riakts/1.6.0/table-management/global-object-expiration/
canonical_link: "https://docs.basho.com/riak/ts/latest/table-management/global-object-expiration"
---

[ttl]: https://en.wikipedia.org/wiki/Time_to_live
[table expiry]: /riak/ts/1.6.0/table-management/per-table-object-expiration

By default, LevelDB keeps all of your data. But Riak TS allows you to configure object expiration (`expiry`) or [time to live (TTL)][ttl] for your data on a global or [per table basis][table expiry].

Expiration is disabled by default, but enabling it lets you expire older objects to reclaim the space used or purge data with a limited time value.

## Enabling Expiry

To enable global object expiry, add the `leveldb.expiration` setting to your riak.conf file:

```riak.conf
leveldb.expiration = on
```

Enabling expiry will instruct LevelDB to start tracking write times for data. New or updated data, along with recently-written data that has not yet been compacted, will be eligible for expiry. Older data will not expire until it is rewritten, due to data updates or internal read repair[XXX http://docs.basho.com/riak/kv/2.2.0/learn/concepts/replication/].

If expiration is enabled without configuring a retention time, LevelDB will track the age of data but will not expire it.


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

The minimum *effective* expiry period is 1 minute, and in fact data can remain for almost 2 minutes if written shortly after the "top" of the minute.

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
