---
title: "Leveled"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Leveled"
    identifier: "planning_backend_leveled"
    weight: 101
    parent: "planning_choose_backend"
toc: true
aliases:
  - /riak/2.9.0p5/ops/advanced/backends/leveled/
  - /riak/kv/2.9.0p5/ops/advanced/backends/leveled/
  - /riak/2.9.0p5/setup/planning/backend/leveled/
  - /riak/2.9.0/setup/planning/backend/leveled/
  - /riak/kv/2.9.0/setup/planning/backend/leveled/
  - /riak/kv/2.9.0p1/setup/planning/backend/leveled/
  - /riak/kv/2.9.0p2/setup/planning/backend/leveled/
  - /riak/kv/2.9.0p3/setup/planning/backend/leveled/
  - /riak/kv/2.9.0p4/setup/planning/backend/leveled/
---


[glossary vnode]: {{<baseurl>}}riak/kv/2.9.0p5/learn/glossary/#vnode
[config reference]: {{<baseurl>}}riak/kv/2.9.0p5/configuring/reference
[perf index]: {{<baseurl>}}riak/kv/2.9.0p5/using/performance
[config reference#aae]: {{<baseurl>}}riak/kv/2.9.0p5/configuring/reference/#active-anti-entropy

[Leveled](https://github.com/martinsumner/leveled) is a simple Key-Value store based on the concept of Log-Structured Merge Trees, with the following characteristics:

- Optimised for workloads with larger values (e.g. > 4KB).
- Explicitly supports HEAD requests in addition to GET requests:
- Splits the storage of value between keys/metadata and body (assuming some definition of metadata is provided);
- Allows for the application to define what constitutes object metadata and what constitutes the body (value-part) of the object - and assign tags to objects to manage multiple object-types with different extraction rules;
- Stores keys/metadata in a merge tree and the full object in a journal of CDB files
- Allowing for HEAD requests which have lower overheads than GET requests; and
- Queries which traverse keys/metadatas to be supported with fewer side effects on the page cache than folds over keys/objects.
- Support for tagging of object types and the implementation of alternative store behaviour based on type.
- Allows for changes to extract specific information as metadata to be returned from HEAD requests;
- Potentially usable for objects with special retention or merge properties.
- Support for low-cost clones without locking to provide for scanning queries (e.g. secondary indexes).
- Low cost specifically where there is a need to scan across keys and metadata (not values).
- Written in Erlang as a message passing system between Actors.


## Strengths

1. leveled was developed specifically as a potential backend for Riak, with features such as:
      * Support for secondary indexes
      * Multiple fold types
      * Auto expiry of objects
    Enabling compression means more CPU usage but less disk space. Compression
    is especially good for text data, including raw text, Base64, JSON, etc.
2. Optimised for workloads with larger values (e.g. > 4KB).
3. Explicitly supports HEAD requests in addition to GET requests.
4. Support for low-cost clones without locking to provide for scanning queries (e.g. secondary indexes).

## Weaknesses

1. Leveled is still a comparatively new technology and more likely to suffer from edge case issues than Bitcask or LevelDB simply because they've been around longer and have been more thoroughly tested via usage in customer environments.
2. Leveled works better with medium to larger sized objects. It works perfectly well with small objects but the additional diskspace overhead may render LevelDB a better choice if disk space is at a premium and all of your data will be exclusively limited a few KB or less. This may change as Leveled matures though.

## Installing leveled

Leveled is included with Riak KV 2.9.0 and beyond, so there is no need to install anything further.

```riakconf
storage_backend = leveled
```

```appconfig
{riak_kv, [
    %% ...
    {storage_backend, riak_kv_leveled_backend},
    %% ...
    ]}
```

## Configuring leveled

Leveled's default behavior can be modified by adding/changing
parameters in the `leveled` section of the [`riak.conf`][config reference]. The section below details the parameters you'll use to modify leveled.

The configuration values that can be set in your
[`riak.conf`][config reference] for leveled are as follows:

Config | Description | Default
:------|:------------|:-------
`leveled.data_root` | leveled data root. | `./data/leveled`
`leveled.sync_strategy` | Strategy for flushing data to disk. | `none`
`leveled.compression_method` | Compression Method. | `native`
`leveled.compression_point` | Compression Point - The point at which compression is applied to the Journal. | `on_receipt`
`leveled.log_level` | Log Level - Set the minimum log level to be used within leveled. | `info`
`leveled.journal_size` | The approximate size (in bytes) when a Journal file should be rolled. | `1000000000`
`leveled.compaction_runs_perday` | The number of journal compactions per vnode per day | `24`
`leveled.compaction_low_hour` | The hour of the day in which journal compaction can start. | `0`
`leveled.compaction_top_hour` | The hour of the day, after which journal compaction should stop.  | `23`
`leveled.max_run_length` | Max Journal Files Per Compaction Run. | `4`

### Recommended Settings

Below are **general** configuration recommendations for Linux
distributions. Individual users may need to tailor these settings for
their application.

#### sysctl

For production environments, please see [System Performance Tuning][perf index]
for the recommended `/etc/sysctl.conf` settings.

#### Block Device Scheduler

Beginning with the 2.6 kernel, Linux gives you a choice of four I/O
[elevator models](http://www.gnutoolbox.com/linux-io-elevator/). We
recommend using the NOOP elevator. You can do this by changing the
scheduler on the Linux boot line: `elevator=noop`.

#### No Entropy

If you are using https protocol, the 2.6 kernel is widely known for
stalling programs waiting for SSL entropy bits. If you are using https,
we recommend installing the
[HAVEGE](http://www.irisa.fr/caps/projects/hipsor/) package for
pseudorandom number generation.

#### clocksource

We recommend setting `clocksource=hpet` on your Linux kernel's `boot`
line. The TSC clocksource has been identified to cause issues on
machines with multiple physical processors and/or CPU throttling.

#### swappiness

We recommend setting `vm.swappiness=0` in `/etc/sysctl.conf`. The
`vm.swappiness` default is 60, which is aimed toward laptop users with
application windows. This was a key change for MySQL servers and is
often referenced in database performance literature.

## Implementation Details

[Leveled](https://github.com/martinsumner/leveled) is an open source project that has been developed specifically as a backend option for Riak, rather than a generic backend.
