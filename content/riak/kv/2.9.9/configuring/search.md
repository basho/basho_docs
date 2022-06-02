---
title: "Riak Search Settings"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "Riak Search Settings"
    identifier: "configuring_search"
    weight: 160
    parent: "configuring"
toc: true
aliases:
  - /riak/2.9.9/ops/advanced/configs/search/
  - /riak/kv/2.9.9/ops/advanced/configs/search/
---

[usage search]: {{<baseurl>}}riak/kv/2.9.9/developing/usage/search
[usage search schema]: {{<baseurl>}}riak/kv/2.9.9/developing/usage/search-schemas
[usage search data types]: {{<baseurl>}}riak/kv/2.9.9/developing/usage/searching-data-types
[usage custom extractors]: {{<baseurl>}}riak/kv/2.9.9/developing/usage/custom-extractors
[cluster-ops aae throttle]: {{<baseurl>}}riak/kv/2.9.9/using/cluster-operations/active-anti-entropy/#throttling
[config reference]: {{<baseurl>}}riak/kv/2.9.9/configuring/reference
[config reference#search]: {{<baseurl>}}riak/kv/2.9.9/configuring/reference/#search
[glossary aae]: {{<baseurl>}}riak/kv/2.9.9/learn/glossary/#active-anti-entropy-aae
[security index]: {{<baseurl>}}riak/kv/2.9.9/using/security/

[java se downloads]: http://www.oracle.com/technetwork/java/javase/downloads
[java se docs]: http://www.oracle.com/technetwork/java/javase/documentation

This page covers how to use Riak Search (with
[Solr](http://lucene.apache.org/solr/) integration). 

For a simple reference of the available configs and their defaults, see the [configuration reference][config reference#search].

If you are looking to develop on or with Riak Search, take a look at:

* [Using Search][usage search]
* [Search Schema][usage search schema]
* [Custom Search Extractors][usage custom extractors]
* [Riak KV Data Types and Search][usage search data types]

## Overview

We'll be walking through:

1. [Prequisites](#prerequisites)
2. [Enable Riak Search](#enabling-riak-search)
3. [Search Configuration Settings](#search-config-settings)
4. [Additional Solr Information](#more-on-solr)

## Prerequisites 

Because Solr is a Java application, you will need to install **Java 7
or later** on every node. Installation packages can be found on the [Java SE Downloads
page][java se downloads] and instructions in the [Java SE documentation site][java se docs].


## Enabling Riak Search

Riak Search is not enabled by default, so you must enable it in every
node's [configuration file][config reference] as follows:

```riak.conf
search = on
```


## Search Config Settings

You will find all the Riak Search configuration settings in riak.conf. Setting `search` to `on` is required, but other search settings are optional. A handy reference list of these parameters can be found in our [configuration files][config reference#search] documentation.

### `search`

Enable or disable search; defaults to `off`. 

Valid values:  `on` or `off`

### `search.anti_entropy.data_dir`

The directory in which Riak Search stores files related to [active anti-entropy][glossary aae]; defaults to `./data/yz_anti_entropy`.

Valid values: a directory

### `search.anti_entropy.throttle`

Whether the throttle for Yokozuna active anti-entropy is enabled; defaults to `on`.

Valid values: `on` or `off`

You can read more about throttling [here][cluster-ops aae throttle].

### `search.anti_entropy.throttle.$tier.delay`

Set the throttling tiers delay for [active anti-entropy][glossary aae]; no default.

Each tier is a [minimum Solrq queue size](#search-anti-entropy-throttle-tier-solrq-queue-length) and a time-delay that the throttle should observe at that size and above. 

For example:

```
search.anti_entropy.throttle.tier1.solrq_queue_length = 0
search.anti_entropy.throttle.tier1.delay = 0ms
search.anti_entropy.throttle.tier2.solrq_queue_length = 40
search.anti_entropy.throttle.tier2.delay = 5ms
```
will introduce a 5 millisecond sleep for any queues of length 40 or higher. If configured, there must be a tier which includes a mailbox size of 0. Both [`.solrq_queue_length`](#search-anti-entropy-throttle-tier-solrq-queue-length) and `.delay` must be set for each tier. There is no limit to the number of tiers that may be specified. See [`search.anti_entropy.throttle`](#search-anti-entropy-throttle).

Valid values: Non-negative integer

### `search.anti_entropy.throttle.$tier.solrq_queue_length`

Set the throttling tiers for [active anti-entropy][glossary aae]; no default.

Each tier is a minimum Solrq queue size and a [time-delay](#search-anti-entropy-throttle-tier-delay) that the throttle
should observe at that size and above. 

For example:

```
search.anti_entropy.throttle.tier1.solrq_queue_length = 0
search.anti_entropy.throttle.tier1.delay = 0ms
search.anti_entropy.throttle.tier2.solrq_queue_length = 40
search.anti_entropy.throttle.tier2.delay = 5ms
```
will introduce a 5 millisecond sleep for any queues of length 40 or higher. If configured, there must be a tier which includes a mailbox size of 0. Both `.solrq_queue_length` and [`.delay`](#search-anti-entropy-throttle-tier-delay) must be set for each tier. There is no limit to the number of tiers that may be specified. See [`search.anti_entropy.throttle`](#search-anti-entropy-throttle).

Valid values: Non-negative integer

### `search.dist_query`

Enable this node in distributed query plans; defaults to `on`.  

If enabled, this node will participate in distributed Solr queries.  If disabled, the node will be excluded from Riak search cover plans, and will therefore never be consulted in a distributed query.  Note that this node may still be used to execute a query.  Use this flag if you have a long running administrative operation (e.g. reindexing) which requires that the node be removed from query plans, and which would otherwise result in inconsistent search results.

This setting can also be changed via `riak-admin` by issuing one of the following commands:

```
riak-admin set search.dist_query=off
```
 or 

``` 
riak-admin set search.dist_query=on
```

Setting this value in riak.conf is useful when you are restarting a node which was removed from search queries with the `riak-admin` feature. Setting `search.dis_query` in riak.conf will prevent the node from being included in search queries until it is fully spun up.

Valid values: `on` or `off`

### `search.index.error_threshold.failure_count`

The number of failures encountered while updating a search index within [`search.index.error_threshold.failure_interval`](#search-index-error-threshold-failure-interval) before Riak KV will skip updates to that index; defaults to `3`.

Valid values: Integer

### `search.index.error_threshold.failure_interval`

The window of time during which `search.index.error_threshold.failure_count` failures will cause Riak KV to skip updates to a search index; defaults to `5000`. 

If [`search.index.error_threshold.failure_count`](#search-index-error-threshold-failure-count) errors have occurred within this interval on a given search index, then Riak will skip updates to that index until the [`search.index.error_threshold.reset_interval`](#search-index-error-threshold-reset-interval) has passed.

Valid values: Milliseconds

### `search.index.error_threshold.reset_interval`

The amount of time it takes for updates to a given search index to resume/refresh once Riak KV has started skipping update operations; defaults to `30000`.

Valid values: Milliseconds

### `search.queue.batch.flush_interval`

The maximum delay between notification to flush batches to Solr; defaults to `1000` (milliseconds).

This setting is used to increase or decrease the frequency of batch delivery into Solr, specifically for relatively low-volume input into Riak KV. This setting ensures that data will be delivered into Solr in accordance with the `search.queue.batch.minimum` and `search.queue.batch.maximum` settings within the specified interval. Batches that are smaller than `search.queue.batch.minimum` will be delivered to Solr within this interval. This setting will generally have no effect on heavily loaded systems. You may use any time unit; the default is in milliseconds.

Valid values: `ms`, `s`, `m`, or `h`

### `search.queue.batch.maximum`

The maximum batch size, in number of Riak objects; defaults to `500`.

Any batches that are larger than this amount will be split, where the first `search.queue.batch.maximum` objects will be flushed to Solr and the remaining objects enqueued for that index will be retained until the next batch is delivered. This parameter ensures that at most `search.queue.batch.maximum` objects will be delivered into Solr in any given request.

Valid values: Integer

### `search.queue.batch.minimum`

The minimum batch size, in number of Riak objects; defaults to `10`.

Any batches that are smaller than this amount will not be immediately flushed to Solr, but are guaranteed to be flushed within the `search.queue.batch.flush_interval`.

Valid valus: Integer

### `search.queue.high_watermark`

The queue high water mark; defaults to `1000`. 

If the total number of queued messages in a Solrq worker instance exceed this limit, then the calling vnode will be blocked until the total number falls below this limit. This parameter exercises flow control between Riak KV and the Riak Search batching subsystem, if writes into Solr start to fall behind.

Valid values: Integer

### `search.queue.high_watermark.purge_strategy`

The strategy for how purging is handled when the `search.queue.high_watermark` is hit; defaults to `purge_one`.

Valid values:  `purge_one`, `purge_index`, or `off`

* `purge_one` removes the oldest item on the queue from an erroring (references to fuses blown in the code) index in order to get below the [`search.queue.high_watermark`](#search-queue-high-watermark) 
* `purge_index` removes all items associated with one random erroring (references to fuses blown in the code) index in order to get below the [`search.queue.high_watermark`](#search-queue-high-watermark)
* `off` disables purging

### `search.root_dir`

The root directory in which index data and configuration is stored; defaults to `./data/yz`.

Valid values: a directory

### `search.solr.jvm_options`

The options to pass to the Solr JVM; defaults to `-d64 -Xms1g -Xmx1g -XX:+UseStringCache -XX:+UseCompressedOops`.

Non-standard options (e.g. `-XX`) may not be portable across JVM implementations.

Valid values: Java command-line arguments

### `search.solr.jmx_port`

The port number to which Solr JMX binds (note: binds on every interface); defaults to `8985`.

Valid values: Integer

NB JMX ceased being a Riak feature in Riak KV 2.9.0p5. This setting is left here for reference but no longer affects anything.

### `search.solr.port`

The port number to which Solr binds (note: binds on every interface); defaults to `8093`.

Valid values: Integer

### `search.solr.start_timeout`

How long Riak KV will wait for Solr to start (attempts twice before shutdown); defaults to `30s`. 

Values lower than 1s will be rounded up to 1s. 

Valid values: Integer with time units (e.g. 2m)


## More on Solr
### Solr JVM and Ports

Riak Search runs one Solr process per node to manage its indexing and
search functionality. While the underlying project manages
index distribution, node coverage for queries, active anti-entropy
(AAE), and JVM process management, you should provide plenty of RAM and diskspace for running both Riak and the JVM running Solr. We recommend a minimum of 6GB of RAM per node.

Concerning ports, be sure to take the necessary [security][security index] precautions to prevent exposing the extra Solr ports
to the outside world.

### Solr for Operators

For further information on Solr monitoring, tuning, and performance, we
recommend the following documents for getting started:

* [Solr Monitoring](https://wiki.apache.org/solr/SolrMonitoring)
* [Solr Performance
    Factors](https://wiki.apache.org/solr/SolrPerformanceFactors)
* [Solr Performance
    Problems](https://wiki.apache.org/solr/SolrPerformanceProblems)
* [JConsole](http://docs.oracle.com/javase/7/docs/technotes/guides/management/jconsole.html)

A wide variety of other documentation is available from the Solr OSS
community.




