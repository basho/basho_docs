---
title: "Search Settings"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Search Settings"
    identifier: "configuring_search"
    weight: 108
    parent: "configuring"
toc: true
aliases:
  - /riak/2.0.7/ops/advanced/configs/search/
  - /riak/kv/2.0.7/ops/advanced/configs/search/
---

[usage search]: {{<baseurl>}}riak/kv/2.0.7/developing/usage/search
[usage search schema]: {{<baseurl>}}riak/kv/2.0.7/developing/usage/search-schemas
[usage search data types]: {{<baseurl>}}riak/kv/2.0.7/developing/usage/searching-data-types
[usage custom extractors]: {{<baseurl>}}riak/kv/2.0.7/developing/usage/custom-extractors
[config reference]: {{<baseurl>}}riak/kv/2.0.7/configuring/reference
[config reference#search]: {{<baseurl>}}riak/kv/2.0.7/configuring/reference/#search
[glossary aae]: {{<baseurl>}}riak/kv/2.0.7/learn/glossary/#active-anti-entropy-aae
[security index]: {{<baseurl>}}riak/kv/2.0.7/using/security/


This document covers how to use the Riak Search (with
[Solr](http://lucene.apache.org/solr/) integration) subsystem from an
operational perspective. 

For a simple reference of the available configs & their defaults, go [here][config reference#search].

If you are looking developer-focused docs, we recommend the following:

* [Using Search][usage search]
* [Search Schema][usage search schema]
* [Custom Search Extractors][usage custom extractors]
* [Riak KV Data Types and Search][usage search data types]

## Overview

We'll be walking through:

1. [Prequisites][#prerequisites]
2. [Enable Riak Search][#enabling-riak-search]
3. [Riak.conf Configuration Settings][#riak-config-settings]
4. [Additional Solr Information][#more-on-solr]


## Prerequisites 

Because Solr is a Java application, you will need to install **Java 1.6
or later** on every node. We recommend installing Oracle's [JDK
7u25](http://www.oracle.com/technetwork/java/javase/7u25-relnotes-1955741.html).
Installation packages can be found on the [Java SE 7 Downloads
page](http://www.oracle.com/technetwork/java/javase/downloads/java-archive-downloads-javase7-521261.html#jre-7u25-oth-JPR)
and instructions on the [documentation
page](http://www.oracle.com/technetwork/java/javase/documentation/index.html).


## Enabling Riak Search

Riak Search is not enabled by default, so you must enable it in every
node's [configuration file][config reference] as follows:

```riak.conf
search = on
```


## Riak Config Settings

Setting `search` to `on` is required, but other search settings are
optional. A list of these parameters can also be found in our
[configuration files][config reference#search] documentation.

Field | Default | Valid values | Description
:-----|:--------|:-------------|:-----------
`search` | `off` | `on` or `off` | Enable or disable Search
`search.anti_entropy.data_dir` | `./data/yz_anti_entropy` | Directory | The directory in which Riak Search stores files related to [active anti-entropy][glossary aae]
`search.root_dir` | `./data/yz` | Directory | The root directory in which index data and configuration is stored
`search.solr.start_timeout` | `30s` | Integer with time units (eg. 2m) | How long Riak will wait for Solr to start (attempts twice before shutdown). Values lower than 1s will be rounded up to 1s.
`search.solr.port` | `8093` | Integer | The port number to which Solr binds (note: binds on every interface)
`search.solr.jmx_port` | `8985` | Integer | The port number to which Solr JMX (note: binds on every interface)
`search.solr.jvm_options` | `-d64 -Xms1g -Xmx1g -XX:+UseStringCache -XX:+UseCompressedOops` | Java command-line arguments | The options to pass to the Solr JVM. Non-standard options, e.g. `-XX`, may not be portable across JVM implementations.
`search.queue.batch.minimum` | `1` | Integer | The minimum batch size, in number of Riak objects. Any batches that are smaller than this amount will not be immediately flushed to Solr, but are guaranteed to be flushed within the `search.queue.batch.flush_interval`.
`search.queue.batch.maximum`| `100` | Integer | The maximim batch size, in number of Riak objects. Any batches that are larger than this amount will be split, where the first `search.queue.batch.maximum` objects will be flushed to Solr and the remaining objects enqueued for that index will be retained until the next batch is delivered. This parameter ensures that at most `search.queue.batch.maximum` objects will be delivered into Solr in any given request.
`search.queue.batch.flush_interval` | `1000` | `ms`, `s`, `m`, `h` | The maximum delay between notification to flush batches to Solr. This setting is used to increase or decrease the frequency of batch delivery into Solr, specifically for relatively low-volume input into Riak. This setting ensures that data will be delivered into Solr in accordance with the `search.queue.batch.minimum` and `search.queue.batch.maximum` settings within the specified interval. Batches that are smaller than `search.queue.batch.minimum` will be delivered to Solr within this interval. This setting will generally have no effect on heavily loaded systems. You may use any time unit; the default is in milliseconds.
`search.queue.high_watermark` | `10000` | Integer | The queue high water mark. If the total number of queued messages in a Solrq worker instance exceed this limit, then the calling vnode will be blocked until the total number falls below this limit. This parameter exercises flow control between Riak and the Riak Search batching subsystem, if writes into Solr start to fall behind.
`search.queue.worker_count` | `10` | Integer | The number of Solr queue workers to instantiate. Solr queue workers are responsible for enqueing objects for insertion or update into Solr. Increasing the number of Solrq workers distributes the queuing of objects and can lead to greater throughput under high load, potentially at the expense of smaller batch sizes.
`search.queue.helper_count` | `10` | Integer | The number of Solr queue helpers to instantiate. Solr queue helpers are responsible for delivering batches of data into Solr. Increasing the number of Solrq helpers will increase concurrent writes into Solr.
`search.index.error_threshold.failure_count` | `3` | Integer | The number of failures encountered while updating a search index within `search.index.error_threshold.failure_interval` before Riak will skip updates to that index.
`search.index.error_threshold.failure_interval` | `5000` | Milliseconds | The window of time during which `search.index.error_threshold.failure_count` failures will cause Riak to skip updates to a search index. If `search.index.error_threshold.failure_count` errors have occurred within this interval on a given search index, then Riak will skip updates to that index until the `search.index.error_threshold.reset_interval` has passed.
`search.index.error_threshold.reset_interval` | `30000` | Milliseconds | The amount of time it takes for updates to a given search index to resume/refresh once Riak has started skipping update operations.
`search.queue.high_watermark.purge_strategy` | `purge_one` | `purge_one`, `purge_index`, `purge_all`, or `off` | The strategy for how we handle purging when we hit the `search.queue.high_watermark`. The options: <br> * `purge_one` removes the oldest item on the queue from an erroring (references to fuses blown in the code) index in order to get below the `search.queue.high_watermark`,</br> <br> * `purge_index` removes all items associated with one random erroring (references to fuses blown in the code) index in order to get below the `search.queue.high_watermark`,</br> <br> * `purge_all` removes all items associated with all erroring (references to fuses blown in the code) indices in order to get below the `search.queue.high_watermark`, and</br> <br> *`off` disables purging.</br>


## More on Solr
### Solr JVM and Ports

Riak Search runs one Solr process per node to manage its indexing and
search functionality. While the underlying project manages
index distribution, node coverage for queries, active anti-entropy
(AAE), and JVM process management, you should provide plenty of RAM and diskspace for running both Riak and the JVM running Solr. We recommend a minimum of 6GB of RAM per node.

Concerning ports, be sure to take the necessary [security][security index] precautions to prevent exposing the extra Solr and JMX ports
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
