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
canonical_link: "https://docs.basho.com/riak/kv/latest/configuring/search"
---

[usage search]: /riak/kv/2.0.7/developing/usage/search
[usage search schema]: /riak/kv/2.0.7/developing/usage/search-schemas
[usage search data types]: /riak/kv/2.0.7/developing/usage/searching-data-types
[usage custom extractors]: /riak/kv/2.0.7/developing/usage/custom-extractors
[config reference]: /riak/kv/2.0.7/configuring/reference
[config reference#search]: /riak/kv/2.0.7/configuring/reference/#search
[glossary aae]: /riak/kv/2.0.7/learn/glossary/#active-anti-entropy-aae
[security index]: /riak/kv/2.0.7/using/security/


This document covers how to use the Riak Search (with
[Solr](http://lucene.apache.org/solr/) integration) subsystem from an
operational perspective. 

For a simple reference of the available configs & their defaults, go [here][config reference#search].

If you are looking developer-focused docs, we recommend the following:

* [Using Search][usage search]
* [Search Schema][usage search schema]
* [Custom Search Extractors][usage custom extractors]
* [Riak KV Data Types and Search][usage search data types]

##Overview

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

**New Configs:** 

`solrq_batch_min` (default: 1) The minimum batch size, in number of Riak objects. Any batches that are smaller than this amount will not be immediately flushed to Solr, but are guaranteed to be flushed within solrq_delayms_max milliseconds (see below).

`solrq_batch_max` (default: 100) The maximim batch size, in number of Riak objects. Any batches that are larger than this amount will be split, where the first solrq_batch_max objects will be flushed to Solr, and the remaining objects enqueued for that index will be retained until the next batch is delivered. This parameter ensures that at most solrq_batch_max objects will be delivered into Solr in any given request.

`solrq_delayms_max` (default: 1000) The maximim delay (in milliseconds) between notification to flush batches to Solr. This setting is used to increase or decrease the frequency of batch delivery into Solr, specifically for relatively low-volume input into Riak. This setting ensures that data will be delivered into Solr in accordance with the solrq_batch_min and solrq_batch_max settings within the specified interval. Batches that are smaller than solrq_batch_min will be delivered to Solr within this interval. This setting will generally have no effect on heavily loaded systems.

`solrq_queue_hwm` (default: 10000) The queue high water mark. If the total number of queued messages in a Solrq worker instance exceed this limit, then the calling vnode will be blocked until the total number falls below this limit. This parameter exercises flow control between Riak and the Yokozuna batching subsystem, if writes into Solr start to fall behind.

`num_solrq` (default: 10) The number of solr queue workers to instantiate in the Yokozuna application. Solr queue workers are responsible for enqueing objects for insertion or update into Solr. Increasing the number of solrq distributes the queuing of objects, and can lead to greater throughput under high load, potentially at the expense of smaller batch sizes.

`num_solrq_helpers` (default: 10) The number of solr queue helpers to instantiate in the Yokozuna application. Solr queue helpers are responsible for delivering batches of data into Solr. Increasing the number of solrq helpers will increase concurrent writes into Solr.

`melt_attempts` (default: 3) The number of melts within the melt_time_window before a fuse will blow.

`melt_time_window` (default: 5000) The window of time (in milliseconds) in which melt_attempts melts will cause a fuse to blow.

`melt_reset_refresh` (default: 30000) The amount of elapsed time without a fuse blown event before a fuse will automatically heal.

`purge_blown_indices` (default: true) If true, then buffered data may be discarded from indices with blown fuses, if the solrq_queue_hwm is reached. See Fuses below for more information about fuses.

**Statistics**

`search_index_throughput_(count|one)` The total count of objects that have been indexed, per Riak node, and the count of objects that have been indexed within the metric measurement window

`search_index_latency_(min|mean|max|median|95|99|999)` The minimum, mean, maximum, median, 95th percentile, 99th percentile, and 99.9th percentile measurements of indexing latency, as measured from the time it takes to send a request to Solr to the time the response is received from Solr.

`search_index_batchsize_(min|mean|max|median|95|99)` The minimum, mean, maximum, median, 95th percentile, and 99th percentile measurements of the batch size across all indices and Solrq worker processes.

`search_index_blockedvnode_(count|one)` The total count of vnodes that have been blocked, per Riak node, and the count of blocked vnodes within the metric measurement window. VNodes are blocked when a Solrq worker exceeds it high water mark, as defined by the solrq_queue_hwm configuration setting. (See above)

`search_index_fail_(count|one)` The total count of failed attempts to index, per Riak node, and the count of index failures within the metric measurement window.

`search_query_throughput_(count|one)` The total count of queries, per Riak node, and the count of queries within the metric measurement window.

`search_query_latency_(min|mean|max|median|95|99|999)` The minimum, mean, maximum, median, 95th percentile, 99th percentile, and 99.9th percentile measurements of querying latency, as measured from the time it takes to send a request to Solr to the time the response is received from Solr.

`search_query_fail_(count|one)` The total count of failed queries, per Riak node, and the count of query failures within the metric measurement window.

While most of the default values are sufficient, you may have to
increase `search.solr.start_timeout` as more data is indexed, which may
cause Solr to require more time to start.


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
