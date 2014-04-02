---
title: Riak Search Settings
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [search]
moved: {
    '1.4.0-': '/references/Riak-Search---Settings'
}
---

<div class="info">This document refers to the new Riak Search 2.0 with [[Solr|http://lucene.apache.org/solr/]] integration (codenamed Yokozuna). For information about the deprecated Riak Search, visit [[the old Riak Search Settings|http://docs.basho.com/riak/1.4.8/ops/advanced/configs/search/]].</div>

## Enabling Riak Search

Riak Search is integrated into Riak, but it is not enabled by default. You must enable it on every node's `riak.conf` file by setting `search` to `on`.

```riakconf
search = on
```

## JVM Installation

Because Solr is a Java application, you will need to install **Java 1.6 or later** on every node. We recommend installing Oracle's [JDK 7u25](http://www.oracle.com/technetwork/java/javase/7u25-relnotes-1955741.html). Installation packages can be found on the [Java SE 7 Downloads page](http://www.oracle.com/technetwork/java/javase/downloads/java-archive-downloads-javase7-521261.html#jre-7u25-oth-JPR) and instructions on the [documentation page](http://www.oracle.com/technetwork/java/javase/documentation/index.html).

### Riak Config Settings

Setting `search` to `on` is required, but other search settings are optional.

Field  | Default | Valid Values | Description
-------|---------|--------------|-----
`search` | `off` | `on` or `off`| Enable or disable Search
`search.solr.start_timeout` | `30s` | integer with time units (eg. 2m) | How long Riak will wait for Solr to start, attempts twice beore shutdown. Values lower than 1s will be rounded up to 1s
`search.solr.port` | 8093 | integer | The port number which Solr binds to, binds on every interface
`search.solr.jmx_port` | 8985 | integer | The port number which Solr JMX binds to, binds on every interface
`search.solr.jvm_options` | `-Xms1g -Xmx1g -XX:+UseStringCache -XX:+UseCompressedOops` | java commandline arguments | The options to pass to the Solr JVM.  Non-standard options, e.g. `-XX`, may not be portable across JVM implementations

While most of the default values are sufficient, you may have to increase `search.solr.start_timeout` as more data is indexed, requiring Solr more time to start.

## Solr JVM and Ports

Riak Search runs a Solr process per node to manage its index and search functionality. While the underlying project Yokozuna manages index distribution, node coverage for queries, [[Active Anti-Entropy (AAE)|Search Details#Active-Anti-Entroy-AAE-]], and JVM process management, you are expected to have plenty of RAM and diskspace available for running both Riak and the JVM running Solr. We recommend a minimum of 6GB of RAM per node.

Concerning ports, be sure to take the necessary security precautions to prevent exposing the extra solr and JMX ports to the outside world.


<!-- connecting to JMX -->
