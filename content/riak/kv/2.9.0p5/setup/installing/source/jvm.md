---
title: "Installing the JVM"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Installing the JVM"
    identifier: "installing_source_jvm"
    weight: 302
    parent: "installing_source"
toc: true
aliases:
  - /riak/2.9.0p5/ops/building/installing/jvm
  - /riak/kv/2.9.0p5/ops/building/installing/jvm
  - /riak/2.9.0p5/ops/building/installing/Installing-the-JVM
  - /riak/kv/2.9.0p5/ops/building/installing/Installing-the-JVM
  - /riak/2.9.0p5/installing/source/jvm/
  - /riak/kv/2.9.0p5/installing/source/jvm/
  - /riak/2.9.0p5/setup/installing/source/jvm/
  - /riak/2.9.0/setup/installing/source/jvm/
  - /riak/kv/2.9.0/setup/installing/source/jvm/
  - /riak/kv/2.9.0p1/setup/installing/source/jvm/
  - /riak/kv/2.9.0p2/setup/installing/source/jvm/
  - /riak/kv/2.9.0p3/setup/installing/source/jvm/
  - /riak/kv/2.9.0p4/setup/installing/source/jvm/
---


[usage search]: {{<baseurl>}}riak/kv/2.9.0p5/developing/usage/search

If you are using [Riak Search 2.0][usage search], codename Yokozuna,
you will need to install **Java 1.6 or later** to run [Apache
Solr](https://lucene.apache.org/solr/), the search platform that powers
Riak Search.

We recommend using Oracle's [JDK
7u25](http://www.oracle.com/technetwork/java/javase/7u25-relnotes-1955741.html).
Installation packages can be found on the [Java SE 7 Downloads
page](http://www.oracle.com/technetwork/java/javase/downloads/java-archive-downloads-javase7-521261.html#jdk-7u25-oth-JPR)
and instructions on the [documentation
page](http://www.oracle.com/technetwork/java/javase/documentation/index.html).

## Installing Solr on OS X

If you're using Riak Search on Mac OS X, you may see the following
error:

```java
java.net.MalformedURLException: Local host name unknown: <YOUR_HOST_NAME>
```

If you encounter this error, we recommend manually setting the hostname
for `localhost` using
[scutil](https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man8/scutil.8.html).

```bash
scutil --set HostName "localhost"
```
