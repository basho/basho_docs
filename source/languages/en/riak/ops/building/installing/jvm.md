---
title: Installing the JVM
project: riak
version: 2.0.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, jvm, java, search, solr]
---

If you are using [[Riak Search 2.0|Using Search]], codename Yokozuna,
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
