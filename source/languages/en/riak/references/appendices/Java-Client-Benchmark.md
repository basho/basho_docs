---
title: Java Client Benchmark
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [drivers, benchmark]
---

This article describes some comparative benchmarks for the [riak-java-client](http://github.com/basho/riak-java-client) undertaken as part of our ongoing development effort.

## Hardware

Generating the load is a MacBook Pro (2.53GHz core duo, 8GB ram, SSD drive) running [[Basho Bench|Benchmarking with Basho Bench]] against a 3 node Riak cluster. These are not full production spec machines but the results illustrate the performance characteristics of the Java Client.

The Riak cluster is made of three identical machines with the following specifications:

* 1 x Intel Core 2 Quad Q8300 LGA775 'Yorkfield' 2.5GHz 4MB-cache (1333FSB) Processor
* 8GB of DDR3 PC3-10666C9 1333MHz Dual Channel RAM
* 1 x Hitachi Deskstar 7K1000.C 1TB SATA-II 32MB Cache 7200RPM hard drive

Running erlang R14B03 and Riak from master.

The Riak cluster and the load generator are on the same Gigabit Ethernet LAN.

## Throughput/Latency 

The purpose is to benchmark the Riak Java Client, not the Riak cluster. _This is a comparative benchmark with the [[Erlang PB Client|https://github.com/basho/riak-erlang-client]]._

#### Method

1. Clean install of Riak on the cluster
2. Run 1 minute of basho_bench
3. Run a full hour of basho_bench
4. Repeat the above 3 times
5. Select the median result

Here is the graph of the one hour result for the [[riak-erlang-client|https://github.com/basho/riak-erlang-client]].

![One Hour Result ErlangPB](/images/one-hour-erlpb.png)

To summarise:

Around 2500 ops/per sec, get latency around 15ms in the 99.9th percentile, put latency around 22ms in the 99.9th percentile.

The basho_bench settings used were:

Max mode, 6 concurrent, 10,000 keys, 1kb values, R/W of 1, 50/50 gets/puts. 

### Java PB Client

The same method was repeated with the Java Client basho_bench driver. The JVMs used were Mac OS X 64-bit and Sun Linux 64 bit, both JDK1.6 with the following JVM arguments:

```bash
  -Xms1024M -Xmx2048M -server
```

Here is the result.

![One Hour Result JavaPB](/images/one-hour-javapb.png)

To summarise: Around 2500 ops/sec with latencies almost identical to the Erlang PB driver. Worth noting is that the Erlang driver runs in the same Erlang VM as basho_bench. The Java driver involves a remote erlang call to the Jinterface mailbox in a JVM, and the attendant serial/de-serialization. 

We ran the same benchmark with Java clients on the same hosts as the Riak cluster. One JVM on each node, 2 threads per JVM. The results are almost identical.

![One Hour Result JavaPB Dist](/images/one-hour-javapb-dist.png)

### Java HTTP Client

The same method was repeated once again, this time setting the transport to HTTP for the Java client. Here is the result graph.

![One Hour Java HTTP](/images/1hrnonagleserverhttpjava4.1_2.png)

To summarise: Just under 1000 ops/sec, with 99.9th percentile latencies on update under 30ms.

For the sake of comparision, we ran the same benchmark using the ibrowse http library and basho_bench's raw http bench driver. The results graph.

![One Hour Browser](/images/1hribrowsenonagleserver.png)

## Memory

### Java PB Client

Using the local JVM we repeated the process running [[jstat|http://download.oracle.com/javase/1,5.0/docs/tooldocs/share/jstat.html]] to observe generational memory usage and garbage collection activity.

Here are the results for the full hour graph for all generations,

![One Hour JavaGC](/images/one-hr-javagc.png)

Along with a close up of a minute of GC.

![Close Up JGC](/images/close-up-javagc.png)

This illustates that familiar, saw-tooth pattern. At the end of the test we forced full GC, notice that the steadily growing old-gen drops down nicely. Full GC required ~10ms. At this rate we estimate full GC will occur every 3-4 hours.

During the test there was no full GC. We re-ran the benchmark with 500kb values to force some full GC. Each full GC lasted ~10ms on each occasion.

### Java HTTP Client

We repeated the bench mark whilst using the Java HTTP client. The full hour graph and close up graph are very similar to the graphs for the PB client.

![Full Hour](/images/41hc_all.png)

![Close Up](/images/41hc_gc_close_up.png)

The heap this time being made up of ephermeral byte[] arrays, Strings and char[] arrays.

_Note: The [[bench_shim|https://github.com/basho/bench_shim]] code between the riak-java-client and basho_bench will contribute to the memory overhead and GC activity of the benchmark._

Whilst the memory usage is high, it is stable and predictable. The vast majority of heap space is taken up with ephemeral byte[] arrays, as is expected for a network client shunting data to Riak. We will continue to optimise the memory footprint. 

## Summary

The Riak Java Client provides a high level of throughput with low latency. The figures are at least on par with the native Erlang PB driver. The memory footprint is typical of Java applications, but with a stable and predictable GC pattern.
