---
title: Latency Reduction Checklist
project: riak
version: 1.0.0+
document: guide
audience: intermediate
keywords: [operator, troubleshooting, latency]
---

Although _some_ latency is unavoidable in distributed systems like Riak, there
are a number of actions that can be undertaken to reduce latency to the lowest
levels possible within a cluster. In this guide, we'll list potential sources of
undue latency and what you can do about it.

## Sources of Latency

Excess latency in Riak most frequently stems from one of the following sources.

## Large Objects

Riak always performs best with smaller objects. We recommend keeping all objects
stored in Riak smaller than 1 MB, preferably below 100 KB. Large objects lead
to increased I/O activity and strain on memory resources.

Larger objects---even a few of them---can impact even requests that are
unrelated to those objects due to the nature of networking buffers in
distributed Erlang.

If your use case requires larger objects, we recommend checking out [[Riak CS]].

#### Mitigation

## Siblings

## Compaction and Merging

## OS Tuning

## IO/Network Bottlenecks

## Overload Protection
