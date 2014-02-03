---
title: Monitoring and Metrics
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
---

Riak CS provides operational statistics that can be useful for monitoring through the Folsom statistics library, and initial probes for analysis of the running system with [DTrace](http://dtrace.org/blogs/about/).

## Operational Statistics

Much like Riak, Riak CS exposes statistics on critical operations that are commonly used for monitoring, alerting, and trend analysis. These statistics can be accessed through HTTP requests to the following resource:

```http
/riak-cs/stats
```

This will return a series of latency histograms and counters for all of the following operations:

* `block_get` --- Total BLOCK GET operations performed
* `block_put` --- Total BLOCK GET operations performed
* `block_delete` --- Total BLOCK DELETE operations performed
* `service_get_buckets` --- Total GET BUCKETS operations performed
* `bucket_list_keys` --- Total BUCKET LIST KEYS operations performed
* `bucket_create` --- Total BUCKET CREATE operations performed
* `bucket_delete` --- Total BUCKET DELETE operations performed
* `bucket_get_acl` --- Total BUCKET GET ACL operations performed
* `bucket_put_acl` --- Total BUCKET PUT ACL operations performed
* `object_get` --- Total GET operations performed
* `object_put` --- Total PUT operations performed
* `object_head` --- Total OBJECT HEAD operations performed
* `object_delete` --- Total OBJECT DELETE operations performed
* `object_get_acl` --- Total OBJECT GET ACL operations performed
* `object_put_acl` --- Total OBJECT PUT ACL operations performed

Alongside each of the above operations are meters showing the count and rate for the operation, as well as a latency histogram showing mean, median, and 95th and 99th percentiles. That list takes the following form:

```bash
<operation_name> [MeterCount, MeterRate, LatencyMean, LatencyMedian, Latency95, Latency99]
```

## DTrace Probes

Riak CS is built with some probes for use with [[DTrace|http://dtrace.org/blogs/about/]] to inspect certain operations in the live system, which can be helpful in diagnosing issues.

### Usage Examples

The following are examples of using DTrace for inspecting various components of a running Riak CS installation.

#### Trace User Object Requests

```bash
dtrace -qn 'erlang*:::user_trace* /arg2 == 703/ {printf("pid %s: mod %s op %s: user %s bucket/file %s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7), copyinstr(arg8), copyinstr(arg9));}'
```

#### Trace Webmachine Resource Execution

```bash
dtrace -qn 'erlang*:::user_trace* /arg2 == 705/ {printf("pid %s: %s:%s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7));}'
```

<div class="info"><div class="title">DTrace Support</div> Work on packaging of Riak CS for SmartOS and other operating systems with DTrace support is ongoing with the goal of providing enhanced ability to diagnose low-level issues in instances of Riak CS running on such operating systems.</div>
