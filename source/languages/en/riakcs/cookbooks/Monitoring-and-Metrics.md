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

Latency histograms showing mean, median, and 95th and 99th percentiles, as well as meters showing the count and rate, are included in the output for each statistic.

The results include histograms and counters for the following operations:

* `block_get` &mdash; Total BLOCK GET operations performed
* `block_put` &mdash; Total BLOCK GET operations performed
* `block_delete` &mdash; Total BLOCK DELETE operations performed
* `service_get_buckets` &mdash; Total GET BUCKETS operations performed
* `bucket_list_keys` &mdash; Total BUCKET LIST KEYS operations performed
* `bucket_create` &mdash; Total BUCKET CREATE operations performed
* `bucket_delete` &mdash; Total BUCKET DELETE operations performed
* `bucket_get_acl` &mdash; Total BUCKET GET ACL operations performed
* `bucket_put_acl` &mdash; Total BUCKET PUT ACL operations performed
* `object_get` &mdash; Total GET operations performed
* `object_put` &mdash; Total PUT operations performed
* `object_head` &mdash; Total OBJECT HEAD operations performed
* `object_delete` &mdash; Total OBJECT DELETE operations performed
* `object_get_acl` &mdash; Total OBJECT GET ACL operations performed
* `object_put_acl` &mdash; Total OBJECT PUT ACL operations performed

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
