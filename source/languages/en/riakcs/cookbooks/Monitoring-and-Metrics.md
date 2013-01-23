---
title: Monitoring and Metrics
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
---

Riak CS provides operational statistics which can be useful for monitoring through the Folsom statistics library, and initial probes for analysis of the running system with [[DTrace|http://dtrace.org/blogs/about/]].

## Operational Statistics
Much like Riak, Riak CS exposes statistics on critical operations which are commonly used for monitoring, alerting, and trend analysis. These statistics can be accessed through HTTP requests to the following resource:

```
/riak-cs/stats
```

Latency histograms showing mean, median, 95th, and 99th percentiles, and meters showing the count and rate are included in the output for each statistic.

The results include histograms and counters for the following operations:

* **block_get**: Total BLOCK GET operations performed
* **block_put**: Total BLOCK GET operations performed
* **block_delete**: Total BLOCK DELETE operations performed
* **service_get_buckets**: Total GET BUCKETS operations performed
* **bucket_list_keys**: Total BUCKET LIST KEYS operations performed
* **bucket_create**: Total BUCKET CREATE operations performed
* **bucket_delete**: Total BUCKET DELETE operations performed
* **bucket_get_acl**: Total BUCKET GET ACL operations performed
* **bucket_put_acl**: Total BUCKET PUT ACL operations performed
* **object_get**: Total GET operations performed
* **object_put**: Total PUT operations performed
* **object_head**: Total OBJECT HEAD operations performed
* **object_delete**: Total OBJECT DELETE operations performed
* **object_get_acl**: Total OBJECT GET ACL operations performed
* **object_put_acl**: Total OBJECT PUT ACL operations performed

## DTrace Probes
Riak CS is built with some probes for use with [[DTrace|http://dtrace.org/blogs/about/]] to inspect certain operations in the live system, which can be helpful to diagnose issues.

### Usage Examples
The following are examples of using DTrace for inspecting various components of a running Riak CS installation.

**Trace user object requests**:

    dtrace -qn 'erlang*:::user_trace* /arg2 == 703/ {printf("pid %s: mod %s op %s: user %s bucket/file %s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7), copyinstr(arg8), copyinstr(arg9));}'

**Trace webmachine resource execution**:

    dtrace -qn 'erlang*:::user_trace* /arg2 == 705/ {printf("pid %s: %s:%s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7));}'

<div class="info"><div class="title">DTrace Support</div> Work on packaging of Riak CS for SmartOS and other operating systems with DTrace support is ongoing with a goal to provide enhanced ability to diagnose low level issues in instances of Riak CS running on such operating systems.</div>
