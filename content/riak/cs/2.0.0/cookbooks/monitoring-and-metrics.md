---
title: "Monitoring and Metrics"
description: ""
menu:
  riak_cs-2.0.0:
    name: "Monitoring & Metrics"
    identifier: "advanced_monitor_metrics"
    weight: 101
    parent: "run_advanced"
project: "riak_cs"
project_version: "2.0.0"
aliases:
  - /riakcs/2.0.0/cookbooks/monitoring-and-metrics/
---

[amazon]: http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html
[s3 api]: {{< baseurl >}}riak/cs/latest/references/apis/storage/s3/

Riak S2 (CS) includes metrics and operational statistics to help you monitor your system in more detail and diagnose system issues more easily. There are three major categories of metrics:

1. Frontend API performance
2. Backend Riak performance (Stanchion)
3. S2 internal performance
 
Metrics are also available for Stanchion, in addition to the Stanchion-specific `stanchion-admin` command and `/stats` HTTP endpoint. 

provides operational statistics that can be useful for
monitoring through the Folsom statistics library, and initial probes for
analysis of the running system with
[DTrace](http://dtrace.org/blogs/about/).

>**Note: Older Versions of Riak S2**
>
>All statistics available in versions of Riak S2 below 2.0.x have either been renamed or removed entirely.


## Using Metrics

Riak S2 exposes statistics on critical operations that
are commonly used for monitoring, alerting, and trend analysis. These
statistics can be accessed through the command line:

```bash
riak-cs-admin status
```

or through HTTP requests to the following resource:

```http
/riak-cs/stats
```

>**Note**
>
>In order to access statistics from the <code>/stats</code> endpoint, you
must issue signed requests containing the admin user's access key and
secret key. The interface used by Riak S2 is directly analogous to that
of Amazon S3. For more information on signed requests, see [Amazon's
documentation][amazon].
>
>Unsigned requests will yield a <code>403 Forbidden</code> error.

## `riak-cs-admin status`

Running `riak-cs-admin status` will show the names and values of all available metrics. 

There are too many metrics (over 1000) to list all of them here. The following sections provide an overview of each major statistic category, associated prefixes, and major operations for that category.

### S3 API statistics

S3 API statistics start with one of the following prefixes (all of which are names for S3 APIs):

- `service`
- `bucket`
- `list`
- `multiple_delete`
- `object`
- `multipart`

Each prefix is typically followed by operations such as:

- `put`
- `get`
- `delete`

Operation | Description
:---------|:-----------
`service_get` | GET Service
`bucket_(put∣head∣delete)` | PUT, HEAD, DELETE Bucket
`bucket_acl_(get∣put)` | PUT, GET Bucket ACL
`bucket_policy_(get∣put∣delete)` | PUT, GET, DELETE Bucket Policy
`bucket_location_get` | GET Bucket Location
`list_uploads` | listing all multipart uploads
`multiple_delete` | Delete Multiple Objects
`list_objects` | listing all objects in a bucket, equally GET Bucket
`object_(get∣put∣delete)` | GET, PUT, DELETE, HEAD Objects
`object_put_copy` | PUT Copy Object
`object_acl` | GET, PUT Object ACL
`multipart_post` | Initiate a multipart upload
`multipart_upload_put` | PUT Multipart Upload, putting a part of an object by copying from existing object
`multipart_upload_post` | complete a multipart upload
`multipart_upload_delete` | delete a part of a multipart upload
`multipart_upload_get` | get a list of parts in a multipart upload

See the [S3 API documentation][s3 api] for information on all available APIs.

### Stanchion access statistics

Stanchion access statistics start with the prefix `velvet`.

These statistics cover latency and counts for the Stanchion process creating/updating/deleting buckets or creating users. Stanchion access statistics can help determine if latency or slow requests are in Stanchion.

Operation | Description
:---------|:-----------
`velvet_create_user` | requesting creating a user to Stanchion
`velvet_update_user` | requesting updating a user to Stanchion
`velvet_create_bucket` | requesting creating a bucket to Stanchion
`velvet_delete_bucket` | requesting deleting a bucket to Stanchion
`velvet_set_bucket_acl` | requesting updating a bucket ACL to Stanchion
`velvet_set_bucket_policy` | requesting putting a new bucket policy to Stanchion
`velvet_delete_bucket_policy` | requesting deleting a policy of the bucket to Stanchion

### Riak access statistics

Riak access statistics start with the prefix `riakc`.

These statistics cover latency and call counts to Riak PB API. Riak access statistics are useful in determining the source of latency. For example getting a user record, bucket record, or updating manifests.

The `riakc` prefix is typically followed by operations like: 

- `put`
- `get`

And their targets, such as manifests or blocks.

Operation | Description
:---------|:-----------
`riakc_ping` | ping PB API. invoked by /riak-cs/ping
`riakc_get_cs_bucket` | getting a bucket record
`riakc_get_cs_user_strong` | getting a user record with PR=all
`riakc_get_cs_user` | getting a user record with R=quorum and PR=one
`riakc_put_cs_user` | putting a user record after create/deleting a bucket
`riakc_get_manifest` | getting a manifest
`riakc_put_manifest` | putting a manifest
`riakc_delete_manifest` | deleting a manifest (invoked via GC)
`riakc_get_block_n_one` | getting a block with N=1 without sloppy quorum
`riakc_get_block_n_all` | getting a block with N=3 after N=1 get failed
`riakc_get_block_remote` | getting a block after N=3 get resulted in not found
`riakc_get_block_legacy` | getting a block when N=1 get is turned off
`riakc_put_block` | putting a block
`riakc_put_block_resolved` | putting a block when block siblings resolution is invoked
`riakc_head_block` | heading a block, invoked via GC
`riakc_delete_block_constrained` | first trial to delete block with PW=all
`riakc_delete_block_secondary` | second trial to delete block with PW=quorum, after PW=all failed
`riakc_(get∣put)_gc_manifest_set` | invoked when a manifest is being moved to GC bucket
`riakc_(get∣delete)_gc_manifest_set` | invoked when manifests are being collected
`riakc_(get∣put)_access` | getting access stats, putting access stats
`riakc_(get∣put)_storage` | getting storage stats, putting storage stats
`riakc_fold_manifest_objs` | invoked inside GET Bucket (listing objects within a bucket)
`riakc_mapred_storage` | stats on each MapReduce job performance
`riakc_list_all_user_keys` | all users are listed out when starting storage calculation
`riakc_list_all_manifest_keys` | only used when deleting a bucket to verify it's empty
`riakc_list_users_receive_chunk` | listing users invoked via /riak-cs/users API.
`riakc_get_uploads_by_index` |
`riakc_get_user_by_index` |
`riakc_get_gc_keys_by_index` |
`riakc_get_cs_buckets_by_index` |
`riakc_get_clusterid` | invoked when for the first time when a proxy_get is performed


## `/riak-cs/stats`

That will return a JSON object containing a series of latency histograms
and counters for a variety of operations, e.g. `object_get` and
`block_put`. Alongside each operation there will be a list showing the
count and rate for the operation, as well as a latency histogram showing
mean, median, and 95th and 99th percentiles:

```json
<operation_name>: [MeterCount, MeterRate, LatencyMean, LatencyMedian, Latency95, Latency99]
```

You will see a list of that form for each of the following operations:

Operation | Description
:---------|:-----------
`block_get` | Total BLOCK GET operations performed
`block_put` | Total BLOCK GET operations performed
`block_delete` | Total BLOCK DELETE operations performed
`service_get_buckets` | Total GET BUCKETS operations performed
`bucket_list_keys` | Total BUCKET LIST KEYS operations performed
`bucket_create` | Total BUCKET CREATE operations performed
`bucket_delete` | Total BUCKET DELETE operations performed
`bucket_get_acl` | Total BUCKET GET ACL operations performed
`bucket_put_acl` | Total BUCKET PUT ACL operations performed
`object_get` | Total GET operations performed
`object_put` | Total PUT operations performed
`object_head` | Total OBJECT HEAD operations performed
`object_delete` | Total OBJECT DELETE operations performed
`object_get_acl` | Total OBJECT GET ACL operations performed
`object_put_acl` | Total OBJECT PUT ACL operations performed

## Stanchion

## DTrace Probes

Riak CS is built with some probes for use with
[DTrace](http://dtrace.org/blogs/about/) to inspect certain operations
in the live system, which can be helpful in diagnosing issues.

### Usage Examples

The following are examples of using DTrace for inspecting various
components of a running Riak CS installation.

#### Trace User Object Requests

```bash
dtrace -qn 'erlang*:::user_trace* /arg2 == 703/ {printf("pid %s: mod %s op %s: user %s bucket/file %s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7), copyinstr(arg8), copyinstr(arg9));}'
```

#### Trace Webmachine Resource Execution

```bash
dtrace -qn 'erlang*:::user_trace* /arg2 == 705/ {printf("pid %s: %s:%s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7));}'
```

{{% note title="Note on DTrace Support" %}}
Work on packaging of Riak CS for SmartOS and other operating systems with
DTrace support is ongoing with the goal of providing enhanced ability to
diagnose low-level issues in instances of Riak CS running on such operating
systems.
{{% /note %}}
