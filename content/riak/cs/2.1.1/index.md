---
title: "Riak Cloud Storage"
description: ""
menu:
  riak_cs-2.1.1:
    name: "Riak CS"
    identifier: "index"
    weight: 100
    pre: bolt
project: "riak_cs"
project_version: "2.1.1"
aliases:
  - /riakcs/2.1.1/
---

Riak CS (Cloud Storage) is easy-to-use object storage software built on top of
[Riak KV](http://basho.com/riak/), Basho's distributed database. Riak CS is
designed to provide simple, available, distributed cloud storage at any scale,
and can be used to build cloud architectures---be they public or private---or
as storage infrastructure for heavy-duty applications and services. Riak CS's
API is [Amazon S3 compatible](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
and supports per-tenant reporting for use cases involving billing
and metering.

Riak CS is open source and [free for download](/riak/cs/2.1.1/downloads).

## Notable Riak CS Features

### Amazon S3-API Compatibility

Riak CS has a built-in S3 interface with S3 Access Control List ([ACL](http://docs.aws.amazon.com/AmazonS3/latest/dev/ACLOverview.html)) support, which means that you can both use existing S3 tools and frameworks to manage your data and also import and extract data from Amazon directly. The HTTP REST API supports service, bucket, and object-level operations to easily store and retrieve data. There is also support for the [OpenStack Swift API](/riak/cs/2.1.1/references/appendices/comparisons/swift/)

### Per-Tenant Visibility

With the Riak CS [Reporting API](/riak/cs/2.1.1/cookbooks/monitoring-and-metrics), you can access per-tenant usage data and statistics over network I/O. This reporting functionality supports use cases including accounting,
subscription, chargebacks, plugins with billing systems, efficient multi-department utilization, and much more.

### Supports Large Objects of Arbitrary Content Type, Plus Metadata

Riak CS enables you to store any conceivable data type, such as
images, text, video, documents, database backups, or software binaries.
Riak CS can store objects into the terabyte size range using multipart
file uploads. Riak CS also supports standard Amazon [metadata headers](http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html).

### Multi-Datacenter Replication (Enterprise Edition Only)

Riak CS [Enterprise](http://basho.com/riak-enterprise) Multi-Datacenter Replication for active backups, disaster recovery, and data locality. Provide low-latency storage wherever your users are and maintain availability even in the event of site failure.
