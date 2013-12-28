---
title: Riak CS
project: riakcs
version: 0.10.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: []
simple: true
versions: false
body_id: riakcs-index
---

![Riak CS Logo](/images/riak-cs-logo.png)


Riak CS (Cloud Storage) is easy-to-use object storage software built on top of [Riak](http://basho.com/riak/), Basho's distributed database. Riak CS is designed to provide simple, available, distributed cloud storage at any scale, and can be used to build cloud architectures&mdash;be they public or private&mdash;or as storage infrastructure for heavy-duty applications and services. Riak CS's API is [Amazon S3 compatible](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html) and supports per-tenant reporting for use cases involving billing and metering.

Riak CS is open source and [[free for download|Download Riak CS]].

## Notable Riak CS Features

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Amazon S3-API Compatibility</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p>Riak CS has a built-in S3 interface with S3 Access Control List <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/ACLOverview.html">ACL</a> support, which means that you can both use existing S3 tools and frameworks to manage your data and also import and extract data from Amazon directly. The HTTP REST API supports service, bucket, and object-level operations to easily store and retrieve data.</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;"><strong>Per-Tenant Visibility</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>With the Riak CS [[Reporting API|Monitoring and Metrics]], you can access per-tenant usage data and statistics over network I/O. This reporting functionality supports use cases including accounting, subscription, chargebacks, plugins with billing systems, efficient multi-department utilization, and much more.</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<strong>Supports Large Objects of Arbitrary Content Type, Plus Metadata</strong>
</td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>Riak CS enables you to store any conceivable data type, such as images, text, video, documents, database backups, or software binaries. Riak CS can store objects into the terabyte size range using multipart file uploads. Riak CS also supports standard Amazon <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html">metadata headers</a>.</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;"><strong>Multi-Datacenter Replication<br><i>(Enterprise Edition Only)</i></strong>{{1.3.0+}}</td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>Riak CS [[Enterprise edition|Riak Enterprise]] provides Multi-Datacenter Replication for active backups, disaster recovery, and data locality. Provide low-latency storage wherever your users are and maintain availability even in the event of site failure.</p>
</td>
</tr>
</tbody>
</table>