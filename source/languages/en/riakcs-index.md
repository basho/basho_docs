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
---

![Riak CS Logo](/images/riak-cs-logo.png)
<br>
<br>
Riak CS (Cloud Storage) is simple, open source storage software built on top of Riak. It can be used to build public or private clouds, or as reliable storage to power applications and services. Built on Basho's distributed database Riak, Riak CS is open source software designed to provide simple, available, distributed cloud storage at any scale. Riak CS is S3-API compatible and supports per-tenant reporting for billing and metering use cases.

## Notable Riak CS Features

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Amazon S3-API Compatibility</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p>Riak CS has a built-in S3 interface with support for S3&nbsp;ACL so you can use existing S3 tools and frameworks&nbsp;or import and extract Amazon data. The HTTP REST&nbsp;API supports service, bucket and object-level&nbsp;operations to easily store and retrieve data.&nbsp;</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Multi-Datacenter Replication</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p>Riak CS provides multi-datacenter replication for active backups, disaster recovery, and data locality. Provide low-latency storage wherever your users are and maintain availability even in the event of site failure. &nbsp;</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;"><strong>Per-Tenant Visibility</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>With the Riak CS Reporting API, you get per-tenant&nbsp;usage data and statistics on network I/O. This&nbsp;reporting functionality supports use cases including&nbsp;accounting, subscription, chargebacks, plugins with&nbsp;billing systems or efficient multi-department&nbsp;utilization.</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p><strong>Supports Objects of Arbitrary&nbsp;Content Type Up to 5GB, Plus&nbsp;Metadata</strong></p>
</td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>Store images, text, video, documents, database&nbsp;backups, software binaries and other content up to&nbsp;5GB as a single, easily retrievable object.&nbsp;&nbsp;Riak CS also&nbsp;supports standard Amazon metadata headers.</p>
</td>
</tr>
{{#1.3.0+}}
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;"><strong>Multi-Datacenter Replication</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>Riak CS provides multi-datacenter replication for active backups, disaster recovery, and data locality. Provide low-latency storage wherever your users are and maintain availability even in the event of site failure.</p>
</td>
</tr>
{{/1.3.0+}}
</tbody>
</table>


