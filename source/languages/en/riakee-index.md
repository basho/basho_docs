---
title: Riak Enterprise
project: riakee
version: 0.10.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: []
simple: true
versions: false
---

<div class="info"><div class="title">Riak Enterprise Only</div>This documentation applies only to Riak Enterprise, Basho's commercial extension to [[Riak]]. If you'd like to talk to us about using Riak Enterprise, please <a href="http://info.basho.com/Wiki_Contact.html" target="_blank">let us know</a>.</div>

Riak Enterprise is a commercially distributed product built on Riak (Apache 2.0-licensed) that extends Riak's capabilities with [[multi-datacenter replication|Multi Data Center Replication: Architecture]], [[SNMP monitoring|SNMP Configuration]], [[JMX-Monitoring]], and 24x7 support. 

When multi-datacenter replication is enabled, one cluster acts as a "source cluster." The source cluster replicates its data to one or more "sink clusters" through a [[full or realtime sync|Multi Data Center Replication: Architecture]] process. Data transfer is unidirectional by default (source &rarr; sink). Bidirectional synchronization can be achieved, however, by configuring a pair of connections _between_ clusters.

Multi-datacenter features can serve a variety of purposes, including but not limited to the following:

* data locality 
* disaster recovery
* global load balancing  
* active backups
* secondary analytics clusters 
* meeting regulatory requirements

{{#1.3.0-}}

If you prefer a video, this webcast provides a thorough overview.

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/43235103"></div>

{{/1.3.0-}}
