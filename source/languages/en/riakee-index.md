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

<div class="info"><div class="title">Riak Enterprise Only</div>This documentation applies only to Riak Enterprise, Basho's commercial extension to [[Riak]]. To talk to us about using Riak Enterprise,  <a href="http://info.basho.com/Wiki_Contact.html" target="_blank">let us know</a>.</div>

Riak Enterprise is a commercially distributed product built on Riak (Apache 2.0-licensed) that extends Riak's capabilities with [[multi-datacenter replication|Multi Data Center Replication Architecture]], [[SNMP monitoring|SNMP Configuration]], [[JMX-Monitoring]], and 24x7 support. 

In multi-datacenter replication, one cluster acts as a "source cluster". The source cluster replicates its data to one or more "sink clusters" in a [[full or real-time sync|Multi Data Center Replication Architecture]] process. Data transfer is unidirectional (source -> sink). However, bidirectional synchronization can be achieved by configuring a pair of connections between clusters.

Multi-datacenter features can be used for: 

* data locality
* disaster recovery 
* global load balancing  
* active backups
* secondary analytics clusters 
* meeting regulatory requirements

{{#1.3.0-}}

If you prefer a video, check out this webcast with an overview.

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/43235103"></div>

{{/1.3.0-}}
