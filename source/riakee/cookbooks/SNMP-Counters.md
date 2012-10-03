---
title: SNMP Counters
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [snmp]
---

<div class="info"><div class="title">Riak Enterprise Only</div>This documentation applies only to Riak Enterprise, Basho's commercial extension to <a href="http://wiki.basho.com/Riak.html">Riak</a>. To learn more about the differences between Riak and Riak Enterprise, <a href="http://basho.com/products/riak-overview/">read here</a>.  To talk to us about using Riak Enterprise,  <a href="http://info.basho.com/Wiki_Contact.html" target="_blank">let us know</a>.</div>

**vnodeGets**  
*Type:* Counter  
Number of vnode-level GETs in past minute

**vnodePuts**  
*Type:* Counter  
Number of vnode-level PUTs in past minute

**nodeGets**  
*Type:* Counter  
Number of GETs in past minute

**nodePuts**  
*Type:* Counter  
Number of PUTs in past minute

**nodeGetTimeMean**  
*Type:* Gauge  
Mean GET time (microseconds)

**nodeGetTimeMedian**  
*Type:* Gauge  
Median GET time (microseconds)

**nodeGetTime95**  
*Type:* Gauge  
95th percentile GET time (microseconds)

**nodeGetTime99**  
*Type:* Gauge  
99th percentile GET time (microseconds)

**nodeGetTime100**  
*Type:* Gauge  
Maximum GET time (microseconds)

**nodePutTime95**  
*Type:* Gauge  
95th percentile PUT time (microseconds)

**nodePutTime99**  
*Type:* Gauge  
99th percentile PUT time (microseconds)

**nodePutTime100**  
*Type:* Gauge  
Maximum PUT time (microseconds)

**nodePutTimeMean**  
*Type:* Gauge  
Mean PUT time (microseconds)

**nodePutTimeMedian**  
*Type:* Gauge  
Median PUT time (microseconds)
