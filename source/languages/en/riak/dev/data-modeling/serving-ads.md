---
title: Serving Advertisements
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
moved: {
  '1.4.0-': '/cookbooks/use-cases/serving-ads'
}
---

## Simple Case

A common use case for Riak is using it as a data store for serving ad content to many different web and mobile users with low latency. In Riak, advertising content - images or text - can be stored with unique keys, or with Riak-generated keys. Users will often make keys based on a campaign ID for easy retrieval.

## Complex Case

In the advertising industry, being able to serve ads quickly to many users and platforms is often the most important factor in selecting and tuning a database. Riak's tunable CAP controls can be set to favor fast read performance. By setting the r value to 1, only one of n replicas will need to be returned to complete a read operation, yielding lower read latency than a r value equal to the number of replicas. This is ideal for advertising traffic which is primarily serving reads.

## Community Examples

<table class="links">
  <tr>
    <td><a href="http://player.vimeo.com/video/49775483" target="_blank" title="Riak at OpenX"><img src="http://b.vimeocdn.com/ts/343/417/343417336_960.jpg"/></a>
    </td>
    <td><a href="http://player.vimeo.com/video/49775483" target="_blank" title="Riak at OpenX">Riak at OpenX</a>
    <br>
    Los Angeles-based OpenX will serve around four trillion ads this year. In this talk, Anthony Molinaro, Engineer at OpenX, goes in-depth on their architecture, how they've built their system, and why/how they are switching to Riak and Riak Core for data storage after using databases like CouchDB and Cassandra in production.
    </td>
  </tr>
</table>
