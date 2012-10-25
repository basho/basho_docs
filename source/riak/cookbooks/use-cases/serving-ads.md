---
title: Serving Advertisements
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

## Simple Case

A common use case for Riak is using it as a data store for serving tons of ad content to many different web and mobile users with low latency. In Riak, advertising content - images or text - can be stored with unique keys (perhaps something relevant to the campaign), or with Riak-generated keys. 

## Complex Case

In the advertising industry, being able to serve ads quickly to many users and platforms is often the most important factor in selecting and tuning a database. Riak's tunable CAP controls can be set to favor fast read performance. By setting the r value to 1, only one of n replicas will need to be returned to complete a read operation, yielding lower read latency than a r value equal to the number of replicas. This is ideal for advertising traffic which is primarily serving reads.
