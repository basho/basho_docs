---
title: "Configuring Riak CS Overview"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Configuring"
    identifier: "config"
    weight: 100
    parent: "ops"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/configuration/
  - /riak/cs/latest/cookbooks/configuration/
---

In a Riak CS storage system, three components work in conjunction with one another, which means that you must configure each component to work with the others:

* Riak --- The database system that acts as the backend storage
* Riak CS --- The cloud storage layer over Riak which exposes the storage and billing APIs, storing files and metadata in Riak, and streaming them back to users
* Stanchion --- Manages requests involving globally unique system entities, such as buckets and users sent to a Riak instance, for example, to create users or to create or delete buckets

In addition, you must also configure the S3 client you use to communicate with your Riak CS system.

You should plan on having one Riak node for every Riak CS node in your system. Riak and Riak CS nodes can be run on separate physical machines, but in many cases it is preferable to run one Riak and one Riak CS node on the same physical machine. Assuming the single physical machine has sufficient capacity to meet the needs of both a Riak and a Riak CS node, you will typically see better performance due to reduced network latency.

If your system consists of several nodes, configuration primarily represents setting up the communication between components. Other settings, such as where log files are stored, are set to default values and need to be changed only if you want to use non-default values.

## Configuration of System Components

* [Configuring Riak]({{<baseurl>}}riak/cs/2.1.2/cookbooks/configuration/riak-for-cs)
* [configuring Riak CS]({{<baseurl>}}riak/cs/2.1.2/cookbooks/configuration/riak-cs)
* [Configuring Stanchion]({{<baseurl>}}riak/cs/2.1.2/cookbooks/configuration/stanchion)
* [Configuring an S3 client]({{<baseurl>}}riak/cs/2.1.2/cookbooks/configuration/s3-client)
