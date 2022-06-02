---
title: "Basho Data Platform"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Basho Data Platform"
    identifier: "index"
    weight: 100
    pre: riak
toc: false
aliases:
  - /dataplatform/latest/
---

[bdp install]: {{<baseurl>}}dataplatform/1.0.0/installing/
[bdp reference]: {{<baseurl>}}dataplatform/1.0.0/learn/service-manager/
[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html

Basho Data Platform (BDP) builds on Riak KV (Riak) to support your data-centric services. Ensure your application is highly available and scalable by leveraging BDP features such as:

* Data replication & synchronization between components
* Real-time analytics through Apache Spark integration
* Cluster management
* Caching with Redis for rapid performance (Enterprise only)

BDP reduces the complexity of integrating and deploying the components of your technology stack, providing Riak KV in-product, NoSQL databases, caching, real-time analytics, and search. These features are required in order to run distributed active workloads across applications; BDP controls the replication and synchronization of data between components while also providing cluster management.

> **Important**
>
> Basho Data Platform is largely open source, but some features are only available with an [Enterprise license][ee]. Those features will be marked as (Enterprise only).

## Supported Operating Systems

Basho Data Platform supports the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Amazon Linux

BDP also supports the following OSes for development:

* OSX 10.10
* Debian 7

## Learn More

BDP has a variety of features and commands available for you to use. Learn more about them [here][bdp reference].

## Get Started

Ready to get started? Learn how to [install BDP][bdp install].
