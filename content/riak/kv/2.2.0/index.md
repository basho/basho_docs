---
title: "Riak KV 2.2.0"
description: ""
project: "riak_kv"
project_version: "2.2.0"
lastmod: 2016-11-12T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.2.0:
    name: "Riak KV"
    identifier: "index"
    weight: 100
    pre: riak
toc: false
aliases:
  - /riak/2.2.0/
---

[aboutenterprise]: http://basho.com/contact/
[config index]: {{<baseurl>}}riak/kv/2.2.0/configuring
[downloads]: {{<baseurl>}}riak/kv/2.2.0/downloads/
[install index]: {{<baseurl>}}riak/kv/2.2.0/setup/installing/
[plan index]: {{<baseurl>}}riak/kv/2.2.0/setup/planning
[perf open files]: {{<baseurl>}}riak/kv/2.2.0/using/performance/open-files-limit
[install debian & ubuntu]: {{<baseurl>}}riak/kv/2.2.0/setup/installing/debian-ubuntu
[usage search]: {{<baseurl>}}riak/kv/2.2.0/developing/usage/search
[getting started]: {{<baseurl>}}riak/kv/2.2.0/developing/getting-started
[dev client libraries]: {{<baseurl>}}riak/kv/2.2.0/developing/client-libraries

Riak KV is a distributed NoSQL database designed to deliver maximum data availability by distributing data across multiple servers. As long as your Riak KV client can reach one Riak server, it should be able to write data.

[Riak KV Enterprise][aboutenterprise] includes multi-datacenter cluster replication, which ensures low-latency and robust business continuity.

## Supported Operating Systems

- Amazon Linux 2016.09 (AWS)
- CentOS 6
- CentOS 7
- Debian 7.0 ("Wheezy")
- Debian 8.0 ("Jessie")
- Red Hat Enterprise Linux 6
- Red Hat Enterprise Linux 7
- Solaris 10
- SUSE Linux Enterprise Server 11
- Ubuntu 12.04 ("Precise Pangolin")
- Ubuntu 14.04 ("Trusty Tahr")
- Ubuntu 16.04 ("Xenial Xerus")
- Mac OSX 10.8+ (development only)

## Getting Started

Are you brand new to Riak KV? Start by [downloading][downloads] Riak KV, and then follow the below pages to get started:

1. [Install Riak KV][install index]
2. [Plan your Riak KV setup][plan index]
3. [Configure Riak KV for your needs][config index]

{{% note title="Developing with Riak KV" %}}
If you are looking to integrate Riak KV with your existing tools, check out the [Developing with Riak KV]({{<baseurl>}}riak/kv/2.2.0/developing) docs. They provide instructions and examples for languages such as: Java, Ruby, Python, Go, Haskell, NodeJS, Erlang, and more.
{{% /note %}}

## Popular Docs

1. [Open Files Limit][perf open files]
2. [Installing on Debian-Ubuntu][install debian & ubuntu]
3. [Developing with Riak KV: Searching][usage search]
4. [Developing with Riak KV: Getting Started][getting started]
5. [Developing with Riak KV: Client Libraries][dev client libraries]

## Academy Training

[Basho's Academy](https://academy.basho.com) courses offer hands-on experience with Riak KV. Sign up for a [free account](https://academy.basho.com/users/sign_up) to get started.
