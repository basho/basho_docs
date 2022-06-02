---
title: "Installing Riak TS"
description: "Installing Riak TS"
menu:
  riak_ts-1.3.1:
    name: "Install"
    identifier: "installing"
    weight: 200
    pre: install
project: "riak_ts"
project_version: "1.3.1"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing"]
    - ["1.4.0+",      "setup/installing"]
aliases:
    - /riakts/1.3.1/installing/installing/
---


[AWS]: aws/
[concept aae]: {{<baseurl>}}riak/kv/2.1.3/learn/concepts/active-anti-entropy
[Centos]: rhel-centos/
[Debian]: debian-ubuntu/
[download]: ../downloads/
[OSX]: mac-osx/
[source]: source/
[Ubuntu]: debian-ubuntu/


Riak TS is a distributed NoSQL key/value store optimized for fast reads and writes of time series data. To use it, all you need to do is [download][download] and install the Riak TS package.
 

## Installing

Choose your OS and follow the instructions:

* [CentOS/RHEL 6 & 7][Centos]
* [Debian 7 & 8][Debian]
* [OS X 10.10 & 10.11][OSX]
* [Ubuntu 12.04 & 14.04][Ubuntu]

Riak TS is also available on [AWS Marketplace][AWS].

If your OS is not in this list, you can also install [from source][source].

>**Upgrade Note:** 
>
>Riak TS is a complete, standalone product. Upgrading from Riak KV to Riak TS is not supported.
