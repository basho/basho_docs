---
title: Installing Riak TS
project: riak_ts
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[AAE]: http://docs.basho.com/riak/latest/theory/concepts/aae/
[Centos]: http
[Debian]: http
[OSX]: http
[Ubuntu]: http


Riak TS is a distributed NoSQL key/value store optimized for fast reads and writes of time series data. With Riak TS, you no longer have to build your own time series database on Riak KV.
 

## Installing

Choose your OS and follow the instructions:

* [CentOS 6 & 7][Centos]
* [Debian 6 & 7][Debian]
* [OSX 10.8][OSX]
* [Ubuntu 12.04 & 14.04)][Ubuntu]


Once you've installed Riak TS, you must turn off [AAE][AAE]. To do this, edit riak.conf as follows:

```riak.conf
anti_entropy = passive
```

>**Upgrade Note:** 
>
>Risk TS is debuting as a standalone product. Upgrading from Riak KV to Riak TS is not currently supported.
