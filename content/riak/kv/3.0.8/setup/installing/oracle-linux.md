---
title_supertext: "Installing on"
title: "Oracle Linux"
description: ""
project: "riak_kv"
project_version: "3.0.8"
lastmod: 2023-12-08T00:00:00-00:00
sitemap:
  priority: 0.9
menu:
  riak_kv-3.0.8:
    name: "Oracle Linux"
    identifier: "installing_oraclelinux"
    weight: 306
    parent: "installing"
toc: true
version_history:
  in: "3.0.3+"
aliases:
  - /riak/3.0.8/ops/building/installing/Installing-on-Oracle-Linux
  - /riak/kv/3.0.8/ops/building/installing/Installing-on-Oracle-Linux
  - /riak/3.0.8/installing/Oracle-Linux/
  - /riak/kv/3.0.8/installing/Oracle-Linux/
---

[install source index]: {{<baseurl>}}riak/kv/3.0.8/setup/installing/source
[install source erlang]: {{<baseurl>}}riak/kv/3.0.8/setup/installing/source/erlang
[install verify]: {{<baseurl>}}riak/kv/3.0.8/setup/installing/verify

## Installing From Package

If you wish to install the Oracle Linux package by hand, follow these
instructions.

### For Oracle Linux 8

**Note** There are various Riak packages available for different OTP versions, please ensure that you are using the correct package for your OTP version.

Before installing Riak on Oracle Linux 8, we need to satisfy some Erlang dependencies
from EPEL first by installing the EPEL repository:

```bash
sudo yum install -y epel-release
```

Once the EPEL has been installed, you can install Riak on Oracle Linux 8 using yum, which we recommend:

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.8/oracle/8/riak-3.0.8.OTP22.3-1.el8.x86_64.rpm
sudo yum install -y riak-3.0.8.OTP22.3-1.el8.x86_64.rpm
```

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].

