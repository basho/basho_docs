---
title_supertext: "Installing on"
title: "SUSE"
description: ""
project: "riak_kv"
project_version: "2.1.4"
lastmod: 2016-04-07T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.1.4:
    name: "SUSE"
    identifier: "installing_suse"
    weight: 307
    parent: "installing"
toc: false
aliases:
  - /riak/2.1.4/ops/building/installing/Installing-on-SUSE
  - /riak/kv/2.1.4/ops/building/installing/Installing-on-SUSE
  - /riak/2.1.4/installing/suse/
  - /riak/kv/2.1.4/installing/suse/
---

[install verify]: {{<baseurl>}}riak/kv/2.1.4/setup/installing/verify

Riak KV can be installed on OpenSuse and SLES systems using a binary package. The following steps have been tested to work with Riak on
the following x86/x86_64 flavors of SuSE:

* SLES11-SP1
* SLES11-SP2
* SLES11-SP3
* SLES11-SP4
* OpenSUSE 11.2
* OpenSUSE 11.3
* OpenSUSE 11.4

## Installing with rpm

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.4/sles/11/riak-2.1.4-1.SLES11.x86_64.rpm
sudo rpm -Uvh riak-2.1.4-1.SLES11.x86_64.rpm
```

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
