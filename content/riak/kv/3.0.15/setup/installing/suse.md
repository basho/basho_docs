---
title_supertext: "Installing on"
title: "SUSE"
description: ""
project: "riak_kv"
project_version: "3.0.15"
lastmod: 2023-02-15T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.15:
    name: "SUSE"
    identifier: "installing_suse"
    weight: 310
    parent: "installing"
toc: false
aliases:
  - /riak/3.0.15/ops/building/installing/Installing-on-SUSE
  - /riak/kv/3.0.15/ops/building/installing/Installing-on-SUSE
  - /riak/3.0.15/installing/suse/
  - /riak/kv/3.0.15/installing/suse/
---

[install verify]: {{<baseurl>}}riak/kv/3.0.15/setup/installing/verify

{{% note title="SUSE End of Life (EOL) for Riak KV 2.2.3" %}}
SUSE is no longer supported in Riak KV 2.2.3+. If you are interested in using Riak KV on SUSE, you can still [build from source](../source). The steps below have been left here for reference only and are no longer maintained.
{{% /note %}}

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
wget https://files.tiot.jp/riak/kv/2.2/2.2.3/sles/11/riak-2.2.3-1.SLES11.x86_64.rpm
sudo rpm -Uvh riak-2.2.3-1.SLES11.x86_64.rpm
```

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
