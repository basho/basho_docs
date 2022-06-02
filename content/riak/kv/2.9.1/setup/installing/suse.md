---
title_supertext: "Installing on"
title: "SUSE"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "SUSE"
    identifier: "installing_suse"
    weight: 307
    parent: "installing"
toc: false
aliases:
  - /riak/2.9.1/ops/building/installing/Installing-on-SUSE
  - /riak/kv/2.9.1/ops/building/installing/Installing-on-SUSE
  - /riak/2.9.1/installing/suse/
  - /riak/kv/2.9.1/installing/suse/
---

[install verify]: {{<baseurl>}}riak/kv/2.9.1/setup/installing/verify

{{% note title="SUSE End of Life (EOL) for Riak KV 2.2.3" %}}
SUSE is no longer supported in Riak KV 2.9.1+. If you are interested in using Riak KV on SUSE, you can still [build from source](../source). The steps below have been left here for reference only and are no longer maintained.
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
