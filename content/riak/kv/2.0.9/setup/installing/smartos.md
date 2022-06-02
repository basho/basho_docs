---
title_supertext: "Installing on"
title: "SmartOS"
description: ""
project: "riak_kv"
project_version: "2.0.9"
menu:
  riak_kv-2.0.9:
    name: "SmartOS"
    identifier: "installing_smartos"
    weight: 305
    parent: "installing"
toc: true
aliases:
  - /riak/2.0.9/ops/building/installing/Installing-on-SmartOS
  - /riak/kv/2.0.9/ops/building/installing/Installing-on-SmartOS
  - /riak/2.0.9/installing/smartos/
  - /riak/kv/2.0.9/installing/smartos/
---

[install verify]: {{<baseurl>}}riak/kv/2.0.9/setup/installing/verify

{{% note title="SmartOS End of Life (EOL) for Riak KV 2.0.9" %}}
SmartOS is no longer supported in Riak KV 2.0.9+. If you are interested in using Riak KV on SmartOS, you can still [build from source](../source).
{{% /note %}}

The following steps have been tested to work with Riak version 1.2 on SmartOS version **joyent_20120614T184600Z**. They demonstrate installation of a Riak node on SmartOS as the root user.

## Open Files Limit

Before proceeding with installation, you should ensure that the system's open
files limit is at least 65536. Check the current limits to verify this:

```bash
ulimit -a
```

To temporarily increase this limit *for the life of your session*, use the following command:

```bash
ulimit -n 65536
```

To increase this value in a persistent manner that will be enforced after restarting the system, add the following to `/etc/system`:

```bash
set rlim_fd_max=65536
```

## Choosing a Version

SmartOS, albeit powerful, can make some easy tasks (like figuring out a "version" of SmartOS) difficult. Defining the correct version is a combination of the Global Zone snapshot version and the pkgsrc version in the guest zones. Here is the way to determine which Riak package to use.

The thing that really matters for Riak is what dataset was used to make the SmartOS VM. These datasets come from joyent and appear like this with the `dsadm` command:

```
fdea06b0-3f24-11e2-ac50-0b645575ce9d smartos 2012-12-05 sdc:sdc:base64:1.8.4
f4c23828-7981-11e1-912f-8b6d67c68076 smartos 2012-03-29 sdc:sdc:smartos64:1.6.1
```

This is where the `1.6` and `1.8` versions come from in the package naming. It isn't perfect, but if you know what dataset you used to make your SmartOS VM, you will know which package to use.

For Joyent Cloud users who don't know what dataset was used, in the guest zone type:

```
cat /opt/local/etc/pkgin/repositories.conf
```

* If this returns `http://pkgsrc.joyent.com/sdc6/2012Q2/x86_64/All` or any other *2012Q2* you need to use the `1.8` download.
* If this returns `http://pkgsrc.joyent.com/sdc6/2011Q4/x86_64/All` or any other *2011* you need to use the `1.6` download.

## Download and Install

Download your version of the Riak binary package for SmartOS:

```bash
curl -o /tmp/riak-2.0.9-SmartOS-x86_64.tgz http://s3.amazonaws.com/downloads.basho.com/riak/2.0/2.0.9/smartos/1.8/riak-2.0.9-SmartOS-x86_64.tgz
```

Next, install the package:

```
pkg_add /tmp/riak-2.0.9-SmartOS-x86_64.tgz
```

After installing the package, enable the Riak and Erlang Port Mapper Daemon (epmd) services:

```bash
svcadm -v enable -r riak
```

Finally, after enabling the services, check to see that they are online:

```
svcs -a | grep -E 'epmd|riak'
```

Output from the above command should resemble the following:

```
online    17:17:16 svc:/network/epmd:default
online    17:17:16 svc:/application/riak:default
```

Finally, and provided that the services are shown to be in an **online** state, go ahead and ping Riak:

```bash
riak ping
```

Pinging Riak will result in a `pong` response if the node is up and reachable. If the node is not up and reachable, a `Node <nodename> not responding to pings` error will result instead.

If all responses indicate that riak is up and running, then you have successfully installed and configured Riak as service on SmartOS.

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
