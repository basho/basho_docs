---
title_supertext: "Installing on"
title: "Solaris"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Solaris"
    identifier: "installing_solaris"
    weight: 306
    parent: "installing"
toc: true
aliases:
  - /riak/2.1.3/ops/building/installing/Installing-on-Solaris
  - /riak/kv/2.1.3/ops/building/installing/Installing-on-Solaris
  - /riak/2.1.3/installing/solaris/
  - /riak/kv/2.1.3/installing/solaris/
---

[install verify]: {{<baseurl>}}riak/kv/2.1.3/setup/installing/verify

The following steps have been tested to work with Riak version 1.3.1 on Solaris 10 i386. They demonstrate installation of a Riak node on Solaris as the root user.

> **Note:** Before installing Riak on Solaris, be sure that you've installed `sudo` as Riak's scripts require it for proper operation.

## Open Files Limit

Before proceeding with installation, you should ensure that the system's open files limit is at least 65536 by verifying the current value of `nofiles(descriptors)`. Check the current value with the `ulimit` command:

```bash
ulimit -a
```

To temporarily increase this limit for the life of your session, use the following command:

```bash
ulimit -n 65536
```

To increase this value in a persistent manner that will be enforced after restarting the system, add the following to the `/etc/system` file:

```
set rlim_fd_max=65536
set rlim_fd_cur=65536
```

Note that you must restart to have the above settings take effect.

## Download and Install

Download your version of the Riak binary package for Solaris 10:

```bash
curl -o /tmp/BASHOriak-2.1.3-Solaris10-i386.pkg.gz http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.3/solaris/10/BASHOriak-2.1.3-Solaris10-x86_64.pkg.gz
```

Next, install the package:

```bash
gunzip /tmp/BASHOriak-2.1.3-Solaris10-i386.pkg.gz
pkgadd /tmp/BASHOriak-2.1.3-Solaris10-i386.pkg
```

After installing the package, be sure to include `/opt/riak/bin` in the
appropriate user's `PATH`. After doing so, you can then start Riak:

```bash
riak start
```

Finally, go ahead and ping Riak to ensure it is running:

```bash
riak ping
```

Pinging Riak will result in a `pong` response if the node is up and reachable. If the node is not up and reachable, a `Node <nodename> not responding to pings` error will result instead.

If all responses indicate that riak is up and running, then you have successfully installed Riak on Solaris 10.

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
