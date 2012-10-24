---
title: Installing on SmartOS
project: riak
version: 1.1.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, smartos]
prev: ["Installing on FreeBSD", "Installing-on-FreeBSD.html"]
up:   ["Installing and Upgrading", "index.html"]
next: ["Installing on SUSE", "Installing-on-SUSE.html"]
---

<div class="info"><div class="title">Version Information</div>The following steps have been tested to work with Riak version 1.2 on SmartOS version <strong>joyent_20120614T184600Z</strong>.</div>

As of version 1.2, Riak can be installed on SmartOS using Basho [packages](http://basho.com/resources/downloads/). The following steps demonstrate installation of a Riak node on SmartOS as the root user.

## Open Files Limit
Before proceeding with installation, you should ensure that the system's open files limit meets or exceeds the recommended minimum of **4096**. Check the current limits to verify this:

```bash
ulimit -a
```

To temporarily increase this limit *for the life of your session*, use the following command:

```bash
ulimit -n 65536
```

To increase this value in a persistent manner that will be enforced after restarting the system, add the following to `/etc/system`:

```text
set rlim_fd_max=65536
```

## Download and Install
First, download the latest version of the Riak binary package for SmartOS; for this example, we're downloading Riak 1.2:

```bash
curl http://s3.amazonaws.com/downloads.basho.com/riak/1.2/1.2.1/smartos/11/riak-1.2.1-SmartOS-i386.tgz
```

Next, install the package:

```
pkg_add riak-1.2.1-SmartOS-i386.tgz
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

```text
online    17:17:16 svc:/network/epmd:default
online    17:17:16 svc:/application/riak:default
```

Finally, and provided that the services are shown to be in an **online** state, go ahead and ping Riak:

```bash
riak ping
```

Pinging Riak will result in a `pong` response if the node is up and reachable, and a `pang` response if the node is up, but has a problem. If the node is not up and reachable, a *not responding to pings* error will result instead.

If all responses indicate that riak is up and running, then you have successfully installed and configured Riak as service on SmartOS.

Next Steps?
-----------

Now that Riak is installed, check out the following resources:

-   [[The Riak Fast Track]]: A
    guide for setting up a 4 node cluster and exploring Riak's main
    features.
-   [[Basic Cluster Setup]]: A  guide that will show you how to go from one
    node to bigger than Google!
