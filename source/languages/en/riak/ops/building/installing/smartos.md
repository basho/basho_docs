---
title: Installing on SmartOS
project: riak
version: 1.2.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, smartos]
prev: "[[Installing on FreeBSD]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on Solaris]]"
download:
  key: smartos
  name: "SmartOS"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-SmartOS/'
}
---

The following steps have been tested to work with Riak version 1.2 on SmartOS version <strong>joyent_20120614T184600Z</strong>. They demonstrate installation of a Riak node on SmartOS as the root user.

## Open Files Limit

Before proceeding with installation, you should ensure that the system's open
files limit is at least **4096**. Check the current limits to verify this:

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

{{#1.3.0+}}

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

{{/1.3.0+}}

## Download and Install

Download your version of the Riak binary package for SmartOS{{#1.3.0}} *(below we installing with SmartOS version 1.6, for version 1.8 just replace the `1.6` in the download url with `1.8`)*{{/1.3.0}}:

{{#1.2.1-}}

```bash
curl -o /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/smartos/11/riak-{{V.V.V}}-SmartOS-i386.tgz
```

Next, install the package:

```
pkg_add /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz
```

{{/1.2.1-}}
{{#1.2.1}}

```bash
curl -o /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/smartos/11/riak-{{V.V.V}}-SmartOS-i386.tgz
```

Next, install the package:

```
pkg_add /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz
```

{{/1.2.1}}
{{#1.3.0}}

```bash
curl -o /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/smartos/1.6/riak-{{V.V.V}}-SmartOS-i386.tgz
```

Next, install the package:

```
pkg_add /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz
```

{{/1.3.0}}
{{#1.3.1+}}

```bash
curl -o /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/smartos/1.8/riak-{{V.V.V}}-SmartOS-i386.tgz
```

Next, install the package:

```
pkg_add /tmp/riak-{{V.V.V}}-SmartOS-i386.tgz
```

{{/1.3.1+}}

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

-   [[Post Installation Notes|Post Installation]]: for checking Riak health after installation
-   [[Five Minute Install]]: A  guide that will show you how to go from one
    node to bigger than Google!
