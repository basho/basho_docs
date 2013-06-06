---
title: Installing on Solaris 10
project: riak
version: 1.2.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, solaris]
prev: "[[Installing on SmartOS]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on SUSE]]"
download: 
  key: solaris
  name: "Solaris"
---

The following steps have been tested to work with Riak version 1.3.1 on Solaris 10 i386. They demonstrate installation of a Riak node on Solaris as the root user.

<div class="note">Before installing Riak on Solaris, be sure that you've
installed <code>sudo</code> as Riak's scripts require it for proper operation.</div>

## Open Files Limit

Before proceeding with installation, you should ensure that the system's open files limit is at least **4096** by verifying the current value of *nofiles(descriptors)*. Check the current value with the `ulimit` command:

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

{{#1.2.1-}}

```bash
curl -o /tmp/BASHOriak-1.2.0-1-Solaris10-i386.pkg.gz http://s3.amazonaws.com/downloads.basho.com/riak/1.2/1.2.0/solaris/10/BASHOriak-1.2.0-1-Solaris10-i386.pkg.gz
```

Next, install the package:

```bash
gunzip /tmp/BASHOriak-1.2.0-1-Solaris10-i386.pkg.gz
pkgadd /tmp/BASHOriak-1.2.0-1-Solaris10-i386.pkg
```

{{/1.2.1-}}
{{#1.2.1}}

```bash
curl -o /tmp/BASHOriak-1.2.1-1-Solaris10-i386.pkg.gz http://s3.amazonaws.com/downloads.basho.com/riak/1.2/1.2.1/solaris/10/BASHOriak-1.2.1-1-Solaris10-i386.pkg.gz
```

Next, install the package:

```bash
gunzip /tmp/BASHOriak-1.2.1-1-Solaris10-i386.pkg.gz
pkgadd -d BASHOriak-1.2.1-1-Solaris10-i386.pkg
```

{{/1.2.1}}
{{#1.3.0}}

```bash
curl -o /tmp/BASHOriak-1.3.0-1-Solaris10-i386.pkg.gz http://s3.amazonaws.com/downloads.basho.com/riak/1.3/1.3.0/solaris/10/BASHOriak-1.3.0-1-Solaris10-i386.pkg.gz
```

Next, install the package:

```bash
gunzip /tmp/BASHOriak-1.3.0-1-Solaris10-i386.pkg.gz
pkgadd -d /tmp/BASHOriak-1.3.0-1-Solaris10-i386.pkg
```

{{/1.3.0}}
{{#1.3.1}}

```bash
curl -o /tmp/BASHOriak-1.3.1-1-Solaris10-i386.pkg.gz http://s3.amazonaws.com/downloads.basho.com/riak/1.3/1.3.1/solaris/10/BASHOriak-1.3.1-1-Solaris10-i386.pkg.gz
```

Next, install the package:

```bash
gunzip /tmp/BASHOriak-1.3.1-1-Solaris10-i386.pkg.gz
pkgadd -d /tmp/BASHOriak-1.3.1-1-Solaris10-i386.pkg
```

{{/1.3.1}}

After installing the package, be sure to include `/opt/riak/bin` in the
appropriate user's PATH. After doing so, you can then start Riak:

```bash
riak start
```


Finally, go ahead and ping Riak to ensure it is running:

```bash
riak ping
```

Pinging Riak will result in a `pong` response if the node is up and reachable, and a `pang` response if the node is up, but has a problem. If the node is not up and reachable, a *not responding to pings* error will result instead.

If all responses indicate that riak is up and running, then you have successfully installed Riak on Solaris 10.

## Next Steps?

Now that Riak is installed, check out the following resources:

-   [[Post Installation Notes|Post Installation]]: for checking Riak health after installation
-   [[Five Minute Install]]: A  guide that will show you how to go from one
    node to bigger than Google!
