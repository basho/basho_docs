---
title: Installing on Debian and Ubuntu
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, debian, ubuntu, linux]
prev: ["Installing Erlang", "Installing-Erlang.html"]
up:   ["Installing and Upgrading", "index.html"]
next: ["Installing on RHEL and CentOS", "Installing-on-RHEL-and-CentOS.html"]
---

<div class="info">
The following steps have been tested to work with Riak on Debian version 6.05 and Ubuntu version 12.04.
</div>

Riak can be installed on Debian or Ubuntu based systems with a binary package or by [[compiling Riak from source code|Installing Riak from Source]].

Installing From Package
-----------------------

### SSL Library Requirement for Ubuntu

Riak currently requires libssl version 0.9.8, which is not installed by
default on recent versions of Ubuntu. Before installing Riak via package
on Ubuntu, install the `libssl0.9.8` package. Note that this
version of libssl can be safely installed alongside current/existing
libssl installations.

To install the libssl version 0.9.8 package, execute the following
command:

```bash
    $ sudo apt-get install libssl0.9.8
```

After the libssl package installation, proceed to installing Riak from
the pre-built package by executing the following commands as appropriate
for the target platform:

### Riak 64-bit Installation

#### Ubuntu Lucid

```bash
    $ wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/ubuntu/lucid/riak_1.2.1-1_amd64.deb
    $ sudo dpkg -i riak_1.2.1-1_amd64.deb
```

#### Ubuntu Natty  

```bash
    $ wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/ubuntu/natty/riak_1.2.1-1_amd64.deb
    $ sudo dpkg -i riak_1.2.1-1_amd64.deb
```

#### Ubuntu Precise

```bash
    $ wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/ubuntu/precise/riak_1.2.1-1_amd64.deb
    $ sudo dpkg -i riak_1.2.1-1_amd64.deb
```

### Riak 32-bit Installation

```bash
    $ wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/ubuntu/lucid/riak_1.2.1-1_i386.deb
    $ sudo dpkg -i riak_1.2.1-1_i386.deb
```
<div class="note"><div class="title">Upgrading Riak</div>If upgrading the Riak package, and the user named "riak" exists without a home directory, create a home directory (`/var/lib/riak`), and execute `chown riak:riak /var/lib/riak` before starting Riak.</div>


Installing Riak From Source
---------------------------

First, install Riak dependencies using apt:

```bash
    $ sudo apt-get install build-essential libc6-dev-i386 git
```

Riak requires [Erlang](http://www.erlang.org/) R15B01 or later. If
Erlang is not already installed, install it before continuing (see:
[[Installing Erlang]] for more information).

With Erlang installed, proceed to downloading and installing Riak:

```bash
    $ wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/riak-1.2.1.tar.gz
    $ tar zxvf riak-1.2.1.tar.gz
    $ cd riak-1.2.1
    $ make rel
```

If the build was successful, a fresh build of Riak will exist in the
`rel/riak` directory.

Next Steps?
-----------

Now that Riak is installed, check out the following resources:

-   [[The Riak Fast Track]]: a
    guide for setting up a 3 node cluster and exploring Riak's main features.
-   [[Basic Cluster Setup]]:
    a guide that will show you how to go from one node to bigger than
    Google!
