---
title: Installing on Debian and Ubuntu
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, debian, ubuntu, linux]
prev: "[[Installing Erlang]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on RHEL and CentOS]]"
download: 
  key: debian
  name: "Debian or Ubuntu"
---

Riak can be installed on Debian or Ubuntu based systems with a binary package or by [[compiling Riak from source code|Installing Riak from Source]]. The following steps have been tested to work with Riak on **Debian version 6.05** and **Ubuntu version 12.04**.

Installing From Apt-Get
-----------------------

If you wish to just install Riak and get on with your life, use `apt-get`.

First you must get the signing key.

```bash
curl http://apt.basho.com/gpg/basho.apt.key | sudo apt-key add -
```

Then add the Basho repository to your apt sources list (and update them).

```
sudo bash -c "echo deb http://apt.basho.com `lsb_release -sc` main > /etc/apt/sources.list.d/basho.list"
sudo apt-get update
```

Now install riak.

```bash
sudo apt-get install riak
```

That should be all.

Installing From Package
-----------------------

If you wish to install the deb packages by hand follow these instructions.

### SSL Library Requirement for Ubuntu

Riak currently requires libssl version 0.9.8, which is not installed by
default on recent versions of Ubuntu. Before installing Riak via package
on Ubuntu, install the `libssl0.9.8` package. Note that this
version of libssl can be safely installed alongside current/existing
libssl installations.

To install the libssl version 0.9.8 package, execute the following
command:

```bash
sudo apt-get install libssl0.9.8
```

After the libssl package installation, proceed to installing Riak from
the pre-built package by executing the following commands as appropriate
for the target platform:

### Riak 64-bit Installation

#### Ubuntu Lucid Lynx (10.04)

{{#1.0.3}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak_1.0.3-1_amd64.deb
sudo dpkg -i riak_1.0.3-1_amd64.deb
```

{{/1.0.3}}
{{#1.1.4}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak_1.1.4-1_amd64.deb
sudo dpkg -i riak_1.1.4-1_amd64.deb
```

{{/1.1.4}}
{{#1.2.0}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/lucid/riak_1.2.0-1_amd64.deb
sudo dpkg -i riak_1.2.1-0_amd64.deb
```

{{/1.2.0}}
{{#1.2.1}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/lucid/riak_1.2.1-1_amd64.deb
sudo dpkg -i riak_1.2.1-1_amd64.deb
```

{{/1.2.1}}

#### Ubuntu Natty Narwhal (11.04)

{{#1.0.3}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak_1.0.3-1_amd64.deb
sudo dpkg -i riak_1.0.3-1_amd64.deb
```

{{/1.0.3}}
{{#1.1.4}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak_1.1.4-1_amd64.deb
sudo dpkg -i riak_1.1.4-1_amd64.deb
```

{{/1.1.4}}
{{#1.2.0}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/ubuntu/natty/riak_1.2.0-1_amd64.deb
sudo dpkg -i riak_1.2.0-1_amd64.deb
```

{{/1.2.0}}
{{#1.2.1}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/natty/riak_1.2.1-1_amd64.deb
sudo dpkg -i riak_1.2.1-1_amd64.deb
```

{{/1.2.1}}

#### Ubuntu Precise Pangolin (12.04)

{{#1.0.3}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak_1.0.3-1_amd64.deb
sudo dpkg -i riak_1.0.3-1_amd64.deb
```

{{/1.0.3}}
{{#1.1.4}}

```bash
http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak_1.1.4-1_amd64.deb
sudo dpkg -i riak_1.1.4-1_amd64.deb
```

{{/1.1.4}}
{{#1.2.0}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/ubuntu/precise/riak_1.2.0-1_amd64.deb
sudo dpkg -i riak_1.2.0-1_amd64.deb
```

{{/1.2.0}}
{{#1.2.1}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/precise/riak_1.2.1-1_amd64.deb
sudo dpkg -i riak_1.2.1-1_amd64.deb
```

{{/1.2.1}}

### Riak 32-bit Installation

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/lucid/riak_1.2.1-1_i386.deb
sudo dpkg -i riak_1.2.1-1_i386.deb
```
<div class="note"><div class="title">Upgrading Riak</div>If upgrading the Riak package, and the user named "riak" exists without a home directory, create a home directory (`/var/lib/riak`), and execute `chown riak:riak /var/lib/riak` before starting Riak.</div>


Installing Riak From Source
---------------------------

First, install Riak dependencies using apt:

```bash
sudo apt-get install build-essential libc6-dev-i386 git
```

Riak requires [Erlang](http://www.erlang.org/) R15B01. *Note: don't use Erlang version R15B02, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.
If Erlang is not already installed, install it before continuing (see:
[[Installing Erlang]] for more information).

With Erlang installed, proceed to downloading and installing Riak:

{{#1.0.3}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak-1.0.3.tar.gz
tar zxvf riak-1.0.3.tar.gz
cd riak-1.0.3
make rel
```

{{/1.0.3}}
{{#1.1.4}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak-1.1.4.tar.gz
tar zxvf riak-1.1.4.tar.gz
cd riak-1.1.4
make rel
```

{{/1.1.4}}
{{#1.2.0}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/riak-1.2.0.tar.gz
tar zxvf riak-1.2.0.tar.gz
cd riak-1.2.0
make rel
```

{{/1.2.0}}
{{#1.2.1}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```

{{/1.2.1}}

If the build was successful, a fresh build of Riak will exist in the
`rel/riak` directory.

Next Steps?
-----------

Now that Riak is installed, check out the following resources:

-   [[Post Installation Notes|Post Installation]]: for checking Riak health after installation
-   [[The Riak Fast Track]]: a
    guide for setting up a 3 node cluster and exploring Riak's main features.
-   [[Basic Cluster Setup]]:
    a guide that will show you how to go from one node to bigger than
    Google!
