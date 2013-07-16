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
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-Debian-and-Ubuntu'
}
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
sudo bash -c "echo deb http://apt.basho.com $(lsb_release -sc) main > /etc/apt/sources.list.d/basho.list"
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

### Installing on Non-LTS Ubuntu Releases

Typically we only package Riak for LTS releases to keep our build and
testing matrix focused.  In some cases such as Ubuntu 11.04 (Natty),
there are changes that affect how Riak is packaged so we will release a
separate package for that non-LTS release.  In most other cases
though, if you are running a non-LTS release (such as 12.10) it is
safe to follow the below instructions for the LTS release prior to
your release.  In the case of Ubuntu 12.10, follow the installation
instructions for Ubuntu 12.04.

### SSL Library Requirement for Ubuntu

Riak currently requires libssl version 0.9.8 on some versions of
Ubuntu. Starting at Ubuntu 12.04 this is no longer an issue. Before
installing Riak via package on Ubuntu, install the `libssl0.9.8`
package. Note that this version of libssl can be safely installed
alongside current/existing libssl installations.

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

{{#1.2.0-}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak_{{V.V.V}}-1_amd64.deb
sudo dpkg -i riak_{{V.V.V}}-1_amd64.deb
```

{{/1.2.0-}}
{{#1.2.0+}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/ubuntu/lucid/riak_{{V.V.V}}-1_amd64.deb
sudo dpkg -i riak_{{V.V.V}}-1_amd64.deb
```

{{/1.2.0+}}

#### Ubuntu Natty Narwhal (11.04)

{{#1.2.0-}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak_{{V.V.V}}-1_amd64.deb
sudo dpkg -i riak_{{V.V.V}}-1_amd64.deb
```

{{/1.2.0-}}
{{#1.2.0+}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/ubuntu/natty/riak_{{V.V.V}}-1_amd64.deb
sudo dpkg -i riak_{{V.V.V}}-1_amd64.deb
```

{{/1.2.0+}}


#### Ubuntu Precise Pangolin (12.04)

{{#1.2.0-}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak_{{V.V.V}}-1_amd64.deb
sudo dpkg -i riak_{{V.V.V}}-1_amd64.deb
```

{{/1.2.0-}}
{{#1.2.0+}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/ubuntu/precise/riak_{{V.V.V}}-1_amd64.deb
sudo dpkg -i riak_{{V.V.V}}-1_amd64.deb
```

{{/1.2.0+}}


{{#1.2.1-1.3.9}}

### Riak 32-bit Installation

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/ubuntu/lucid/riak_{{V.V.V}}-1_i386.deb
sudo dpkg -i riak_{{V.V.V}}-1_i386.deb
```

<div class="note"><div class="title">Upgrading Riak</div>If upgrading the Riak package, and the user named "riak" exists without a home directory, create a home directory (`/var/lib/riak`), and execute `chown riak:riak /var/lib/riak` before starting Riak.</div>

{{/1.2.1-1.3.9}}

Installing Riak From Source
---------------------------

First, install Riak dependencies using apt:

```bash
sudo apt-get install build-essential libc6-dev-i386 git
```

Riak requires [Erlang](http://www.erlang.org/) R15B01. *Note: don't use Erlang version R15B02 or R15B03, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.
If Erlang is not already installed, install it before continuing (see:
[[Installing Erlang]] for more information).

With Erlang installed, proceed to downloading and installing Riak:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak-{{V.V.V}}.tar.gz
tar zxvf riak-{{V.V.V}}.tar.gz
cd riak-{{V.V.V}}
make rel
```

If the build was successful, a fresh build of Riak will exist in the
`rel/riak` directory.

Next Steps?
-----------

Now that Riak is installed, check out the following resources:

-   [[Post Installation Notes|Post Installation]]: for checking Riak health after installation
-   [[Five Minute Install]]:
    a guide that will show you how to go from one node to bigger than
    Google!
