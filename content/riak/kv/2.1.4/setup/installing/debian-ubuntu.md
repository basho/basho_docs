---
title_supertext: "Installing on"
title: "Debian and Ubuntu"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Debian & Ubuntu"
    identifier: "installing_debian_ubuntu"
    weight: 302
    parent: "installing"
toc: true
aliases:
  - /riak/2.1.4/ops/building/installing/Installing-on-Debian-and-Ubuntu
  - /riak/kv/2.1.4/ops/building/installing/Installing-on-Debian-and-Ubuntu
  - /riak/2.1.4/installing/debian-ubuntu/
  - /riak/kv/2.1.4/installing/debian-ubuntu/
---

[install source index]: {{<baseurl>}}riak/kv/2.1.4/setup/installing/source/
[security index]: {{<baseurl>}}riak/kv/2.1.4/using/security/
[install source erlang]: {{<baseurl>}}riak/kv/2.1.4/setup/installing/source/erlang
[install verify]: {{<baseurl>}}riak/kv/2.1.4/setup/installing/verify

Riak KV can be installed on Debian or Ubuntu-based systems using a binary
package or by compiling from source code.

The following steps have been tested to work with Riak KV on
Debian versions 6.05 and 7.6 and Ubuntu version 14.04.

For versions of Riak prior to 2.0, Basho used a self-hosted
[apt](http://en.wikipedia.org/wiki/Advanced_Packaging_Tool) repository
for Debian and Ubuntu packages. For versions 2.0 and later, Basho has
moved those packages to the
[packagecloud.io](https://packagecloud.io/basho/riak?filter=debs)
hosting service. Instructions for installing via shell scripts, manual
installation, Chef, and Puppet can be found in packagecloud's
[installation docs](https://packagecloud.io/basho/riak/install).

Platform-specific pages are linked below:

* [Lucid](https://packagecloud.io/basho/riak/packages/ubuntu/lucid/riak_2.1.4-1_amd64.deb)
* [Precise](https://packagecloud.io/basho/riak/packages/ubuntu/precise/riak_2.1.4-2_amd64.deb)
* [Squeeze](https://packagecloud.io/basho/riak/packages/debian/squeeze/riak_2.1.4-1_amd64.deb)
* [Trusty](https://packagecloud.io/basho/riak/packages/ubuntu/trusty/riak_2.1.4-1_amd64.deb)
* [Wheezy](https://packagecloud.io/basho/riak/packages/debian/wheezy/riak_2.1.4-1_amd64.deb)

Our documentation also includes instructions regarding signing keys and
sources lists, which can be found in the section below.

## Installing with apt and Packagecloud

> **Note on Debian 7**
>
> If you wish to install Riak on Debian 7, you may need to install
[libc6](https://packages.debian.org/search?keywords=libc6) version 2.15 or
later, which in turn requires upgrading your system to
[sid](https://www.debian.org/releases/sid/). Installation instructions
can be found
[here](https://wiki.debian.org/DebianUnstable#How_do_I_install_Sid.3F).
>
> Once sid has been installed, you can install libc6 with the following
command:
>
>```bash
apt-get -t sid install libc6 libc6-dev libc6-dbg
```

For the simplest installation process on LTS (Long-Term Support)
releases, use `apt-get`. First, you must retrieve the signing key:

```curl
curl https://packagecloud.io/gpg.key | sudo apt-key add -
```

Second, you must install the `apt-transport-https` package in order to
be able to fetch packages over HTTPS:

```bash
sudo apt-get install -y apt-transport-https
```

Next download & install Riak KV:

```bash
curl -s https://packagecloud.io/install/repositories/basho/riak/script.deb.sh | sudo bash
sudo apt-get install riak=2.1.4-1
```

## Installing From Package

If you wish to install the deb packages by hand, follow these
instructions.

### Installing on Non-LTS Ubuntu Releases

Typically we only package Riak for LTS releases to keep our build and
testing matrix focused.  In some cases, such as Ubuntu 11.04 (Natty),
there are changes that affect how Riak is packaged, so we will release a
separate package for that non-LTS release. In most other cases, however,
if you are running a non-LTS release (such as 12.10) it is safe to
follow the below instructions for the LTS release prior to your release.
In the case of Ubuntu 12.10, follow the installation instructions for
Ubuntu 12.04.

### PAM Library Requirement for Ubuntu

One dependency that may be missing on your machine is the `libpam0g-dev`
package used for Pluggable Authentication Module (PAM) authentication,
associated with [Riak security][security index].

To install:

```bash
sudo apt-get install libpam0g-dev
```

### SSL Library Requirement for Ubuntu

Riak currently requires libssl version 0.9.8 on some versions of Ubuntu.
Starting at Ubuntu 12.04 this is no longer an issue. Before installing
Riak via package on Ubuntu, install the `libssl0.9.8` package. Note that
this version of libssl can be safely installed alongside
current/existing libssl installations.

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

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.4/ubuntu/lucid/riak_2.1.4-1_amd64.deb
sudo dpkg -i riak_2.1.4-1_amd64.deb
```

#### Ubuntu Natty Narwhal (11.04)

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.4/ubuntu/natty/riak_2.1.4-1_amd64.deb
sudo dpkg -i riak_2.1.4-1_amd64.deb
```

#### Ubuntu Precise Pangolin (12.04)

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.4/ubuntu/precise/riak_2.1.4-1_amd64.deb
sudo dpkg -i riak_2.1.4-1_amd64.deb
```

## Installing From Source

First, install Riak dependencies using apt:

```bash
sudo apt-get install build-essential libc6-dev-i386 git
```

Riak requires an [Erlang](http://www.erlang.org/) installation.
Instructions can be found in [Installing Erlang][install source erlang].

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.4/riak-2.1.4.tar.gz
tar zxvf riak-2.1.4.tar.gz
cd riak-2.1.4
make rel
```

If the build was successful, a fresh build of Riak will exist in the
`rel/riak` directory.

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
