---
title_supertext: "Installing on"
title: "Debian and Ubuntu"
description: ""
project: "riak_kv"
project_version: "3.0.2"
lastmod: 2021-01-05T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.2:
    name: "Debian & Ubuntu"
    identifier: "installing_debian_ubuntu"
    weight: 303
    parent: "installing"
toc: true
aliases:
  - /riak/3.0.2/ops/building/installing/Installing-on-Debian-and-Ubuntu
  - /riak/kv/3.0.2/ops/building/installing/Installing-on-Debian-and-Ubuntu
  - /riak/3.0.2/installing/debian-ubuntu/
  - /riak/kv/3.0.2/installing/debian-ubuntu/
---

[install source index]: {{<baseurl>}}riak/kv/3.0.2/setup/installing/source/
[security index]: {{<baseurl>}}riak/kv/3.0.2/using/security/
[install source erlang]: {{<baseurl>}}riak/kv/3.0.2/setup/installing/source/erlang
[install verify]: {{<baseurl>}}riak/kv/3.0.2/setup/installing/verify

Riak KV can be installed on Debian or Ubuntu-based systems using a binary
package or by compiling from source code.

The following steps have been tested to work with Riak KV on:

- Ubuntu 18.02
- Ubuntu 16.04
- Ubuntu 14.04
- Ubuntu 12.04
- Debian 9.2
- Debian 8.6
- Debian 7.6
- Raspbian Buster

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

## Installing From Package

If you wish to install the deb packages by hand, follow these
instructions.

### Installing on Non-LTS Ubuntu Releases

Typically we only package Riak for LTS releases to keep our build and
testing matrix focused.  In some cases, such as the historic Ubuntu 11.04 (Natty),
there are changes that affect how Riak is packaged, so we will release a
separate package for that non-LTS release. In most other cases, however,
if you are running a non-LTS release (such as 12.10) it is safe to
follow the below instructions for the LTS release prior to your release.
In the case of later subversions such as Ubuntu 12.10, follow the installation instructions for
Ubuntu 12.04.

### PAM Library Requirement for Ubuntu

One dependency that may be missing on your machine is the `libpam0g-dev`
package used for Pluggable Authentication Module (PAM) authentication,
associated with [Riak security][security index].

To install:

```bash
sudo apt-get install libpam0g-dev
```

### Riak 64-bit Installation

#### Ubuntu Bionic Beaver (18.04)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/ubuntu/bionic64/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Ubuntu Xenial Xerus (16.04)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/ubuntu/xenial64/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Ubuntu Trusty Tahr (14.04)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/ubuntu/trusty64/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Ubuntu Precise Pangolin (12.04)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/ubuntu/precise64/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Debian Stretch (9.0)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/debian/9/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Debian Jessie (8.0)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/debian/8/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Debian Wheezy (7.0)

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/debian/7/riak_3.0.2-1_amd64.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

#### Raspbian Buster

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/raspbian/buster/riak_3.0.2-1_armhf.deb
sudo dpkg -i riak_3.0.2-1_amd64.deb
```

## Installing From Source

First, install Riak dependencies using apt:

```bash
sudo apt-get install build-essential libc6-dev-i386 git
```

Riak requires an [Erlang](http://www.erlang.org/) installation.
Instructions can be found in [Installing Erlang][install source erlang].

```bash
wget https://files.tiot.jp/riak/kv/3.0/3.0.2/riak-3.0.2.tar.gz
tar zxvf riak-3.0.2.tar.gz
cd riak-3.0.2
make rel
```

If the build was successful, a fresh build of Riak will exist in the
`rel/riak` directory.

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].

