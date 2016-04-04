---
title: Installing on Debian and Ubuntu
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, debian, ubuntu, linux]
download:
  key: debian
  name: "Debian or Ubuntu"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-Debian-and-Ubuntu',
    '2.1.1-': '/ops/building/installing/Installing-on-Debian-and-Ubuntu',
}
---

Riak can be installed on Debian- or Ubuntu-based systems using a binary
package or by [[compiling Riak from source code|Installing Riak from
Source]]. The following steps have been tested to work with Riak on
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

* [Lucid](https://packagecloud.io/basho/riak/packages/ubuntu/lucid/riak_{{VERSION}}-1_amd64.deb)
* [Precise](https://packagecloud.io/basho/riak/packages/ubuntu/precise/riak_{{VERSION}}-1_amd64.deb)
* [Squeeze](https://packagecloud.io/basho/riak/packages/debian/squeeze/riak_{{VERSION}}-1_amd64.deb)
* [Trusty](https://packagecloud.io/basho/riak/packages/ubuntu/trusty/riak_{{VERSION}}-1_amd64.deb)
* [Wheezy](https://packagecloud.io/basho/riak/packages/debian/wheezy/riak_{{VERSION}}-1_amd64.deb)

Our documentation also includes instructions regarding signing keys and
sources lists, which can be found in the section below.

## Installing with apt and Packagecloud

<div class="note">
<div class="title">Note on Debian 7</div>
If you wish to install Riak on Debian 7, you may need to install
[libc6](://packages.debian.org/search?keywords=libc6) version 2.15 or
later, which in turn requires upgrading your system to
[sid](https://www.debian.org/releases/sid/). Installation instructions
can be found
[here](https://wiki.debian.org/DebianUnstable#How_do_I_install_Sid.3F).

Once sid has been installed, you can install libc6 with the following
command:

```bash
apt-get -t sid install libc6 libc6-dev libc6-dbg
```
</div>

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
sudo apt-get install riak=2.1.3-1
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
associated with [[Riak security|Authentication and Authorization]].

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
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/ubuntu/lucid/riak_{{VERSION}}-1_amd64.deb
sudo dpkg -i riak_{{VERSION}}-1_amd64.deb
```

#### Ubuntu Natty Narwhal (11.04)

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/ubuntu/natty/riak_{{VERSION}}-1_amd64.deb
sudo dpkg -i riak_{{VERSION}}-1_amd64.deb
```

#### Ubuntu Precise Pangolin (12.04)

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/ubuntu/precise/riak_{{VERSION}}-1_amd64.deb
sudo dpkg -i riak_{{VERSION}}-1_amd64.deb
```

## Installing From Source

First, install Riak dependencies using apt:

```bash
sudo apt-get install build-essential libc6-dev-i386 git
```

Riak requires an [Erlang](http://www.erlang.org/) installation.
Instructions can be found in [[Installing Erlang]].

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/riak-{{VERSION}}.tar.gz
tar zxvf riak-{{VERSION}}.tar.gz
cd riak-{{VERSION}}
make rel
```

If the build was successful, a fresh build of Riak will exist in the
`rel/riak` directory.

## Next Steps

Now that Riak is installed, check out [[Verifying a Riak Installation]].
