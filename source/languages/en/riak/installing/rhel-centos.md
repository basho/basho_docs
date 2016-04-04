---
title: Installing on RHEL and CentOS
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, rhel, centos, linux]
download:
  key: rhel
  name: "Red Hat or CentOS"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-RHEL-and-CentOS',
    '2.1.1-': '/ops/building/installing/Installing-on-RHEL-and-CentOS'
}
---

Riak can be installed on CentOS- or Red-Hat-based systems using a binary
package or by [[compiling Riak from source code|Installing Riak from
Source]]. The following steps have been tested to work with Riak on
CentOS/RHEL 5.10, 6.5, and 7.0.1406.

<div class="note">
<div class="title">Note on SELinux</div>
CentOS enables SELinux by default, so you may need to disable SELinux if
you encounter errors.
</div>

## Installing with rpm

For versions of Riak prior to 2.0, Basho used a self-hosted
[rpm](http://www.rpm.org/) repository for CentOS and RHEL packages. For
versions 2.0 and later, Basho has moved those packages to the
[packagecloud.io](https://packagecloud.io/) hosting service.
Instructions for installing via shell scripts, manual installation,
Chef, and Puppet can be found in packagecloud's [installation
docs](https://packagecloud.io/basho/riak/install).

Platform-specific pages are linked below:

* [el5](https://packagecloud.io/basho/riak/packages/el/5/riak-{{VERSION}}-1.x86_64.rpm)
* [el6](https://packagecloud.io/basho/riak/packages/el/6/riak-{{VERSION}}-1.el6.x86_64.rpm)
* [el7](https://packagecloud.io/basho/riak/packages/el/7/riak-{{VERSION}}-1.el7.centos.x86_64.rpm)
* [Fedora 19](https://packagecloud.io/basho/riak/packages/fedora/19/riak-{{VERSION}}-1.fc19.x86_64.rpm)

## Installing with Yum and Packagecloud

For the simplest installation process on Long-Term Support (LTS)
releases, use yum and packagecloud:

```bash
curl -s https://packagecloud.io/install/repositories/basho/riak/script.rpm.sh | sudo bash
```

#### For CentOS 5 / RHEL 5:

```bash
sudo yum install riak-2.1.3-1.el5.x86_64
```

#### For CentOS 6 / RHEL 6:

```bash
sudo yum install riak-2.1.3-1.el6.x86_64
```

## Installing with Yum and Packages

If you wish to install the RHEL/CentOS packages by hand, follow these
instructions.

#### For Centos 5 / RHEL 5

Download the package and install:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.3/rhel/5/riak-2.1.3-1.el5.x86_64.rpm
sudo yum install riak-2.1.3-1.el5.x86_64
```

#### For Centos 6 / RHEL 6

Download the package and install:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.3/rhel/6/riak-2.1.3-1.el6.x86_64.rpm
sudo yum install riak-2.1.3-1.el6.x86_64
```

## Installing with rpm

#### For Centos 5 / RHEL 5

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.3/rhel/5/riak-2.1.3-1.el5.x86_64.rpm
sudo rpm -Uvh riak-2.1.3-1.el5.x86_64.rpm
```

#### For Centos 6 / RHEL 6

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/2.1/2.1.3/rhel/6/riak-2.1.3-1.el6.x86_64.rpm
sudo rpm -Uvh riak-2.1.3-1.el6.x86_64.rpm
```

## Installing From Source

Riak requires an [[Erlang|http://www.erlang.org/]] installation.
Instructions can be found in [[Installing Erlang]].

Building from source will require the following packages:

* `gcc`
* `gcc-c++`
* `glibc-devel`
* `make`
* `pam-devel`

You can install these with yum:

```bash
sudo yum install gcc gcc-c++ glibc-devel make git pam-devel
```

Now we can download and install Riak:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/riak-{{VERSION}}.tar.gz
tar zxvf riak-{{VERSION}}.tar.gz
cd riak-{{VERSION}}
make rel
```

You will now have a fresh build of Riak in the `rel/riak` directory.

## Next Steps

Now that Riak is installed, check out [[Verifying a Riak Installation]].
