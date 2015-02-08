---
title: Installing on RHEL and CentOS
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, rhel, centos, linux]
prev: "[[Installing on Debian and Ubuntu]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on Mac OS X]]"
download:
  key: rhel
  name: "Red Hat or CentOS"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-RHEL-and-CentOS'
}
---

<div class="note">
<div class="title">Note: 2.0.4 not currently available</div>
Riak version 2.0.4 is not currently available for RHEL/CentOS due to a
known issue. If you'd like to upgrade Riak, we'd recommend waiting for
the 2.0.5 release.
</div>

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

* [el5](https://packagecloud.io/basho/riak/riak-2.0.0-1.x86_64.rpm?distro=5)
* [el6](https://packagecloud.io/basho/riak/riak-2.0.0-1.el6.x86_64.rpm?distro=6)
* [el7](https://packagecloud.io/basho/riak/riak-2.0.0-1.el7.centos.x86_64.rpm?distro=7)
* [Fedora 19](https://packagecloud.io/basho/riak/riak-2.0.0-1.fc19.x86_64.rpm?distro=19)

Our documentation also includes instructions regarding signing keys and
sources lists, which can be found in the [[Advanced rpm
Installation|Installing on RHEL and CentOS#Advanced-rpm-Installation]]
section immediately below.

## Advanced rpm Installation

For the simplest installation process on LTS (Long-Term Support)
releases, use yum. First, you must install the `pygpgme` package, which
enables yum to handle [GPG](https://www.gnupg.org/) signatures:

```bash
sudo yum install pygpgme
```

If you wish to install using a `.repo` file, packagecloud can generate
one for you on the basis of a name that you specify, e.g. a hostname,
and the desired operating system and distribution. The following example
script would store your hostname in the variable `HOSTNAME`, send that
information to packagecloud to generate a `.repo` file, and then store
the return value in a file called `basho.repo`, which is stored in the
`/etc/yum.repos.d` directory:

```bash
#!/bin/bash

HOSTNAME=`hostname -f`
FILENAME=/etc/yum.repos.d/basho.repo
OS=el
DIST=5
PACKAGE_CLOUD_RIAK_DIR=https://packagecloud.io/install/repositories/basho/riak
curl "${PACKAGE_CLOUD_RIAK_DIR}/config_file.repo?os=${OS}&dist=${DIST}&name=${HOSTNAME}" > $FILENAME
```

The `name` that you submit to packagecloud can be anything you like. The
`HOSTNAME` used above was for example purposes. The resulting file
should contents like the following:

```
[basho_riak]
name=basho_riak
baseurl=https://packagecloud.io/basho/riak/el/5/$basesearch
repo_gpgcheck=1
gpgcheck=0
enabled=1
gpgkey=https://packagecloud.io/gpg.key
sslverify=1
sslcacert=/etc/pki/tls/certs/ca-bundle.crt
```

With your `basho.repo` file population, you can update your rpm sources
list.

## Installing From Package

If you wish to install the RHEL/CentOS packages by hand, follow these
instructions.

### For Centos 5 / RHEL 5

You can install CentOS 5/RHEL 5 using yum, which we recommend:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-5-1.noarch.rpm
sudo yum install riak
```

Or you can install the `.rpm` package manually:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/rhel/5/riak-{{VERSION}}-1.el5.x86_64.rpm
sudo rpm -Uvh riak-{{VERSION}}-1.el5.x86_64.rpm
```

### For Centos 6 / RHEL 6

You can install using yum, which we recommend:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-6-1.noarch.rpm
sudo yum install riak
```

Or you can install the `.rpm` package manually:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/rhel/6/riak-{{VERSION}}-1.el6.x86_64.rpm
sudo rpm -Uvh riak-{{VERSION}}-1.el6.x86_64.rpm
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

## Next Steps?

From here you might want to check out:

* [[Post Installation]] --- A guide to checking Riak health after
  installation
* [[Five-Minute Install]] --- A guide that will show you how to go from
  one node to as many as you'd like
