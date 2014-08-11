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

When installing Riak on CentOS or Red Hat you can install from source or from our custom .rpm package.

## Notes

* CentOS enables SELinux by default and you may need to disable SELinux if you encounter errors.
* Erlang OTP R15B01 and Riak Enterprise 1.2 do not work on CentOS 5.2, but do work on CentOS 5.3 and newer.

## Installing From Our Custom .rpm Package

### For Centos 5 / RHEL 5

You can either install using yum *(recommended)*:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-5-1.noarch.rpm
sudo yum install riak
```

...or install the `rpm` package manually.

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/rhel/5/riak-{{VERSION}}-1.el5.x86_64.rpm
sudo rpm -Uvh riak-{{VERSION}}-1.el5.x86_64.rpm
```

### For Centos 6 / RHEL 6

You can either install using yum *(recommended)*,

```
sudo yum install http://yum.basho.com/gpg/basho-release-6-1.noarch.rpm
sudo yum install riak
```

...or install the `rpm` package manually.

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/rhel/6/riak-{{VERSION}}-1.el6.x86_64.rpm
sudo rpm -Uvh riak-{{VERSION}}-1.el6.x86_64.rpm
```

## Installing From Source

Riak requires [[Erlang|http://www.erlang.org/]] R15B01. *Note: don't use Erlang version R15B02 or R15B03, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.

If you do not have Erlang already installed, see our guide to [[Installing Erlang]]. Don’t worry, it’s easy!

Building from source will require the following packages:

* gcc
* gcc-c++
* glibc-devel
* make

You can install these with yum:

```bash
sudo yum install gcc gcc-c++ glibc-devel make git
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

* [[Post Installation]]: for checking Riak health after installation
* [[Five-Minute Install]]: a guide that will show you how to go from one node to bigger than Google!
