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

Riak can be installed on CentOS- or Red-Hat-based systems with a binary
package or by [[compiling Riak from source code|Installing Riak from
Source]]. The following steps have been tested to work with Riak on
**CentOS version XXX** and **Red Hat version XXX**.

## Notes

* CentOS enables SELinux by default, and you may need to disable SELinux
  if you encounter errors

## Installing with rpm


### For Centos 5 / RHEL 5

You can install CentOS 5/RHEL 5 using yum, which we recommend:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-5-1.noarch.rpm
sudo yum install riak
```

Or you can install the `.rpm` package manuall:

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

We strongly recommend using
Riak requires [[Erlang|http://www.erlang.org/]] R15B01. If you do not
have Erlang already installed, see our guide to [[Installing Erlang]].
Don’t worry, it’s easy!

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
