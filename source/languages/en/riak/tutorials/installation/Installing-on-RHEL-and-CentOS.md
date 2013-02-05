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
---

When installing Riak on CentOS or Redhat you can install from source or from our custom .rpm package.

## Notes

* CentOS enables SE Linux by default and you may need to disable SE Linux if you encounter errors.
* Erlang OTP R15B01 and Riak Enterprise 1.2 do not work on CentOS 5.2, but do work on CentOS 5.3 and newer.

## Installing From Our Custom .rpm Package

### For Centos 5 / RHEL 5

You can either install using yum *(recommended)*,

```
package=basho-release-5-1.noarch.rpm && \
wget http://yum.basho.com/gpg/$package -O /tmp/$package && \
sudo rpm -ivh /tmp/$package
sudo yum install riak
```

...or install the rpm package manually.

{{#1.0.3}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak-1.0.3-1.el5.x86_64.rpm
sudo rpm -Uvh riak-1.0.3-1.el5.x86_64.rpm
```

{{/1.0.3}}
{{#1.1.4}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak-1.1.4-1.el5.x86_64.rpm
sudo rpm -Uvh riak-1.1.4-1.el5.x86_64.rpm
```

{{/1.1.4}}
{{#1.2.0}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/rhel/5/riak-1.2.0-1.el5.x86_64.rpm
sudo rpm -Uvh riak-1.2.0-1.el5.x86_64.rpm
```

{{/1.2.0}}
{{#1.2.1}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/rhel/5/riak-1.2.1-1.el5.x86_64.rpm
sudo rpm -Uvh riak-1.2.1-1.el5.x86_64.rpm
```

{{/1.2.1}}
{{#1.3.0}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/1.3/1.3.0rc2/rhel/5/riak-1.3.0rc2-1.el5.x86_64.rpm
sudo rpm -Uvh riak-1.3.0rc2-1.el5.x86_64.rpm
```

{{/1.3.0}}

### For Centos 6 / RHEL 6

You can either install using yum *(recommended)*,

```
package=basho-release-6-1.noarch.rpm && \
wget http://yum.basho.com/gpg/$package -O /tmp/$package && \
sudo rpm -ivh /tmp/$package
sudo yum install riak
```

...or install the rpm package manually.

{{#1.0.3}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak-1.0.3-1.el6.x86_64.rpm
sudo rpm -Uvh riak-1.0.3-1.el6.x86_64.rpm
```

{{/1.0.3}}
{{#1.1.4}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak-1.1.4-1.el6.x86_64.rpm
sudo rpm -Uvh riak-1.1.4-1.el6.x86_64.rpm
```

{{/1.1.4}}
{{#1.2.0}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/rhel/6/riak-1.2.0-1.el6.x86_64.rpm
sudo rpm -Uvh riak-1.2.0-1.el6.x86_64.rpm
```

{{/1.2.0}}
{{#1.2.1}}

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/rhel/6/riak-1.2.1-1.el6.x86_64.rpm
sudo rpm -Uvh riak-1.2.1-1.el6.x86_64.rpm
```

{{/1.2.1}}
{{#1.3.0}}

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/1.3/1.3.0rc2/rhel/6/riak-1.3.0rc2-1.el6.x86_64.rpm
sudo rpm -Uvh riak-1.3.0rc2-1.el6.x86_64.rpm
```

{{/1.3.0}}


## Installing From Source

Riak requires [[Erlang|http://www.erlang.org/]] R15B01. *Note: don't use Erlang version R15B02, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.

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

You will now have a fresh build of Riak in the `rel/riak` directory.

## Next Steps?

From here you might want to check out:

* [[Post Installation|Post Installation Notes]]: for checking Riak health after installation
* [[The Riak Fast Track]]: a guide for setting up a 3 node cluster and exploring Riak’s main features.
* [[Basic Cluster Setup]]: a guide that will show you how to go from one node to bigger than Google!
