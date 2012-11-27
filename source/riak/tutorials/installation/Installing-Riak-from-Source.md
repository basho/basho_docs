---
title: Installing Riak from Source
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, suse]
prev: ["Installing on SUSE", "Installing-on-SUSE.html"]
up:   ["Installing and Upgrading", "index.html"]
next: ["Rolling Upgrades", "Rolling-Upgrades.html"]
---

Riak should be installed from source if you are building on a platform for which a package does not exist or you are interested in contributing to Riak.

## Dependencies
Riak requires [[Erlang|http://www.erlang.org/]] R15B01. *Note: don't use Erlang version R15B02, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.

If you do not have Erlang already installed, see [[Installing Erlang]]. Don't worry, it's easy!

<div class='note'>Riak will not compile with Clang. Please make sure your default C/C++ compiler is GCC.</div>

## Installation
The following instructions generate a complete, self-contained build of Riak in `$RIAK/rel/riak` where “`$RIAK`” is the location of the unpacked or cloned source.

### Installing from source package
Download the Riak source package from the [[Download Center|http://basho.com/resources/downloads/]] and build:

{{#1.2.0}}
```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/riak-1.2.0.tar.gz
tar zxvf riak-1.2.0.tar.gz
cd riak-1.2.0
make rel
```
{{/1.2.0}}

{{#1.2.1}}
```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```
{{/1.2.1}}

<div class='note'> If you see the error `fatal: unable to connect to github.com` see the following instructions for building on systems with no internet availability </div>

### Installation on Closed Networks
The error `fatal: unable to connect to github.com` when building from source is caused by building on a system with no network connection to Github. Either the port is turned off for security reasons, or the source build is happening on a computer with no outside internet access.  To rectify this problem, an additional file will need to be deployed along with the source tarball.

Download the following `leveldb` archive for the version of Riak you are using:

  * Riak 1.2.1 - https://github.com/basho/leveldb/zipball/1.2.2p5 {{1.2.1+}}
  * Riak 1.2.0 - https://github.com/basho/leveldb/zipball/2aebdd9173a7840f9307e30146ac95f49fbe8e64
  * Riak 1.1.4 - https://github.com/basho/leveldb/zipball/14478f170bbe3d13bc0119d41b70e112b3925453

The instructions going forward will assume Riak 1.2.0, replace the appropriate file for your version.

Deploy the file to the system with the build error and run the following commands.

```bash
$ mv 2aebdd9173a7840f9307e30146ac95f49fbe8e64 riak-1.2.0/deps/eleveldb/c_src/leveldb.zip
$ cd riak-1.2.0/deps/eleveldb/c_src/
$ unzip leveldb.zip
$ mv basho-leveldb-* leveldb
$ cd ../../../
$ make rel
```

### Installing from GitHub
The [[Riak Github repository|http://github.com/basho/riak]] has much more information on building and installing Riak from source. To clone and and build Riak from source, follow these steps:

Clone the repository using [[Git|http://git-scm.com/]] and build:

```bash
git clone git://github.com/basho/riak.git
cd riak
make rel
```

## Platform Specific Instructions
For instructions about specific platforms, see:

  * [[Installing on Debian and Ubuntu]]
  * [[Installing on Mac OS X]]
  * [[Installing on RHEL and CentOS]]
  * [[Installing on SUSE]]

If you are running Riak on a platform not in the list above and need some help getting it up and running, join The Riak Mailing List and inquire about it there. We are happy to help you get up and running with Riak.

### Windows
Riak is not currently supported on Microsoft Windows.
