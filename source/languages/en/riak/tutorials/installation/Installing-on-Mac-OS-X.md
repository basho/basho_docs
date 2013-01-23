---
title: Installing on Mac OS X
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, osx]
prev: "[[Installing on RHEL and CentOS]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on FreeBSD]]"
download: 
  key: osx
  name: "Mac OS X"
---

The following steps are known to work with Mac OS X 10.5 and 10.6. You can install from source or download a precompiled tarball.

## Install Types
  * Precompiled Tarballs
  * Homebrew
  * Source

<div class="note"><div class="title">ulimit on OS X</div>OS X gives you a very small limit on open file handles, so even with a backend that uses very few file handles, it's possible to run out. See [[Open Files Limit]] for more information about changing the limit.</div>

## From Precompiled Tarballs
To run Riak from our precompiled tarball, run these commands for the appropriate platform:

{{#1.0.3}}
### 64-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak-1.0.3-osx-x86_64.tar.gz
tar xzvf riak-1.0.3-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.0/1.0.3/riak-1.0.3-osx-i386.tar.gz
tar xzvf riak-1.0.3-osx-i386.tar.gz
```

{{/1.0.3}}
{{#1.1.4}}

### 64-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak-1.1.4-osx-x86_64.tar.gz
tar xzvf riak-1.1.4-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.1/1.1.4/riak-1.1.4-osx-i386.tar.gz
tar xzvf riak-1.1.4-osx-i386.tar.gz
```

{{/1.1.4}}
{{#1.2.0}}

### 64-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/osx/10.4/riak-1.2.0-osx-x86_64.tar.gz
tar xzvf riak-1.2.0-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/osx/10.4/riak-1.2.0-osx-i386.tar.gz
tar xzvf riak-1.2.0-osx-i386.tar.gz
```

{{/1.2.0}}
{{#1.2.1}}

### 64-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/osx/10.4/riak-1.2.1-osx-x86_64.tar.gz
tar xzvf riak-1.2.1-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/osx/10.4/riak-1.2.1-osx-i386.tar.gz
tar xzvf riak-1.2.1-osx-i386.tar.gz
```

{{/1.2.1}}

After the release is untared you will be able to cd into the riak directory and execute bin/riak start to start the Riak node.

## Homebrew

<div class="note">Homebrew's Riak recipe is community supported, and thus is not always up to date with the latest Riak package. Please ensure that the current recipe is using the latest supported code (and don't be scared to update if it's not).</div>

Installing with Homebrew is easy:

```bash
brew install riak
```

Homebrew will install Erlang if you don't have it already.

## From Source
You must have XCode tools installed from the CD that came with your Mac or from [[Apple's Developer website|http://developer.apple.com/]].

<div class="note">Riak will not compile with Clang. Please make sure your default C/C++ compiler is GCC.</div>

Riak requires [[Erlang|http://www.erlang.org/]] R15B01. *Note: don't use Erlang version R15B02, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.

If you do not have Erlang already installed, see [[Installing Erlang]]. Don't worry, it's easy!

Next, download and unpack the source distribution.

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

If you get errors when building about "incompatible architecture", please verify that you built Erlang with the same architecture as your system (Snow Leopard and higher - 64bit, everything else - 32bit).

## Next Steps?
From here you might want to check out:

  * [[Post Installation|Post Installation Notes]]: for checking Riak health after installation
  * [[The Riak Fast Track]]: a guide for setting up a 4 node cluster and exploring Riak's main features.
  * [[Basic Cluster Setup]]: a guide that will show you how to go from one node to bigger than Google!
