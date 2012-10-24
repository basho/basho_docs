---
title: Installing on Mac OS X
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, osx]
prev: ["Installing on RHEL and CentOS", "Installing-on-RHEL-and-CentOS.html"]
up:   ["Installing and Upgrading", "index.html"]
next: ["Installing on FreeBSD", "Installing-on-FreeBSD.html"]
---

The following steps are known to work with Mac OS X 10.5 and 10.6. You can install from source or download a precompiled tarball.

<div class="note"><div class="title">ulimit on OS X</div>OS X gives you a very small limit on open file handles, so even with a backend that uses very few file handles, it's possible to run out. See Open Files Limit for more information about changing the limit.</div>

## Install Types
  * Precompiled Tarballs
  * Homebrew
  * Source

## From Precompiled Tarballs
To run Riak from our precompiled tarball, run these commands for the appropriate platform:

### 64-bit
```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/osx/10.4/riak-1.2.1-osx-x86_64.tar.gz
tar xzvf riak-1.2.1-osx-x86_64.tar.gz
```

### 32-bit
```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/osx/10.4/riak-1.2.1-osx-i386.tar.gz
tar xzvf riak-1.2.1-osx-i386.tar.gz
```

After the release is untared you will be able to cd into the riak directory and execute bin/riak start to start the Riak node.

### 32-bit
Only a 64-bit binary is available at this time.

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

Riak requires [[Erlang|http://www.erlang.org/]] R15B01 or later. If you do not have Erlang already installed, see [[Installing Erlang]]. Don't worry, it's easy!

Next, download and unpack the source distribution.

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```

If you get errors when building about "incompatible architecture", please verify that you built Erlang with the same architecture as your system (Snow Leopard and higher - 64bit, everything else - 32bit).

## Next Steps?
From here you might want to check out:

  * [[The Riak Fast Track]]: a guide for setting up a 3 node cluster and exploring Riak's main features.
  * [[Basic Cluster Setup]]: a guide that will show you how to go from one node to bigger than Google!
