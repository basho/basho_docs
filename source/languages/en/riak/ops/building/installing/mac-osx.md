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
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-Mac-OS-X/'
}
---

The following steps are known to work with Mac OS X 10.5 and 10.6. You can install from source or download a precompiled tarball.

## Install Types
  * Precompiled Tarballs
  * Homebrew
  * Source

<div class="note"><div class="title">ulimit on OS X</div>OS X gives you a very small limit on open file handles, so even with a backend that uses very few file handles, it's possible to run out. See [[Open Files Limit]] for more information about changing the limit.</div>

## From Precompiled Tarballs
To run Riak from our precompiled tarball, run these commands for the appropriate platform:

{{#1.2.0-}}

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak-{{V.V}}-osx-x86_64.tar.gz
tar xzvf riak-{{V.V.V}}-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak-{{V.V.V}}-osx-i386.tar.gz
tar xzvf riak-{{V.V.V}}-osx-i386.tar.gz
```

{{/1.2.0-}}
{{#1.2.0}}

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.4/riak-{{V.V.V}}-osx-x86_64.tar.gz
tar xzvf riak-{{V.V.V}}-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.4/riak-{{V.V.V}}-osx-i386.tar.gz
tar xzvf riak-{{V.V.V}}-osx-i386.tar.gz
```

{{/1.2.0}}
{{#1.2.1}}

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.4/riak-{{V.V.V}}-osx-x86_64.tar.gz
tar xzvf riak-{{V.V.V}}-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.4/riak-{{V.V.V}}-osx-i386.tar.gz
tar xzvf riak-{{V.V.V}}-osx-i386.tar.gz
```

{{/1.2.1}}
{{#1.3.0-1.3.2}}

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.6/riak-{{V.V.V}}-osx-x86_64.tar.gz
tar xzvf riak-{{V.V.V}}-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.6/riak-{{V.V.V}}-osx-i386.tar.gz
tar xzvf riak-{{V.V.V}}-osx-i386.tar.gz
```

{{/1.3.0-1.3.2}}
{{#1.3.2+}}

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.8/riak-{{V.V.V}}-osx-x86_64.tar.gz
tar xzvf riak-{{V.V.V}}-osx-x86_64.tar.gz
```

### 32-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/osx/10.8/riak-{{V.V.V}}-osx-i386.tar.gz
tar xzvf riak-{{V.V.V}}-osx-i386.tar.gz
```

{{/1.3.2+}}

After the release is untarred you will be able to cd into the riak directory and execute bin/riak start to start the Riak node.

## Homebrew

<div class="note">Homebrew's Riak recipe is community supported, and thus is not always up to date with the latest Riak package. Please ensure that the current recipe is using the latest supported code (and don't be scared to update if it's not).</div>

Installing with Homebrew is easy:

```bash
brew install riak
```

Homebrew will install Erlang if you don't have it already.

## From Source
You must have Xcode tools installed from the CD that came with your Mac or from [[Apple's Developer website|http://developer.apple.com/]].

<div class="note">Riak will not compile with Clang. Please make sure your default C/C++ compiler is GCC.</div>

Riak requires [[Erlang|http://www.erlang.org/]] R15B01. *Note: don't use Erlang version R15B02 or R15B03, for the moment, as it causes an [error with riak-admin status](https://github.com/basho/riak/issues/227) commands*.

If you do not have Erlang already installed, see [[Installing Erlang]]. Don't worry, it's easy!

Next, download and unpack the source distribution.

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{V.V.V}}/riak-{{V.V.V}}.tar.gz
tar zxvf riak-{{V.V.V}}.tar.gz
cd riak-{{V.V.V}}
make rel
```

If you get errors when building about "incompatible architecture", please verify that you built Erlang with the same architecture as your system (Snow Leopard and higher - 64bit, everything else - 32bit).

## Next Steps?
From here you might want to check out:

  * [[Post Installation]]: for checking Riak health after installation
  * [[Five Minute Install]]: a guide that will show you how to go from one node to bigger than Google!
