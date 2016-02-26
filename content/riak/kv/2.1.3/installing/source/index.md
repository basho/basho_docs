---
title: "Installing Riak KV From Source"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Installing From Source"
    identifier: "installing_source"
    weight: 310
    parent: "installing"
toc: true
aliases:
  - /riak/2.1.3/installing/source/
  - /riak/2.1.1/installing/source/
---

[Install Debian & Ubuntu#Source]: /riak/kv/2.1.3/installing/debian-ubuntu/#Installing-From-Source
[Install FreeBSD#Source]: /riak/kv/2.1.3/installing/freebsd/#Installing-From-Source
[Install Mac OSX#Source]: /riak/kv/2.1.3/installing/mac-osx/#Installing-From-Source
[Install RHEL & CentOS#Source]: /riak/kv/2.1.3/installing/rhel-centos/#Installing-From-Source
[Install Verify]: /riak/kv/2.1.3/installing/verify
[Install Erlang]: /riak/kv/2.1.3/installing/source/erlang

Riak should be installed from source if you are building on a platform
for which a package does not exist or if you are interested in
contributing to Riak.

## Dependencies

### Erlang

To install Riak, you will need to have [Erlang](http://www.erlang.org/) installed. We strongly recommend using Basho's patched version of Erlang to install Riak 2.0. All of the patches in this version have been incorporated into later versions of the official Erlang/OTP release.

See [Installing Erlang][Install Erlang] for instructions.

### Git

Riak depends on source code located in multiple Git repositories. Install [Git](https://git-scm.com/) on the target system before attempting the build.

### GCC

Riak will not compile with Clang. Please make sure your default C/C++
compiler is [GCC](https://gcc.gnu.org/).

## Installation

The following instructions generate a complete, self-contained build of
Riak in `$RIAK/rel/riak` where `$RIAK` is the location of the unpacked
or cloned source.

### Installing from source package

Download the Riak source package from the [[Download
Center|http://basho.com/resources/downloads/]] and build:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/riak-{{VERSION}}.tar.gz
tar zxvf riak-{{VERSION}}.tar.gz
cd riak-{{VERSION}}
make locked-deps
make rel
```

### Installing from GitHub

The [Riak Github respository](http://github.com/basho/riak) has much
more information on building and installing Riak from source. To clone
and build Riak from source, follow the steps below.

Clone the repository using [Git](http://git-scm.com) and build:

```bash
git clone git://github.com/basho/riak.git
cd riak
make locked-deps
make rel
```

## Platform-Specific Instructions

For instructions about specific platforms, see:
  
  * [Debian & Ubuntu][Install Debian & Ubuntu#Source]
  * [FreeBSD][Install FreeBSD#Source]
  * [Mac OS X][Install Mac OSX#Source]
  * [RHEL & CentOS][Install RHEL & CentOS#Source]

If you are running Riak on a platform not in the list above and need
some help getting it up and running, join The Riak Mailing List and
inquire about it there. We are happy to help you get up and running with
Riak.

### Windows

Riak is not currently supported on Microsoft Windows.

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][Install Verify].
