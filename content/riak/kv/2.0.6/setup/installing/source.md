---
title_supertext: "Installing"
title: "Riak KV From Source"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "Installing From Source"
    identifier: "installing_source"
    weight: 310
    parent: "installing"
toc: true
aliases:
  - /riak/2.0.6/ops/building/Installing-Riak-from-Source
  - /riak/kv/2.0.6/ops/building/Installing-Riak-from-Source
  - /riak/2.0.6/installing/source/
  - /riak/kv/2.0.6/installing/source/
---



[install source erlang]: {{<baseurl>}}riak/kv/2.0.6/setup/installing/source/erlang
[downloads]: {{<baseurl>}}riak/kv/2.0.6/downloads/
[install debian & ubuntu#source]: {{<baseurl>}}riak/kv/2.0.6/setup/installing/debian-ubuntu/#installing-from-source
[install freebsd#source]: {{<baseurl>}}riak/kv/2.0.6/setup/installing/freebsd/#installing-from-source
[install mac osx#source]: {{<baseurl>}}riak/kv/2.0.6/setup/installing/mac-osx/#installing-from-source
[install rhel & centos#source]: {{<baseurl>}}riak/kv/2.0.6/setup/installing/rhel-centos/#installing-from-source
[install verify]: {{<baseurl>}}riak/kv/2.0.6/setup/installing/verify

Riak should be installed from source if you are building on a platform
for which a package does not exist or if you are interested in
contributing to Riak.

## Dependencies

### Erlang

To install Riak, you will need to have [Erlang](http://www.erlang.org/) installed. We strongly recommend using Basho's patched version of Erlang to install Riak 2.0. All of the patches in this version have been incorporated into later versions of the official Erlang/OTP release.

See [Installing Erlang][install source erlang] for instructions.

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

Download the Riak source package from the [Download Center][downloads] and build:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/2.0/2.0.6/riak-2.0.6.tar.gz
tar zxvf riak-2.0.6.tar.gz
cd riak-2.0.6
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
  
  * [Debian & Ubuntu][install debian & ubuntu#source]
  * [FreeBSD][install freebsd#source]
  * [Mac OS X][install mac osx#source]
  * [RHEL & CentOS][install rhel & centos#source]

If you are running Riak on a platform not in the list above and need
some help getting it up and running, join The Riak Mailing List and
inquire about it there. We are happy to help you get up and running with
Riak.

### Windows

Riak is not currently supported on Microsoft Windows.

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
