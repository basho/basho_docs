---
title_supertext: "Installing on"
title: "Mac OS X"
description: ""
project: "riak_kv"
project_version: "2.2.3"
menu:
  riak_kv-2.2.3:
    name: "Mac OS X"
    identifier: "installing_macosx"
    weight: 303
    parent: "installing"
toc: true
aliases:
  - /riak/2.2.3/ops/building/installing/Installing-on-Mac-OS-X
  - /riak/kv/2.2.3/ops/building/installing/Installing-on-Mac-OS-X
  - /riak/2.2.3/installing/mac-osx/
  - /riak/kv/2.2.3/installing/mac-osx/
---



[perf open files]: /riak/kv/2.2.3/using/performance/open-files-limit
[install source erlang]: /riak/kv/2.2.3/setup/installing/source/erlang
[install verify]: /riak/kv/2.2.3/setup/installing/verify

The following steps are known to work with Mac OS X 10.8, 10.9
(Mavericks), and Yosemite. You can install from source or download a
precompiled tarball.

> **`ulimit` on OS X**
>
> OS X gives you a very small limit on open file handles, so even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][perf open files] for more information about changing the limit.


## From Precompiled Tarballs

To run Riak from our precompiled tarball, run these commands for the
appropriate platform:

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/2.2/2.2.3/osx/10.8/riak-2.2.3-OSX-x86_64.tar.gz
tar xzvf riak-2.2.3-osx-x86_64.tar.gz
```

After the release is untarred, you will be able to `cd` into the `riak`
directory and execute `bin/riak start` to start the Riak node.

## Installing From Source

You must have Xcode tools installed from [Apple's Developer
website](http://developer.apple.com/).

{{% note title="Note on Clang" %}}
Riak will not compile with Clang. Please make sure that your default C/C++
compiler is [GCC](https://gcc.gnu.org/).
{{% /note %}}

Riak requires [Erlang](http://www.erlang.org/) R16B02+.

If you do not have Erlang already installed, see [Installing Erlang][install source erlang].

Next, download and unpack the source distribution.

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/2.2/2.2.3/riak-2.2.3.tar.gz
tar zxvf riak-2.2.3.tar.gz
cd riak-2.2.3
make rel
```

If you receive errors when building about "incompatible architecture,"
please verify that you built Erlang with the same architecture as your
system (Snow Leopard and higher: 64bit).

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
