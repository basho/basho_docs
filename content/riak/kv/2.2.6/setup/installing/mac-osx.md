---
title_supertext: "Installing on"
title: "Mac OS X"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Mac OS X"
    identifier: "installing_macosx"
    weight: 303
    parent: "installing"
toc: true
aliases:
  - /riak/2.2.6/ops/building/installing/Installing-on-Mac-OS-X
  - /riak/kv/2.2.6/ops/building/installing/Installing-on-Mac-OS-X
  - /riak/2.2.6/installing/mac-osx/
  - /riak/kv/2.2.6/installing/mac-osx/
---



[perf open files]: {{<baseurl>}}riak/kv/2.2.6/using/performance/open-files-limit
[install source erlang]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/source/erlang
[install verify]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/verify

The following steps are known to work with Mac OS X 10.8 and higher. You can install from source or download a precompiled tarball.

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
curl -O https://files.tiot.jp/riak/kv/2.2/2.2.6/osx/10.11/riak-2.2.6-OSX-x86_64.tar.gz
tar xzvf riak-2.2.6-osx-x86_64.tar.gz
```

After the release is untarred, you will be able to `cd` into the `riak`
directory and execute `bin/riak start` to start the Riak node.

## Homebrew

{{% note title="Warning: Homebrew not always up to date" %}}
Homebrew's Riak recipe is community supported, and thus is not always up to
date with the latest Riak package. Please ensure that the current recipe is
using the latest supported code (and don't be afraid to update it if it's
not).
{{% /note %}}

Installing Riak 2.0 with [Homebrew](http://brew.sh/) is easy:

```bash
brew install --devrel riak
```

By default, this will place a `2.2.6` folder in
`/usr/local/Cellar/riak`.

Be aware that you will most likely see the following message after
running `brew install`:

```
Error: The `brew link` step did not complete successfully
The formula built, but is not symlinked into /usr/local

You can try again using:
  brew link riak
```

We do not recommend using `brew link` with Riak. Instead, we recommend
either copying that directory to a desired location on your machine,
aliasing the executables in the `/bin` directory, or interacting with
the Riak installation directory via environment variables.

**Note**: Homebrew will install Erlang if you don't have it already.

## Installing From Source

You must have Xcode tools installed from [Apple's Developer
website](http://developer.apple.com/).

{{% note title="Note on Clang" %}}
Riak has had problems compiling with Clang in the past. As of Riak KV
2.2.6 and Clang 902.0.39.1, Clang can build Riak.
{{% /note %}}

Riak requires [Erlang](http://www.erlang.org/) R16B02+.

If you do not have Erlang already installed, see [Installing Erlang][install source erlang].

Next, download and unpack the source distribution.

```bash
curl -O https://files.tiot.jp/riak/kv/2.2/2.2.6/riak-2.2.6.tar.gz
tar zxvf riak-2.2.6.tar.gz
cd riak-2.2.6
make rel
```

If you receive errors when building about "incompatible architecture,"
please verify that you built Erlang with the same architecture as your
system (Snow Leopard and higher: 64bit).

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
