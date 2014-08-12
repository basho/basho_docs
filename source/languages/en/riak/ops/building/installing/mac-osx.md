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
    '1.4.0-': '/tutorials/installation/Installing-on-Mac-OS-X'
}
---

The following steps are known to work with Mac OS X 10.8, 10.9
(Mavericks), and Yosemite. You can install from source or download a
precompiled tarball.

## Install Types

* Precompiled Tarballs
* Homebrew
* Source

<div class="note">
<div class="title"><code>ulimit</code> on OS X</div>
OS X gives you a very small limit on open file handles, so even with a
backend that uses very few file handles, it's possible to run out. See
[[Open Files Limit]] for more information about changing the limit.
</div>

## From Precompiled Tarballs

To run Riak from our precompiled tarball, run these commands for the
appropriate platform:

### 64-bit

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/osx/10.8/riak-{{VERSION}}-OSX-x86_64.tar.gz
tar xzvf riak-{{VERSION}}-osx-x86_64.tar.gz
```

After the release is untarred, you will be able to `cd` into the `/riak`
directory and execute `bin/riak start` to start the Riak node.

## Homebrew

<div class="note">
<div class="title">Warning: Homebrew not always up to date</div>
Homebrew's Riak recipe is community supported, and thus is not always up
to date with the latest Riak package. Please ensure that the current
recipe is using the latest supported code (and don't be afraid to update
it if it's not).
</div>

Installing Riak 2.0 with [Homebrew](http://brew.sh/) is easy:

```bash
brew install --devel riak
```

By default, this will place a `{{VERSION}}` folder in
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

## From Source

You must have Xcode tools installed from [Apple's Developer
website](http://developer.apple.com/).

<div class="note">
<div title="title">Note on Clang</div>
Riak will not compile with Clang. Please make sure that your default
C/C++ compiler is GCC.
</div>

Riak requires [Erlang](http://www.erlang.org/) {{#2.0.0-}}R15B01{{/2.0.0-}}{{#2.0.0+}}R16B02+{{/2.0.0+}}.

If you do not have Erlang already installed, see [[Installing Erlang]].
Don't worry, it's easy!

Next, download and unpack the source distribution.

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/riak-{{VERSION}}.tar.gz
tar zxvf riak-{{VERSION}}.tar.gz
cd riak-{{VERSION}}
make rel
```

If you receive errors when building about "incompatible architecture,"
please verify that you built Erlang with the same architecture as your
system (Snow Leopard and higher: 64bit{{#1.4.0-}}, everything else:
32bit{{/1.4.0-}}).

## Next Steps?

From here, you might want to check out:

* [[Post Installation]] --- Information on checking Riak health after
  installation
* [[Five-Minute Install]] --- A guide that will show you how to go from
  one node to as many as you'd like
