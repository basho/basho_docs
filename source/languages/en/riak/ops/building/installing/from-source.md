---
title: Installing Riak from Source
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, suse]
prev: "[[Installing on AWS Marketplace]]"
up:   "[[Installing and Upgrading]]"
next: "[[Post Installation]]"
download:
  key: source
  name: "any OS in Source Form"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-Riak-from-Source'
}
---

Riak should be installed from source if you are building on a platform
for which a package does not exist or if you are interested in
contributing to Riak.

## Dependencies

To install Riak, you will need to have Erlang installed. We strongly
recommend using Basho's patched version of Erlang to install Riak 2.0.
All of the patches in this version have been incorporated into later
versions of the official Erlang/OTP release.

If you do not have Erlang already installed, see [[Installing Erlang]].
Don't worry, it's easy!

Riak depends on source code located in multiple Git repositories; ensure
that Git is also installed on the target system before attempting the
build.

<div class="note">
<div class="title">Note on Clang</div>
Riak will not compile with Clang. Please make sure your default C/C++
compiler is GCC.
</div>

## Installation

The following instructions generate a complete, self-contained build of
Riak in `$RIAK/rel/riak` where `$RIAK` is the location of the unpacked
or cloned source.

### Installing from source package

Download the Riak source package from the [[Download
Center|http://basho.com/resources/downloads/]] and build:

{{#2.0.0+}}
```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/riak-{{VERSION}.tar.gz
tar zxvf riak-{{VERSION}}.tar.gz
cd riak-{{VERSION}}
make locked-deps
make rel
```
{{/#2.0.0+}}
{{#2.0.0-}}
```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/riak-{{VERSION}.tar.gz
tar zxvf riak-{{VERSION}}.tar.gz
cd riak-{{VERSION}}
make rel
```
{{/2.0.0-}}

### Installing from GitHub

The [Riak Github respository](http://github.com/basho/riak) has much
more information on building and installing Riak from source. To clone
and build Riak from source, follow the steps below.

Clone the repository using [Git](http://git-scm.com) and build:
{{#2.0.0+}}
```bash
git clone git://github.com/basho/riak.git
cd riak
make locked-deps
make rel
```
{{/2.0.0+}}
{{#2.0.0-}}
```bash
git clone git://github.com/basho/riak.git
cd riak
make rel
```
{{/2.0.0-}}

## Platform-Specific Instructions

For instructions about specific platforms, see:

  * [[Installing on Debian and Ubuntu]]
  * [[Installing on Mac OS X]]
  * [[Installing on RHEL and CentOS]]
  * [[Installing on SUSE]]

If you are running Riak on a platform not in the list above and need
some help getting it up and running, join The Riak Mailing List and
inquire about it there. We are happy to help you get up and running with
Riak.

### Windows

Riak is not currently supported on Microsoft Windows.

## Next Steps?

From here you might want to check out:

* [[Post Installation Notes|Post Installation]]: for checking Riak
  health after installation
* [[Five Minute Install]]: a guide that will show you how to go from one
  node to as many as you would like
