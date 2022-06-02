---
title_supertext: "Installing on"
title: "FreeBSD"
description: ""
project: "riak_kv"
project_version: "2.0.2"
menu:
  riak_kv-2.0.2:
    name: "FreeBSD"
    identifier: "installing_freebsd"
    weight: 303
    parent: "installing"
toc: true
aliases:
  - /riak/2.0.2/ops/building/installing/Installing-on-FreeBSD
  - /riak/kv/2.0.2/ops/building/installing/Installing-on-FreeBSD
  - /riak/2.0.2/installing/freebsd/
  - /riak/kv/2.0.2/installing/freebsd/
---



[install source erlang]: {{<baseurl>}}riak/kv/2.0.2/setup/installing/source/erlang
[downloads]: {{<baseurl>}}riak/kv/2.0.2/downloads/
[install verify]: {{<baseurl>}}riak/kv/2.0.2/setup/installing/verify

You can install Riak on FreeBSD for the AMD64 architecture with a binary package or by building from source code.

## Installing From Binary Package

> **Note:** The Riak 1.2 binary package is supported on FreeBSD version 9. Users have reported success building Riak from source on a number of FreeBSD versions, however.

Installing Riak from a binary package is the simplest method with least required dependencies, and requires less time to complete than building from source.

### Prerequisites and Dependencies

Riak depends on `sudo` to be installed if the Riak command line tools are to be executed by users other than the *riak* user. Please ensure that `sudo` is installed via packages or the ports collection prior to installing the Riak package.

The Riak binary package also depends on a packaged version of OpenSSL. Prior to installing Riak 1.2 on FreeBSD 9, you'll need to install `openssl-1.0.0_7` either from package or the ports collection.

### Installation

You can install the Riak binary package on FreeBSD remotely using the
`pkg_add` remote option. For this example, we're installing `riak-2.0.2-FreeBSD-amd64.tbz`.

```bash
sudo pkg_add -r http://s3.amazonaws.com/downloads.basho.com/riak/2.0/2.0.2/freebsd/9/riak-2.0.2-FreeBSD-amd64.tbz
```

When Riak is installed, a message is displayed with information about the installation and available documentation.

```
Thank you for installing Riak.

Riak has been installed in /usr/local owned by user:group riak:riak

The primary directories are:

    {platform_bin_dir, "/usr/local/sbin"}
    {platform_data_dir, "/var/db/riak"}
    {platform_etc_dir, "/usr/local/etc/riak"}
    {platform_lib_dir, "/usr/local/lib/riak"}
    {platform_log_dir, "/var/log/riak"}

These can be configured and changed in the platform_etc_dir/app.config.

Add /usr/local/sbin to your path to run the riak and riak-admin scripts directly.

Man pages are available for riak(1) and riak-admin(1)
```

If instead of this message, you receive an error during installation regarding OpenSSL, similar to this one:

```
Package dependency openssl-1.0.0_7 for /tmp/riak-2.0.2-FreeBSD-amd64.tbz not found!
```

Be sure that you've installed the required OpenSSL version from packages or the ports collection as described in the **Prerequisites and Dependencies** section.

## Installing From Source

Installing Riak from source on FreeBSD is a straightforward process which requires installation of more dependencies (such as Erlang) prior to building, and requires more time than a binary package installation.

That said, installing from source provides for greater flexibility with respect to configuration, data root locations, and more fine grained control over specific dependency versions.

### Prerequisites and Dependencies

When building and installing Riak from source, you might be required to install some prerequisite software before proceeding with the build.

If you do not currently have the following software installed, please install it with packages or the ports collection before proceeding.

* Erlang ([Installing Erlang][install source erlang])
* Curl
* Git
* OpenSSL (version 1.0.0_7)
* Python
* sudo

### Installation
First download the version you wish to install from [Basho downloads][downloads].

Next, unpack and build a release from source:

```bash
tar zxf <riak-x.x.x>
cd riak-x.x.x
gmake rel
```

Upon conclusion of the build, the `rel/riak` directory will contain a full Riak node environment, including configuration, data, and log directories:

```bash
bin               # Riak binaries
data              # Riak data and metadata
erts-5.9.2        # Erlang Run-Time System
etc               # Riak Configuration
lib               # Third party libraries
log               # Operational logs
releases          # Release information
```

If you'd prefer to build a development environment consisting of 4 nodes which can be run as a cluster on one machine, specify the `devrel` target instead of the `rel` target, like this:

```bash
gmake devrel
```

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
