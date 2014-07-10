---
title: Installing Erlang
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, erlang]
prev: "[[Installing and Upgrading]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on Debian and Ubuntu]]"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-Erlang'
}
---

An [Erlang](http://erlang.org) installation is required to run Riak. We strongly recommend using Basho's patched version of Erlang to install Riak 2.0. The tar file for this version can be downloaded [here](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho5.tar.gz). **If you do not use this version, you will not be able to use Riak's [[security features|Authentication and Authorization]]**.

For Erlang to build and install, you must have a GNU-compatible build system, and the development bindings of [ncurses](http://www.gnu.org/software/ncurses/) and [OpenSSL](https://www.openssl.org/).

<div class="note">
<div class="title">Erlang Version Note</div>
The Riak binary packages for Debian and Ubuntu, Mac OS X, and RHEL and CentOS include an Erlang distribution, and do not require that you build Erlang from source. However, <strong>you must download and install Erlang if you are planning on completing the [[Five-Minute Install]]</strong>.
</div>

## Install using kerl

You can install different Erlang versions in a simple manner using the [kerl](https://github.com/spawngrid/kerl) script. This is probably the easiest way to install Erlang from source on a system, and typically only requires a few commands to do so. Install kerl by running the following commands:

```curl
curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
chmod a+x kerl
```

Once kerl is installed, you can install Basho's recommended version of Erlang [from Github](https://github.com/basho/otp) using the following command:

```bash
kerl build git git://github.com/basho/otp.git OTP_R16B02_basho5 R16B02-basho5
```

<div class="note">
<div class="title">Note on building on Mac OS X, FreeBSD, or Solaris</div>
If you are building Basho's recommended version of Erlang using kerl on Mac OS X, FreeBSD, or Solaris, consult the corresponding sections below for a list of prerequisites that should be fulfilled <em>prior</em> to building with kerl.
</div>

This builds the Erlang distribution and performs all of the steps required to manually install Erlang for you.

When successfully built, you can install the build as follows:

```bash
./kerl install R16B02-basho5 ~/erlang/R16B02-basho5
. ~/erlang/R16B02-basho5/activate
```

The last line activates the Erlang build that was just installed into `~/erlang/R16B02-basho5`. See the kerl [README](https://github.com/spawngrid/kerl) for more details on the available commands.

If you prefer to install Erlang manually from the source code, the following section will show you how.

### Mac OS X Prerequisites

To compile Erlang as 64-bit on Mac OS X, prior to running the `build` command shown above you need to instruct kerl to pass the correct flags to the `configure` command. The easiest way to do this is by creating a `~/.kerlrc` file with the following contents:

```bash
KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads
                        --enable-kernel-poll --without-odbc --enable-darwin-64bit"
```

On Mac OS X, you will also need to install [autoconf](https://www.gnu.org/software/autoconf/). The simplest way to do so is via Homebrew:

```bash
brew install autoconf
```

### FreeBSD/Solaris Prerequisites

When building Erlang using kerl on a FreeBSD/Solaris system (including SmartOS), HIPE should be disabled on these platforms as well with the `--disable-hipe` option shown in the **Mac OS X Prerequisites** section above.

## Installing on GNU/Linux

Most GNU/Linux distributions do not make the most recent Erlang release available, so you will need to install <em>from source</em>.

First, make sure you have a compatible build system and that you have installed the necessary dependencies.

### Debian/Ubuntu Dependencies

Use this command to install the required dependency packages:

```bash
sudo apt-get install build-essential libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev
```

If you'll be using a graphical environment (such as for development purposes) and would like to use Erlang's GUI utilities, then you'll need to install some additional dependencies.

<div class="note">
<div class="title">Note on build output</div>
Note that these packages are not required for operation of a Riak node and notes in the build output about missing support for wxWidgets can be safely ignored when installing Riak in a typical non-graphical server environment.
</div>

To install packages for graphics support, use this command:

```bash
sudo apt-get install libwxbase2.8 libwxgtk2.8-dev libqt4-opengl-dev
```

### RHEL/CentOS Dependencies

Use this command to install the required dependency packages:

```bash
sudo yum install gcc glibc-devel make ncurses-devel openssl-devel autoconf
```

### Erlang

Next, download, build, and install Erlang:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho5.tar.gz
tar zxvf otp_src_R16B02-basho5.tar.gz
cd otp_src_R16B02-basho5
./configure && make && sudo make install
```

## Installing on Mac OS X

You can install Erlang in several ways on OS X: from source, with Homebrew, or with MacPorts.

### Source

To build from source, you must have Xcode tools installed from the Apple [Developer website](http://developer.apple.com/).

First, download and unpack the source:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho5.tar.gz
tar zxvf otp_src_R16B02-basho5.tar.gz
cd otp_src_R16B02-basho5
```

Next, configure Erlang.

#### Mavericks (OS X 10.9), Mountain Lion (OS X 10.8), and Lion (OS X 10.7)

If you're on Mavericks (OS X 10.9), Mountain Lion (OS X 10.8), or Lion (OS X 10.7) you can use LLVM (the default) or GCC to compile Erlang.

Using LLVM:

```bash
CFLAGS=-O0 ./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll --enable-darwin-64bit
```

Or if you prefer GCC:

```bash
CC=gcc-4.2 CPPFLAGS='-DNDEBUG' MAKEFLAGS='-j 3' \
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll --enable-darwin-64bit
```

#### Snow Leopard (OS X 10.6)

If you're on Snow Leopard (OS X 10.6) or Leopard (OS X 10.5) with an Intel processor:

```bash
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll  --enable-darwin-64bit
```

If you're on a non-Intel processor or older version of OS X:

```bash
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll
```

Now build and install:

```bash
make && sudo make install
```

You will be prompted for your sudo password.

### Homebrew

If you want to install Riak with Homebrew, follow the [[Mac OS X Installation documentation|Installing-on-Mac-OS-X]], and Erlang will be installed automatically.

To install Erlang separately with Homebrew, use this command:

```bash
brew install erlang
```

### MacPorts
Installing with MacPorts is easy:

```bash
port install erlang +ssl
```
