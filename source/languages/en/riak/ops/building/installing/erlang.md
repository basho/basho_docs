---
title: Installing Erlang
project: riak
version: 0.10.0+
document: tutorial
toc: true
audience: beginner
keywords: [tutorial, installing, erlang]
prev: "[[Installing and Upgrading]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on Debian and Ubuntu]]"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-Erlang'
}
---

Pre-packaged versions of Riak include an Erlang installation. If you are building Riak from source, you will need to install [Basho's patched version of Erlang](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz). **If you do not use this version of Erlang, you will not be able to use Riak's [[security features|Authentication and Authorization]].**

<div class="note">
<div class="title">Note on Official Support</div>
Please note that only packaged Riak installs are officially support. Visit [[Installing and Upgrading|Installing and Upgrading]]for installing a supported Riak package.
</div>

## Prerequisites

#### Contents

* [[kerl|Installing Erlang#kerl-Prerequisites]]
* [[Debian/Ubuntu|Installing Erlang#Debian-Ubuntu-Prerequisites]]
* [[FreeBSD/Solaris|Installing Erlang#FreeBSD-Solaris-Prerequisites]]
* [[Mac OS X|Installing Erlang#Mac-OS-X-Prerequisites]]
* [[RHEL/CentOS|Installing Erlang#RHEL-CentOS-Prerequisites]]

To build and install Erlang you must have a GNU-compatible build system and these tools:

**Unpacking**

* [GNU unzip](http://www.gzip.org/) or a modern uncompressing utility.
* [GNU Tar](http://www.gnu.org/software/tar/) for working with GNU TAR archives.

**Building**

* [autoconf](http://www.gnu.org/software/autoconf/autoconf.html): generates configure scripts.
* [make](http://www.gnu.org/software/make/): generates executables and other non-source files of a program.
* [gcc](https://gcc.gnu.org/): for compiling C.
* [ncurses](http://www.gnu.org/software/ncurses/): for terminal-based interfaces.
* [OpenSSL](https://www.openssl.org/): toolkit that implements SSL and TSL protocols.
* [Java SE JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html): platform for deploying Java.


## kerl Prerequisites

[kerl](https://github.com/yrashk/kerl) is the quickest way to install different versions of Erlang on most systems.

Install kerl by running the following command:

```bash
curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
chmod a+x kerl
```

If you are using Mac OS X, FreeBSD, or Solaris, see the following sections for additional requirements before building with kerl.

Otherwise, continue with [[Installing with kerl|Installing Erlang#Installing-with-kerl]].

### Configuring kerl on FreeBSD/Solaris

Start by by creating a `~/.kerlrc` file:

```bash
touch ~/.kerlrc
```

Next add the following contents to your `~/.kerlrc` file:

```shell
KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads
                        --enable-kernel-poll --without-odbc --enable-darwin-64bit"
```

Then check for the presence of autoconf by running:

```shell
which autoconf
```
If this returns `autoconf not found`, install autoconf by running:

```shell
sudo pkg update
sudo pkg install autoconf
```

Once you've configured kerl and installed autoconf continue with [[Installing with kerl|Installing Erlang#Installing-with-kerl]].


### Configuring kerl on Mac OS X

To compile Erlang as 64-bit on Mac OS X you need to instruct kerl to pass the correct flags to the `configure` command.

Start by by creating a `~/.kerlrc` file:

```bash
touch ~/.kerlrc
```

Next add the following contents to your `~/.kerlrc` file:

```shell
KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads
                        --enable-kernel-poll --without-odbc --enable-darwin-64bit"
```

On OS X 10.9 (Mavericks) or later, you may need to install [autoconf](https://www.gnu.org/software/autoconf/).

Check for the presence of autoconf by running:

```shell
which autoconf
```

If this returns `autoconf not found`, install autoconf with:

With Homebrew:

```shell
brew install autoconf
```

Or with curl:

```shell
curl -O http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
tar zxvf autoconf-2.69.tar.gz
cd autoconf-2.69
./configure && make && sudo make install
```

Once you've configured kerl and installed autoconf continue with [[Installing with kerl|Installing Erlang#Installing-with-kerl]].



## Debian/Ubuntu Prerequisites

### Dependencies

To install the required dependencies run the following `apt-get` commands:

```bash
sudo apt-get update
sudo apt-get install build-essential autoconf libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev
```

### GUI Dependencies

If you're using a graphical environment and want to use Erlang's GUI utilities, you will need to install additional dependencies.

<div class="note">
<div class="title">Note on build output.</div>
These packages are not required for operation of a Riak node.
Notes in the build output about missing support for wxWidgets can be
safely ignored when installing Riak in a typical non-graphical server
environment.
</div>

To install packages for graphics support use the following `apt-get` command:

```bash
sudo apt-get install libwxbase2.8 libwxgtk2.8-dev libqt4-opengl-dev
```

### Next Steps

Once you've installed the prerequisites, continue with [[Installing on Debian/Ubuntu|Installing Erlang#Installing-on-Debian-Ubuntu]].



## FreeBSD/Solaris Prerequisites

### Dependencies

To install the required dependencies run the following `pkg` command:

```bash
sudo pkg update
sudo pkg install gcc autoconf gmake flex
```

### GUI Dependencies

If you're using a graphical environment and want to use Erlang's GUI utilities, you will need to install additional dependencies.

To install packages for graphics support use the following `pkg` command:

```bash
sudo pkg install wx28-gtk2-2.8.12_4
```

### Next Steps

Once you've installed the prerequisites, continue with [[Installing on FreeBSD/Solaris|Installing Erlang#Installing-on-FreeBSD-Solaris]].



## Mac OS X Prerequisites

* [XCode Developer Tools](http://developer.apple.com/) - Apple Software Development Tools.
* [Homebrew](http://brew.sh/) (*optional*) - Package Manager.

First install [XCode Developer Tools](http://developer.apple.com/). XCode is a set software development tools for developing on OS X.

We also recommend installing [Homebrew](http://brew.sh/), a package manager for OS X. Homebrew is not required to install Erlang and is optional.

Next, if you are running OS X 10.9 (Mavericks) or later, you may need to
install [autoconf](https://www.gnu.org/software/autoconf/). To check for
the presence of autoconf run:

```bash
which autoconf
```

If this returns `autoconf not found`, install autoconf with:

With Homebrew:

```bash
brew install autoconf
```

Or with curl:

```bash
curl -O http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
tar zxvf autoconf-2.69.tar.gz
cd autoconf-2.69
./configure && make && sudo make install
```

Once you've installed the prerequisites continue with [[Installing on Mac OS X|Installing Erlang#Installing-on-Mac-OS-X]].

## RHEL/CentOS Prerequisites

### Dependencies

To install the required dependencies run the following `yum` command:

```bash
sudo yum install gcc gcc-c++ glibc-devel make ncurses-devel openssl-devel autoconf java-1.8.0-openjdk-devel
```

### GUI Dependencies

If you're using a graphical environment and want to use Erlang's GUI utilities, you will need to install additional dependencies.

To install packages for graphics support use the following `blank` command:

```bash
sudo yum install wxBase.x86_64
```

### Next Steps

Once you've installed the prerequisites, continue with [[Installing on RHEL/CentOS|Installing Erlang#Installing-on-RHEL-CentOS]].



## Installation

* [Installing with kerl](#Installing-with-kerl)
* [Installing on Debian/Ubuntu](#Installing-on-Debian-Ubuntu)
* [Installing on FreeBSD/Solaris](#Installing-on-FreeBSD-Solaris)
* [Installing on Mac OS X](#Installing-on-Mac-OS-X)
* [Installing on RHEL/CentOS](#Installing-on-RHEL-CentOS)

## Installing with kerl

First make sure you have installed the necessary dependencies and prerequisites found in [[kerl Prerequisites|Installing Erlang#kerl-Prerequisites]].

With [kerl](https://github.com/yrashk/kerl)  installed, you can install Basho's recommended version of
Erlang [from Github](https://github.com/basho/otp) using the following
command:

```bash
./kerl build git git://github.com/basho/otp.git OTP_R16B02_basho8 R16B02-basho8
```

This builds the Erlang distribution and performs all of the steps
required to manually install Erlang for you.

After Erlang is successfully built, you can install the build as follows:

```bash
./kerl install R16B02-basho8 ~/erlang/R16B02-basho8
. ~/erlang/R16B02-basho8/activate
```

The last line activates the Erlang build that was just installed into
`~/erlang/R16B02-basho8`.

> See the kerl [README](https://github.com/yrashk/kerl) for more details on the available commands.

Confirm Erlang installed to the correct location:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```


## Installing on Debian/Ubuntu

First make sure you have installed the necessary dependencies found in [[Debian/Ubuntu Prerequisites|Installing Erlang#Debian-Ubuntu-Prerequisites]].

Next download [Basho's patched version of Erlang](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz).

Using `wget`:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho8.tar.gz
```

Next `cd` into the unpacked directory, build and install Erlang with:
 
```bash
cd OTP_R16B02_basho8
./otp_build autoconf
./configure && make && sudo make install
```

Confirm Erlang installed to the correct location:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```

## Installing on FreeBSD/Solaris

First make sure you installed the necessary dependencies in [[FreeBSD/Solaris Prerequisites|Installing Erlang#FreeBSD-Solaris-Prerequisites]].

Next download [Basho's patched version of Erlang](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz):

```bash
ftp http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho8.tar.gz
```

Next `cd` into the unpacked directory, build and install Erlang with:
 
```bash
cd OTP_R16B02_basho8
./otp_build autoconf
./configure && gmake && sudo gmake install
```

Confirm Erlang installed to the correct location by running:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```


## Installing on Mac OS X

First make sure you have installed the necessary dependencies found in [[Mac OS X Prerequisites|Installing Erlang#Mac-OS-X-Prerequisites]].

You can install Erlang in several ways on OS X: 

* [[From Source|Installing Erlang#Installing-on-Mac-OS-X-from-Source]]
* [[Homebrew|Installing Erlang#Installing-on-Mac-OS-X-with-Homebrew]]
* [[MacPorts|Installing Erlang#Installing-on-Mac-OS-X-with-MacPorts]]

## Installing on Mac OS X from Source

Next download [Basho's patched version of Erlang](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz):

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho8.tar.gz
```

Follow the steps below to configure Erlang for your operating system.

#### Configuring Erlang on Mavericks (OS X 10.9), Mountain Lion (OS X 10.8), and Lion (OS X 10.7)

If you're on Mavericks (OS X 10.9), Mountain Lion (OS X 10.8), or Lion
(OS X 10.7) you can use LLVM (the default) or GCC to compile Erlang.

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

#### Configuring Erlang on Snow Leopard (OS X 10.6)

If you're on Snow Leopard (OS X 10.6) or Leopard (OS X 10.5) with an
Intel processor:

```bash
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll  --enable-darwin-64bit
```

#### Configuring Erlang on older versions of OS X

If you're on a non-Intel processor or older version of OS X:

```bash
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll
```

After you've configured your system `cd` into the unpacked directory, build and install Erlang with:
 
```bash
cd OTP_R16B02_basho8
./otp_build autoconf
./configure && make && sudo make install
```

Confirm Erlang installed to the correct location by running:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```

## Installing on Mac OS X with Homebrew

To install Erlang with Homebrew, use this command:

```bash
brew install erlang
```

Confirm Erlang installed to the correct location by running:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```

## Installing on Mac OS X with MacPorts

Installing with MacPorts:

```bash
port install erlang +ssl
```

Confirm Erlang installed to the correct location by running:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```

## Installing on RHEL/CentOS

First make sure you have installed the necessary dependencies and prerequisites found in [[RHEL/CentOS Prerequisites|Installing Erlang#RHEL-CentOS-Prerequisites]].

Using `wget`:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho8.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho8.tar.gz
```

Next `cd` into the unpacked directory, build and install Erlang with:

```bash
cd OTP_R16B02_basho8
./otp_build autoconf
./configure && make && sudo make install
```

<div class="note">
<div class="title">Note for RHEL6/CentOS6</div>
In certain versions of RHEL6 and CentO6 the `openSSL-devel` package
ships with Elliptical Curve Cryptography partially disabled. To
communicate this to Erlang and prevent compile- and run-time errors, the
environment variable `CFLAGS="-DOPENSSL_NO_EC=1"` needs to be added to
Erlang's `./configure` call.

The full `make` invocation then becomes

```bash
CFLAGS="-DOPENSSL_NO_EC=1" ./configure && make && sudo make install
```
</div>

Confirm Erlang installed to the correct location:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```
