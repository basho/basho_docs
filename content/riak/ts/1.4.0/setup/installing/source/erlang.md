---
title: "Installing Erlang"
description: "Install Erlang TS"
menu:
  riak_ts-1.4.0:
    name: "Erlang"
    identifier: "installing_erlang"
    weight: 100
    parent: "installing_from_source"
project: "riak_ts"
project_version: "1.4.0"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing/source/erlang"]
    - ["1.4.0+",      "setup/installing/source/erlang"]
aliases:
    - /riakts/1.4.0/installing/source/erlang/
    - /riakts/1.4.0/setup/installing/source/erlang/
    - /riak/ts/1.4.0/installing/source/erlang/
---


[autoconf]: http://www.gnu.org/software/autoconf/autoconf.html
[basho erlang]: https://github.com/basho/otp/archive/OTP_R16B02_basho10.tar.gz
[basho erlang repo]: https://github.com/basho/otp
[gcc]: https://gcc.gnu.org/
[GNU tar]: http://www.gnu.org/software/tar/
[GNU unzip]: http://www.gzip.org/
[Homebrew link]: http://brew.sh/
[Java SE JDK]: http://www.oracle.com/technetwork/java/javase/downloads/index.html
[kerl]: https://github.com/yrashk/kerl
[make]: http://www.gnu.org/software/make/
[ncurses]: http://www.gnu.org/software/ncurses/
[OpenSSL]: https://www.openssl.org/
[source]: {{<baseurl>}}riak/ts/1.4.0/setup/installing/source/
[XCode Developer Tools]: https://developer.apple.com/xcode/downloads/


{{% note %}}
Pre-packaged versions of Riak TS include an Erlang installation.
{{% /note %}} 

If you are building Riak TS from [source], you will need to install [Basho's patched version of Erlang][basho erlang]. If you do not use this version of Erlang, you will not be able to use Riak TS's security features.

{{% note title="Note on Official Support" %}}
Only packaged Riak TS installs are officially supported.
{{% /note %}}


## Prerequisites

* [kerl](#kerl-prerequisites)
* [Debian/Ubuntu](#debian-ubuntu-prerequisites)
* [Mac OS X](#mac-os-x-prerequisites)
* [RHEL/CentOS](#rhel-centos-prerequisites)

To build and install Erlang you must have a GNU-compatible build system and these tools:

**Unpacking**

* [GNU unzip] or a modern uncompressing utility.
* [GNU Tar] for working with GNU TAR archives.

**Building**

* [autoconf]: generates configure scripts.
* [make]: generates executables and other non-source files of a program.
* [gcc]: for compiling C.
* [ncurses]: for terminal-based interfaces.
* [OpenSSL]: toolkit that implements SSL and TSL protocols.
* [Java SE JDK]: platform for deploying Java.


### kerl Prerequisites

[kerl] is the quickest way to install different versions of Erlang on most systems.

Install kerl by running the following command:

```bash
curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
chmod a+x kerl
```

If you are using Mac OS X, see the following section for additional requirements before building with kerl.

Otherwise, continue with [Installing with kerl](#installing-with-kerl).


#### Configuring kerl on Mac OS X

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

On OS X 10.9 (Mavericks) or later, you may need to install [autoconf]. You can check for the presence of autoconf by running:

```shell
which autoconf
```

If this returns `autoconf not found`, install autoconf with:

[Homebrew][Homebrew link]:

```shell
brew install autoconf
```

Or curl:

```shell
curl -O http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
tar zxvf autoconf-2.69.tar.gz
cd autoconf-2.69
./configure && make && sudo make install
```

Once you've configured kerl and installed autoconf continue with [Installing with kerl](#installing-with-kerl).


### Debian/Ubuntu Prerequisites

#### Dependencies

To install the required dependencies run the following `apt-get` commands:

```bash
sudo apt-get update
sudo apt-get install build-essential autoconf libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev git
```


#### GUI Dependencies

If you're using a graphical environment and want to use Erlang's GUI utilities, you will need to install additional dependencies.

{{% note title="Note on build output" %}}
These packages are not required for operation of a Riak node.
Notes in the build output about missing support for wxWidgets can be
safely ignored when installing Riak in a typical non-graphical server
environment.
{{% /note %}}

To install packages for graphics support use the following `apt-get` command:

```bash
sudo apt-get install libwxbase2.8 libwxgtk2.8-dev libqt4-opengl-dev
```


#### Next Steps

Once you've installed the prerequisites, continue with [Installing on Debian/Ubuntu](#installing-on-debian-ubuntu).


### Mac OS X Prerequisites

* [XCode Developer Tools] - Apple Software Development Tools.
* [Homebrew][Homebrew link] *(optional)* - Package Manager.

First install [XCode Developer Tools]. XCode is a set software development tools for developing on OS X.

We also recommend installing [Homebrew][Homebrew link], a package manager for OS X. Homebrew is not required to install Erlang.

Next, if you are running OS X 10.9 (Mavericks) or later, you may need to
install [autoconf]. To check for the presence of autoconf run:

```bash
which autoconf
```

If this returns `autoconf not found`, install autoconf with:

[Homebrew][Homebrew link]:

```bash
brew install autoconf
```

Or curl:

```bash
curl -O http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
tar zxvf autoconf-2.69.tar.gz
cd autoconf-2.69
./configure && make && sudo make install
```

Once you've installed the prerequisites continue with [Installing on Mac OS X](#installing-on-mac-os-x).


### RHEL/CentOS Prerequisites

#### Dependencies

To install the required dependencies run the following `yum` command:

```bash
sudo yum install gcc gcc-c++ glibc-devel make ncurses-devel openssl-devel autoconf java-1.8.0-openjdk-devel git
```


#### GUI Dependencies

If you're using a graphical environment and want to use Erlang's GUI utilities, you will need to install additional dependencies.

To install packages for graphics support use the following `blank` command:

```bash
sudo yum install wxBase.x86_64
```

#### Next Steps

Once you've installed the prerequisites, continue with [Installing on RHEL/CentOS](#installing-on-rhel-centos).


## Installation

* [Installing with kerl](#installing-with-kerl)
* [Installing on Debian/Ubuntu](#installing-on-debian-ubuntu)
* [Installing on Mac OS X](#installing-on-mac-os-x)
* [Installing on RHEL/CentOS](#installing-on-rhel-centos)

## Installing with kerl

First make sure you have installed the necessary dependencies and prerequisites found in [kerl Prerequisites](#kerl-prerequisites).

With [kerl] installed, you can install Basho's recommended version of
Erlang [from Github][basho erlang repo] using the following
command:

```bash
./kerl build git git://github.com/basho/otp.git OTP_R16B02_basho10 R16B02-basho10
```

This builds the Erlang distribution and performs all of the steps
required to manually install Erlang for you.

After Erlang is successfully built, you can install the build as follows:

```bash
./kerl install R16B02-basho10 ~/erlang/R16B02-basho10
. ~/erlang/R16B02-basho10/activate
```

The last line activates the Erlang build that was just installed into
`~/erlang/R16B02-basho10`.

>See the kerl [README][kerl] for more details on the available commands.

Confirm Erlang installed to the correct location:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```


## Installing on Debian/Ubuntu

First make sure you have installed the necessary dependencies found in [Debian/Ubuntu Prerequisites](#debian-ubuntu-prerequisites).

Next download [Basho's patched version of Erlang][basho erlang].

Using `wget`:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho10.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho10.tar.gz
```

Next `cd` into the unpacked directory, build and install Erlang with:

```bash
cd OTP_R16B02_basho10
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


## Installing on Mac OS X

First make sure you have installed the necessary dependencies found in [Mac OS X Prerequisites](#mac-os-x-prerequisites).

You can install Erlang in several ways on OS X:

* [From Source](#installing-on-mac-os-x-from-source)
* [Homebrew](#installing-on-mac-os-x-with-homebrew)
* [MacPorts](#installing-on-mac-os-x-with-macports)

### Installing on Mac OS X from Source

Next download [Basho's patched version of Erlang][basho erlang]:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho10.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho10.tar.gz
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
cd OTP_R16B02_basho10
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


### Installing on Mac OS X with Homebrew

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


### Installing on Mac OS X with MacPorts

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

First make sure you have installed the necessary dependencies and prerequisites found in [RHEL/CentOS Prerequisites](#rhel-centos-prerequisites).

Using `wget`:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho10.tar.gz
```

Then unpack the download with:

```bash
tar zxvf otp_src_R16B02-basho10.tar.gz
```

Next `cd` into the unpacked directory, build and install Erlang with:

```bash
cd OTP_R16B02_basho10
./otp_build autoconf
./configure && make && sudo make install
```

> **Note for RHEL6/CentOS6**
>
> In certain versions of RHEL6 and CentO6 the `openSSL-devel` package
ships with Elliptical Curve Cryptography partially disabled. To
communicate this to Erlang and prevent compile- and run-time errors, the
environment variable `CFLAGS="-DOPENSSL_NO_EC=1"` needs to be added to
Erlang's `./configure` call.
>
> The full `make` invocation then becomes
>
> ```bash
CFLAGS="-DOPENSSL_NO_EC=1" ./configure && make && sudo make install
```

Confirm Erlang installed to the correct location:

```bash
which erl
```

And start Erlang from your terminal with:

```bash
erl
```
