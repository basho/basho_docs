Riak 1.2 requires [[Erlang|http://erlang.org/]] R15B01 or later. Riak versions prior to 1.0 will not function on the R14B02 or later. Riak versions prior to 0.12 will not function on the R14 series of Erlang. For Erlang to build and install, you must have a GNU-compatible build system, and the development bindings of ncurses and openssl.

## Note
The Riak binary packages for Debian and Ubuntu, Mac OS X, and RHEL and CentOS do not require that you build Erlang from source. **You will have to download and install Erlang, however, if you are planning on completing [[The Riak Fast Track]].**

## Install using kerl
kerl is a simple shell script that allows installing different Erlang versions with only two commands. It is probably the easiest way to install Erlang from source on a system. Installing kerl is as simple as running the following command

```bash
curl -O https://raw.github.com/spawngrid/kerl/master/kerl; chmod a+x kerl
```

In order for kerl to compile erlang as 64-bit on Mac OS X, you will need to configure kerl to pass the correct flags to the configure command. The easiest way to do it is by creating a file `~/.kerlrc` with the following contents:

```text
KERL_CONFIGURE_OPTIONS="--enable-hipe --enable-smp-support --enable-threads
                        --enable-kernel-poll  --enable-darwin-64bit"
```

Building with kerl on GNU/Linux has the same prerequisites that building from source does.

Then you can just build the Erlang release of your choice, as of current, you will need R15B01:

```bash
./kerl build R15B01 r15b01
```
This installs Erlang and does all the steps required to manually install Erlang for you.

When successfully built you can install it using:

```bash
./kerl install r15b01 ~/erlang/r15b01
. ~/erlang/r15b01/activate
```

The last line activates the Erlang build that was just installed into `/opt/erlang/r15b01`. See the [[kerl readme|https://github.com/spawngrid/kerl]] for more details on the available commands.

If you prefer to install completely manually, the following will show you how.

## Installing on GNU/Linux
Most distributions do not have the most recent Erlang release available, **so you will need to install from source**.

First, make sure you have a compatible build system and the `ncurses` and `openssl` development libraries installed. On Debian/Ubuntu use this command:

```bash
sudo apt-get install build-essential libncurses5-dev openssl libssl-dev
```

On RHEL/CentOS use this command:

```bash
sudo yum install gcc glibc-devel make ncurses-devel openssl-devel autoconf
```

Next, download, build and install Erlang:

```bash
wget http://erlang.org/download/otp_src_R15B01.tar.gz
tar zxvf otp_src_R15B01.tar.gz
cd otp_src_R15B01
./configure && make && sudo make install
```

## Installing on Mac OS/X
You can install Erlang in several ways on OS/X: from source, with Homebrew, or with MacPorts.

### Source
To build from source, you must have XCode tools installed from the CD that came with your Mac or from the Apple [[Developer website|http://developer.apple.com/]].

First, download and unpack the source:

```bash
curl -O http://erlang.org/download/otp_src_R15B01.tar.gz
tar zxvf otp_src_R15B01.tar.gz
cd otp_src_R15B01
```

Next, configure Erlang.

**Lion (OS/X 10.7)**  
If you're on Lion (OS/X 10.7) you can use LLVM, the default, or GCC to compile Erlang.

Using LLVM:

```bash
CFLAGS=-O0 ./configure --enable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll --enable-darwin-64bit
```

If you prefer GCC:

```bash
CC=gcc-4.2 CPPFLAGS='-DNDEBUG' MAKEFLAGS='-j 3' \
./configure --enable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll --enable-darwin-64bit
```

**Snow Leopard (OS/X 10.6)**  
If you're on Snow Leopard (OS/X 10.6) or Leopard (OS/X 10.5) with an Intel processor:

```bash
./configure --enable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll  --enable-darwin-64bit
```

If you're on a non-Intel processor or older version of OS/X:

```bash
./configure --enable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll
```

Now build and install:

```bash
make && sudo make install
```

You will be prompted for your sudo password.

### Homebrew
If you want to install Riak with Homebrew, simply follow the [[instructions here|https://help.basho.com/Installing-on-Mac-OS-X.html]] and Erlang will be installed automatically. To install it separately:

```bash
brew install erlang
```

<div class='note'><div class='title'>Erlang Suport</div>Riak does not currently officially support Erlang R15 (what Homebrew will install). Therefore, Riak will not build from source using a Homebrew installation of Erlang without modification of Riak's configuration files.</div>

### MacPorts
Installing with MacPorts is easy:

```bash
port install erlang +ssl
```
