Riak should be installed from source if you are building on a platform for which a package does not exist or you are interested in contributing to Riak.

## Dependencies
Riak requires [[Erlang|http://www.erlang.org/]] R15B01 or later. If you do not have Erlang already installed, see [[Installing Erlang]]. Don't worry, it's easy!

<div class='note'>Riak will not compile with Clang. Please make sure your default C/C++ compiler is GCC.</div>

## Installation
The following instructions generate a complete, self-contained build of Riak in `$RIAK/rel/riak` where “`$RIAK`” is the location of the unpacked or cloned source.

### Installing from source package
Download the Riak source package from the [[Download Center|http://basho.com/resources/downloads/]] and build:

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/riak-1.2.0.tar.gz
tar zxvf riak-1.2.0.tar.gz
cd riak-1.2.0
make rel
```

### Installing from GitHub
The [[Riak Github repository|http://github.com/basho/riak]] has much more information on building and installing Riak from source. To clone and and build Riak from source, follow these steps:

Clone the repository using [[Git|http://git-scm.com/]] and build:

```bash
git clone git://github.com/basho/riak.git
cd riak
make rel
```

## Platform Specific Instructions
For instructions about specific platforms, see:

  * [[Installing on Debian and Ubuntu]]
  * [[Installing on Mac OS X]]
  * [[Installing on RHEL and CentOS]]
  * [[Installing on SUSE]]

If you are running Riak on a platform not in the list above and need some help getting it up and running, join The Riak Mailing List and inquire about it there. We are happy to help you get up and running with Riak.

### Windows
Riak is not currently supported on Microsoft Windows.
