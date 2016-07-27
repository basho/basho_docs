---
title: "Installing from Source"
description: "Install TS from Source"
menu:
  riak_ts-1.4.0:
    name: "Source"
    identifier: "installing_from_source"
    weight: 300
    parent: "installing"
project: "riak_ts"
project_version: "1.4.0"
toc: true
canonical_link: "https://docs.basho.com/riak/ts/latest/installing/source/"
---


[download]: ../../downloads/
[Erlang]: http://www.erlang.org/
[GCC]: https://gcc.gnu.org/
[Git]: https://git-scm.com/
[install erlang]: ../source/erlang
[planning]: ../../using/planning/
[Riak TS GitHub respository]: https://github.com/basho/riak/tree/riak_ts-1.4.0


If there isn't a package for your OS or you are interested in contributing back to the project, you can install Riak TS from source!


## Dependencies

### Erlang

To install Riak TS, you will need to have [Erlang] installed. We strongly recommend using Basho's patched version of Erlang to install Riak TS. All of the patches in this version have been incorporated into later versions of the official Erlang/OTP release.

See [Installing Erlang][install erlang] for instructions.


### Git

Riak TS depends on source code located in multiple Git repositories. Install [Git] on the target system before attempting the build.


### GCC

Riak TS will not compile with Clang. Please make sure your default C/C++
compiler is [GCC].


## Installation

The following instructions generate a complete, self-contained build of
Riak TS in `»Unpacked or Cloned Source«/rel/riak`.


### Installing from source package

Download the Riak TS source package from [Downloads][download] and build:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/ts/1.3/1.4.0/riak_ts-1.4.0.tar.gz
tar zxvf riak_ts-1.4.0.tar.gz
cd riak_ts-1.4.0
make locked-deps
make rel
```


### Installing from GitHub

To clone and build Riak TS from source, clone the repository using [Git] and build:

```bash
git clone --branch riak_ts-1.4.0 https://github.com/basho/riak.git
cd riak_ts-1.4.0
make locked-deps
make rel
```


## Verify Your Installation

You can verify that Riak TS was successfully installed by running riak shell:

```bash
./riak-shell
```

Verify your connection by running `show_connection`. You should see a reply like this one:

```
riak_shell is connected to: 'dev1@127.0.0.1' on port 8087
```


## Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].