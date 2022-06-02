---
title: "Installing from Source"
description: "Install TS from Source"
menu:
  riak_ts-1.5.0:
    name: "Source"
    identifier: "installing_from_source"
    weight: 300
    parent: "installing"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing/source"]
    - ["1.4.0+",      "setup/installing/source"]
aliases:
    - /riakts/1.5.0/installing/source/
    - /riakts/1.5.0/setup/installing/source/
    - /riak/ts/1.5.0/installing/source/
---


[download]: {{<baseurl>}}riak/ts/1.5.0/downloads/
[Erlang]: http://www.erlang.org/
[GCC]: https://gcc.gnu.org/
[Git]: https://git-scm.com/
[install erlang]: {{<baseurl>}}riak/ts/1.5.0/setup/installing/source/erlang
[planning]: {{<baseurl>}}riak/ts/1.5.0/using/planning/
[Riak TS GitHub repository]: https://github.com/basho/riak/tree/riak_ts-1.5.0


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
curl -O http://s3.amazonaws.com/downloads.basho.com/riak/ts/1.3/1.5.0/riak_ts-1.5.0.tar.gz
tar zxvf riak_ts-1.5.0.tar.gz
cd riak_ts-1.5.0
make locked-deps
make rel
```


### Installing from GitHub

To clone and build Riak TS from source, clone the repository using [Git] and build:

```bash
git clone --branch riak_ts-1.5.0 https://github.com/basho/riak.git
cd riak_ts-1.5.0
make locked-deps
make rel
```


## Verify Your Installation

You can verify that Riak TS was successfully installed by running `riak-shell`:

```bash
sudo riak-shell
```

Verify your connection by running `show_connection`:

```
riak-shell>show_connection;
```

You should see a reply like this one:

```
riak_shell is connected to: 'dev1@127.0.0.1' on port 8087
```

Make sure to exit the riak shell when you are done by running `q;`.


## Start your Riak TS node

Once you've successfully installed Riak TS, start it on your node:

```bash
riak start
```

## Verify Riak TS is running

You can verify that Riak TS is started and ready to use by pinging it.

```bash
riak ping
```

If Riak TS has started, you will receive a `pong` response. If it has not started, you will receive an error. 


## Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].
