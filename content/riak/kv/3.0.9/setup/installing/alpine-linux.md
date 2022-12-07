---
title_supertext: "Installing on"
title: "Alpine Linux"
description: "installing Riak on Alpine Linux"
project: "riak_kv"
project_version: 3.0.9
menu:
  riak_kv-3.0.9:
    name: "Alpine Linux"
    identifier: "installing_alpine_linux"
    weight: 301
    parent: "installing"
toc: true
since: 3.0.9
version_history:
  in: "3.0.9+"
aliases:
  - /riak/3.0.10/ops/building/installing/installing-on-alpine-linux
  - /riak/kv/3.0.10/ops/building/installing/installing-on-alpine-linux
  - /riak/3.0.10/installing/alpine-linux/
  - /riak/kv/3.0.10/installing/alpine-linux/
---

[security index]: {{<baseurl>}}riak/kv/3.0.10/using/security/
[install source erlang]: {{<baseurl>}}riak/kv/3.0.10/setup/installing/source/erlang
[install verify]: {{<baseurl>}}riak/kv/3.0.10/setup/installing/verify

Riak KV can be installed on Alpine Linux using a binary
package from the Riak repository.

The following steps have been tested to work with Riak KV on:

* Alpine Linux 3.14

## Riak 64-bit Installation

To install Riak on Alpine Linux:

1. Add the Alpine Edge repository:
   * Run `echo https://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories`
2. Update your list of packages:
   * Run `apk update`
3. Install Riak:
   * Run `apk add riak`

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
