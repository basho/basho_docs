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

1. Add the Riak repository:
   * Run `echo https://files.tiot.jp/alpine/v3.16/main >> /etc/apk/repositories`
2. Remove the local dev repository if needed:
   * Edit the file `/etc/apk/repositories` to remove `/home/dev/packages/main` if it is present.
3. Download and install the Riak repository public key:
   * Run `wget http://files.tiot.jp/alpine/alpine@tiot.jp.rsa.pub -O /etc/apk/keys/alpine@tiot.jp.rsa.pub`
4. Update `apk`:
   * Run `apk update`
5. Install Riak:
   * Run `apk add riak`

## Next Steps

Now that Riak is installed, check out [Verifying a Riak Installation][install verify].
