---
title: "Installing on Mac OS X"
description: "Installing on Mac OS X"
menu:
  riak_ts-1.3.0:
    name: "Mac OS X"
    identifier: "installing_on_mac_os_x"
    weight: 202
    parent: "installing"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/installing/mac-osx/
canonical_link: "https://docs.basho.com/riak/ts/latest/installing/mac-osx"
---


[download]: ../../downloads/
[openfileslimit]: /riak/kv/2.1.4/using/performance/open-files-limit
[planning]: ../../using/planning

Riak TS can be installed on Mac OS X systems using a binary
package available [here][download].

>**Important**
>
>Mac OS X is only supported for developing with Riak TS and NOT for general operations.


## Dependencies

### `ulimit`

OS X gives you a very small limit on open file handles. Even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][openfileslimit] for more information about changing the limit.


## Install Riak TS

To install Riak TS on your Mac, [download] the package and then run:

```bash
tar zxvf riak-ts-1.3.0-OSX-x86_64.tar
cd riak-ts-1.3.0
```


## Activate Riak TS node

Once you've installed Riak TS, start it on your node:

```bash
riak start
```


## Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].