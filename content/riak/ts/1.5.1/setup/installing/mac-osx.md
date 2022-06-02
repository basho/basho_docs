---
title: "Installing on Mac OS X"
description: "Installing on Mac OS X"
menu:
  riak_ts-1.5.1:
    name: "Mac OS X"
    identifier: "installing_on_mac_os_x"
    weight: 202
    parent: "installing"
project: "riak_ts"
project_version: "1.5.1"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing/mac-osx"]
    - ["1.4.0+",      "setup/installing/mac-osx"]
aliases:
    - /riakts/1.5.1/installing/mac-osx/
    - /riakts/1.5.1/setup/installing/mac-osx/
    - /riak/ts/1.5.1/installing/mac-osx/
---


[download]: {{<baseurl>}}riak/ts/1.5.1/downloads/
[openfileslimit]: {{<baseurl>}}riak/kv/2.2.0/using/performance/open-files-limit
[planning]: {{<baseurl>}}riak/ts/1.5.1/using/planning

Riak TS can be installed on Mac OS X systems using a binary
package available [here][download].

{{% note %}}
Mac OS X is only supported for developing with Riak TS and NOT for general operations.
{{% /note %}}


## Dependencies

### `ulimit`

OS X gives you a very small limit on open file handles. Even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][openfileslimit] for more information about changing the limit.


## Install Riak TS

To install Riak TS on your Mac, [download] the package and then run:

```bash
tar zxvf riak-ts-1.5.1-OSX-x86_64.tar.gz
cd riak-ts-1.5.1
```


## Start your Riak TS node

Once you've installed Riak TS, start it on your node:

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
