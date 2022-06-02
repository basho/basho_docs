---
title: "Installing on Mac OS X"
description: "Installing on Mac OS X"
menu:
  riak_ts-1.2.0:
    name: "Mac OS X"
    identifier: "installing_on_mac_os_x"
    weight: 202
    parent: "installing"
project: "riak_ts"
project_version: "1.2.0"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing/mac-osx"]
    - ["1.4.0+",      "setup/installing/mac-osx"]
aliases:
    - /riakts/1.2.0/installing/mac-osx/
---

[concept aae]: {{<baseurl>}}riak/kv/2.1.3/learn/concepts/active-anti-entropy
[perf open files]: {{<baseurl>}}riak/kv/2.1.3/using/performance/open-files-limit
[planning]: ../../using/planning

Riak TS can be installed on Mac OS X systems using a binary
package available through ZenDesk.

{{% note %}}
Mac OS X is only supported for developing with Riak TS and NOT for general operations.
{{% /note %}}

Check your email for the link to the download in ZenDesk.

## Dependencies

### `ulimit`

OS X gives you a very small limit on open file handles. Even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][perf open files] for more information about changing the limit.


## Install Riak TS

To install Riak TS on your Mac, download the package from ZenDesk and then run:

```bash
tar zxvf riak-ts-{{VERSION}}-OSX-x86_64.tar.gz
cd riak-ts-{{VERSION}}
```

Then confirm that [AAE][concept aae] is turned off. To do this, check etc/riak.conf for the following: `anti_entropy = passive`.


## Activate Riak TS node

Once you've installed Riak TS, start it on your node:

```bash
riak start
```


## Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].
