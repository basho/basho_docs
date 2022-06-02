---
title: "Installing on Debian and Ubuntu"
description: "Installing on Debian and Ubuntu"
menu:
  riak_ts-1.3.1:
    name: "Debian and Ubuntu"
    identifier: "installing_on_debian_ubuntu"
    weight: 201
    parent: "installing"
project: "riak_ts"
project_version: "1.3.1"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing/debian-ubuntu"]
    - ["1.4.0+",      "setup/installing/debian-ubuntu"]
aliases:
    - /riakts/1.3.1/installing/debian-ubuntu/
---

[download]: ../../downloads/
[openfileslimit]: {{<baseurl>}}riak/kv/2.1.4/using/performance/open-files-limit
[planning]: ../../using/planning
[security basics]: {{<baseurl>}}riak/kv/2.1.4/using/security/basics


Riak TS can be installed on Debian or Ubuntu-based systems using a binary
package available [here][download].

{{% note %}}
Debian is only supported for developing with Riak TS and NOT for general operations.
{{% /note %}}


## Dependencies

### `ulimit`

Debian and Ubuntu give you a very small limit on open file handles. Even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][openfileslimit] for more information about changing the limit.


### PAM Library Requirement for Ubuntu

You must have the `libpam0g-dev` package used for [Pluggable Authentication Module (PAM)][security basics] authentication in order to install Riak TS.

To install this dependency:

```bash
sudo apt-get install libpam0g-dev
```


## Install Riak TS

Once you have [downloaded][download] the package, execute the following command to install Riak TS:

```bash
sudo dpkg -i riak-ts_1.4.0-1_amd64.deb
```


## Activate Riak TS node

Once you've installed Riak TS, start it on your node:

```bash
riak start
```


## Verify your installation

You can verify that Riak TS is successfully installed by running: 

```bash
dpkg -l | grep riak
```

If Riak TS has been installed successfully `riak-ts` is returned.


## Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].
