---
title: "Installing on Debian and Ubuntu"
description: "Installing on Debian and Ubuntu"
menu:
  riak_ts-1.2.0:
    name: "Debian and Ubuntu"
    identifier: "installing_on_debian_ubuntu"
    weight: 201
    parent: "installing"
project: "riak_ts"
project_version: "1.2.0"
toc: true
aliases:
    - /riakts/1.2.0/installing/debian-ubuntu/
canonical_link: "https://https://docs.basho.com/riak/ts/latest/installing/debian-ubuntu"
---


[concept aae]: /riak/kv/2.1.3/learn/concepts/active-anti-entropy
[planning]: ../../../using/planning
[security basics]: /riak/kv/2.1.3/using/security/basics


Riak TS can be installed on Debian or Ubuntu-based systems using a binary
package available through ZenDesk.

Check your email for the link to the download in ZenDesk.


## Dependencies

### PAM Library Requirement for Ubuntu

You must have the `libpam0g-dev` package used for [Pluggable Authentication Module (PAM)][security basics] authentication in order to install Riak TS.

To install this dependency:

```bash
sudo apt-get install libpam0g-dev
```


## Install Riak TS

Once you have downloaded the package from ZenDesk, execute the following command to install Riak TS:

```bash
sudo dpkg -i riak-ts_{{VERSION}}-1_amd64.deb
```

Then confirm that [AAE][concept aae] is turned off. To do this, check /etc/riak/riak.conf for the following: `anti_entropy = passive`.

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