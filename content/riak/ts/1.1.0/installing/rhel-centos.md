---
title: "Installing on CentOS/RHEL"
description: "Installing on CentOS/RHEL"
menu:
  riak_ts-1.1.0:
    name: "CentOS/RHEL"
    identifier: "installing_on_centos_rhel"
    weight: 203
    parent: "installing"
project: "riak_ts"
project_version: "1.1.0"
toc: true
version_history:
  locations:
    - ["1.0.0-1.3.1", "installing/rhel-centos"]
    - ["1.4.0+",      "setup/installing/rhel-centos"]
aliases:
    - /riakts/1.1.0/installing/rhel-centos/
---

[concept aae]: {{<baseurl>}}riak/kv/2.1.3/learn/concepts/active-anti-entropy
[planning]: ../../using/planning

Riak TS can be installed on CentOS-based systems using a binary
package available through ZenDesk.

Check your email for the link to the download in ZenDesk.

>**Note on SELinux**
>
>CentOS enables SELinux by default, so you may need to disable SELinux if
you encounter errors.


## Install Riak TS

### For CentOS 6 / RHEL 6

Once you've downloaded the package from ZenDesk, you can install the package using `yum`:

```bash
sudo yum install riak-ts-1.0.0-1.el6.centos.x86_64.rpm
```

Or manually:

```bash
sudo rpm -Uvh riak-ts-{{VERSION}}-1.el6.x86_64.rpm
```


### For CentOS 7 / RHEL 7

Once you've downloaded the package from ZenDesk, you can install the package using `yum`:

```bash
sudo yum install riak-ts-1.0.0-1.el7.centos.x86_64.rpm
```

Or manually:

```bash
sudo rpm -Uvh riak-ts-{{VERSION}}-1.el7.x86_64.rpm
```


### Turn off AAE

Confirm that [AAE][concept aae] is turned off. To do this, check /etc/riak/riak.conf for the following: `anti_entropy = passive`.


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
