---
title: "Installing on CentOS/RHEL"
description: "Installing on CentOS/RHEL"
menu:
  riak_ts-1.3.1:
    name: "CentOS/RHEL"
    identifier: "installing_on_centos_rhel"
    weight: 203
    parent: "installing"
project: "riak_ts"
project_version: "1.3.1"
toc: true
aliases:
    - /riakts/1.3.1/installing/rhel-centos/
canonical_link: "docs.basho.com/riak/ts/latest/installing/rhel-centos"
---

[download]: ../../downloads/
[openfileslimit]: /riak/kv/2.1.4/using/performance/open-files-limit
[planning]: ../../using/planning/


Riak TS can be installed on CentOS-based systems using a binary
package available [here][download].

>**Note on SELinux**
>
>CentOS enables SELinux by default, so you may need to disable SELinux if
you encounter errors.


## Dependencies

### `ulimit`

CentOS and RHEL give you a very small limit on open file handles. Even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][openfileslimit] for more information about changing the limit.


## Install Riak TS

### For CentOS 6 / RHEL 6

Once you've [downloaded][download] the package, you can install the package using `yum` or `rpm`:

```bash
sudo yum install riak-ts-1.3.1-1.el6.x86_64.rpm
```

or

```bash
sudo rpm -Uvh riak-ts-1.3.1-1.el6.x86_64.rpm
```


### For CentOS 7 / RHEL 7

Once you've [downloaded][download] the package, you can install the package using `yum` or `rpm`:

```bash
sudo yum install riak-ts-1.3.1-1.el7.centos.x86_64.rpm
```

or

```bash
sudo rpm -Uvh riak-ts-1.3.1-1.el7.centos.x86_64.rpm
```


## Activate Riak TS node

Once you've installed Riak TS, start it on your node:

```bash
riak start
```


## Verify your installation

You can verify that Riak TS is successfully installed by running: 

```bash
yum list installed riak-ts
```

or

```bash
rpm -q riak-ts
```

If Riak TS has been installed successfully `riak-ts` is returned.


## Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].