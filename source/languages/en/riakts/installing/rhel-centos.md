---
title: Installing on CentOS
project: riakts
version: 1.0.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, centos, linux]
download:
  key: rhel
  name: "CentOS"
---

[AAE]: 
[configuring]: http://docs.basho.com/riakts/1.0.0/using/configuring

Riak TS can be installed on CentOS-based systems using a binary
package available through ZenDesk.

Check your e-mail for the link to the download in ZenDesk.

>**Note on SELinux**
>
>CentOS enables SELinux by default, so you may need to disable SELinux if
you encounter errors.


##Install Riak TS

###For CentOS 6 / RHEL 6
Once you've downloaded the package from ZenDesk, you can install the package using `yum`:

```bash
sudo yum install riak-ts
```

Or manually:

```bash
sudo rpm -Uvh riak-ts-{{VERSION}}-1.el6.x86_64.rpm
```


### For CentOS 7 / RHEL 7
Once you've downloaded the package from ZenDesk, you can install the package using `yum`:

```bash
sudo yum install riak-ts
```

Or manually:

```bash
sudo rpm -Uvh riak-ts-{{VERSION}}-1.el7.x86_64.rpm
```


###Turn off AAE

Confirm that [AAE][AAE] is turned off. To do this, check riak.conf for the following: `anti_entropy = passive`.


##Activate Riak TS node

Once you've installed Riak TS, start it on your node:

```bash
riak start
```


##Next Steps
Now that you've installed Riak TS, check out [Configuring Your Riak TS Table][configuring].