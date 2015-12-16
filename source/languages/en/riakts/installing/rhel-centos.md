---
title: Installing on CentOS/RHEL
project: riakts
version: 1.0.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, centos, linux]
download:
  key: centos, rhel
  name: "CentOS/RHEL"
---

[AAE]: http://docs.basho.com/riak/2.1.3/theory/concepts/aae/
[planning]: http://docs.basho.com/riakts/1.0.0/using/planning

Riak TS can be installed on CentOS-based systems using a binary
package available through ZenDesk.

Check your email for the link to the download in ZenDesk.

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

Confirm that [AAE][AAE] is turned off. To do this, check /etc/riak/riak.conf for the following: `anti_entropy = passive`.


##Activate Riak TS node

Once you've installed Riak TS, start it on your node:

```bash
riak start
```


##Verify your installation

You can verify that Riak TS is successfully installed by running: 

```bash
dpkg -l | grep riak
```

If Riak TS has been installed successfully `riak-ts` is returned.


##Next Steps

Now that you've installed Riak TS, check out [Planning Your Riak TS Table][planning].