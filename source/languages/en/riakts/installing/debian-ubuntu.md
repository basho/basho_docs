---
title: Installing on Debian and Ubuntu
project: riakts
version: 1.1.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, debian, ubuntu, linux]
download:
  key: debian
  name: "Debian or Ubuntu"
---

[AAE]: http://docs.basho.com/riak/2.1.3/theory/concepts/aae/
[planning]: http://docs.basho.com/riakts/1.1.0/using/planning
[riak security]: http://docs.basho.com/riak/2.1.3/ops/running/authz/


Riak TS can be installed on Debian or Ubuntu-based systems using a binary
package available through ZenDesk.

Check your email for the link to the download in ZenDesk.


##Dependencies

###PAM Library Requirement for Ubuntu

You must have the `libpam0g-dev` package used for [Pluggable Authentication Module (PAM)][riak security] authentication in order to install Riak TS.

To install this dependency:

```bash
sudo apt-get install libpam0g-dev
```


##Install Riak TS

Once you have downloaded the package from ZenDesk, execute the following command to install Riak TS:

```bash
sudo dpkg -i riak-ts_{{VERSION}}-1_amd64.deb
```

Then confirm that [AAE][AAE] is turned off. To do this, check /etc/riak/riak.conf for the following: `anti_entropy = passive`.

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