---
title: Installing on Mac OS X
project: riakts
version: 1.0.0
document: tutorial
audience: beginner
keywords: [tutorial, installing, osx, mac]
download:
  key: osx
  name: "Mac OS X"
---

[AAE]: http://docs.basho.com/riak/2.1.2/theory/concepts/aae/
[openfileslimit]: http://docs.basho.com/riak/2.1.2/ops/tuning/open-files-limit/
[planning]: http://docs.basho.com/riakts/1.0.0/using/planning

Riak TS can be installed on Mac OS X systems using a binary
package available through ZenDesk.

>**Important**
>
>Mac OS X is only supported for developing with Riak TS and NOT for general operations.

Check your email for the link to the download in ZenDesk.

##Dependencies

###`ulimit`

OS X gives you a very small limit on open file handles. Even with a
backend that uses very few file handles, it's possible to run out. See
[Open Files Limit][openfileslimit] for more information about changing the limit.


##Install Riak TS

To install Riak TS on your Mac, download the package from ZenDesk and then run:

```bash
tar zxvf riak-ts-{{VERSION}}.tar.gz
cd riak-ts-{{VERSION}}
make rel
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