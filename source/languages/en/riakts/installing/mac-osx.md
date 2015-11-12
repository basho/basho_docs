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

[openfileslimit]: http://docs.basho.com/riak/2.1.1/ops/tuning/open-files-limit/
[configuring]: http://docs.basho.com/riakts/1.0.0/using/configuring

Riak TS can be installed on Mac OS X systems using a binary
package available through ZenDesk.

>**Note**
>
>Mac OS X is only supported for developing with Riak TS and NOT for general operations.

Check your e-mail for the link to the download in ZenDesk.

##Dependencies

>**`ulimit` on OS X**
>
>OS X gives you a very small limit on open file handles, so even with a
backend that uses very few file handles, it's possible to run out. See
[[Open Files Limit]] for more information about changing the limit.
</div>

##Install Riak TS
To install Riak TS on your Mac, download the package from ZenDesk and then run:

```bash
tar zxvf riak-ts-{{VERSION}}.tar.gz
cd riak-ts-{{VERSION}}
make rel
```

If you receive errors when building about "incompatible architecture,"
please verify that you built Erlang with the same architecture as your
system (Snow Leopard and higher: 64bit{{#1.4.0-}}, everything else:
32bit{{/1.4.0-}}). **?? Would this still apply to TS?**


##Turn off AAE
Once you've installed Riak TS, you must turn off [AAE][AAE]. To do this, edit riak.conf as follows:

```riak.conf
anti_entropy = passive
```


##Next Steps
Now that you've installed Riak TS, check out [Configuring Your Riak TS Table][configuring].