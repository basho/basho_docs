---
title: Verifying a Riak Installation
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, upgrading, verify, verifying]
moved: {
    '1.4.0-': '/tutorials/installation/Post-Installation',
    '2.1.1-': '/ops/installing/Post-Installation'
}
---

After you've installed Riak, we recommend checking the liveness of
each node to ensure that requests are being properly served.

In this document, we cover ways of verifying that your Riak nodes are operating
correctly. After you've determined that your nodes are functioning and you're
ready to put Riak to work, be sure to check out the resources in the
**Now What?** section below.

## Starting a Riak Node

<div class="note">
<div class="title">Note about source installations</div>
To start a Riak node that was installed by compiling the source code, you
can add the Riak binary directory from the installation directory you've
chosen to your <tt>PATH</tt>.

For example, if you compiled Riak from source in
the <tt>/home/riak</tt> directory, then you can add the binary directory
(<tt>/home/riak/rel/riak/bin</tt>) to your <tt>PATH</tt> so that Riak commands can be used in the same manner as with a packaged installation.
</div>

To start a Riak node, use the `riak start` command:

```bash
riak start
```

A successful start will return no output. If there is a problem starting the
node, an error message is printed to standard error.

To run Riak with an attached interactive Erlang console:

```bash
riak console
```

A Riak node is typically started in console mode as part of debugging or
troubleshooting to gather more detailed information from the Riak startup
sequence. Note that if you start a Riak node in this manner, it is running as
a foreground process that will be exited when the console is closed.

You can close the console by issuing this command at the Erlang prompt:

```erlang
q().
```

Once your node has started, you can initially check that it is running with
the `riak ping` command:

```bash
riak ping
```

The command will respond with `pong` if the node is running or `Node <nodename>  not responding to pings` if it is not.

<div class="note">
<div class="title">Open Files Limit</div>
As you may have noticed, if you haven't adjusted your open files limit (`ulimit -n`), Riak will warn you at startup. You're advised
to increase the operating system default open files limit when running Riak.
You can read more about why in the [[Open Files Limit]] documentation.
</div>

## Does it work?

One convenient means of testing the readiness of an individual Riak node and
its ability to read and write data is with the `riak-admin test` command:

```bash
riak-admin test
```

Successful output from `riak-admin test` looks like this:

```text
Attempting to restart script through sudo -H -u riak
Successfully completed 1 read/write cycle to '<nodename>'
```

You can also test whether Riak is working by using the `curl` command-line
tool. When you have Riak running on a node, try this command to retrieve
the the properties associated with the [[bucket type|Using Bucket Types]] `test`:

```bash
curl -v http://127.0.0.1:8098/types/default/props
```

Replace `127.0.0.1` in the example above with your Riak node's IP address or
fully qualified domain name, and you should get a response that looks like this:

```
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test HTTP/1.1
> User-Agent: curl/7.21.6 (x86_64-pc-linux-gnu)
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
< Date: Wed, 26 Dec 2012 15:50:20 GMT
< Content-Type: application/json
< Content-Length: 422
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"props":{"name":"test","allow_mult":false,"basic_quorum":false,
 "big_vclock":50,"chash_keyfun":{"mod":"riak_core_util",
 "fun":"chash_std_keyfun"},"dw":"quorum","last_write_wins":false,
 "linkfun":{"mod":"riak_kv_wm_link_walker","fun":"mapreduce_linkfun"},
 "n_val":3,"notfound_ok":true,"old_vclock":86400,"postcommit":[],"pr":0,
 "precommit":[],"pw":0,"r":"quorum","rw":"quorum","small_vclock":50,
 "w":"quorum","young_vclock":20}}
```

The output above shows a successful response (`HTTP 200 OK`) and additional
details from the verbose option. The response also contains the bucket
properties for the `default` bucket type.

## Riaknostic

It is a good idea to verify some basic configuration and general health
of the Riak node after installation by using Riak's built-in diagnostic
utility [Riaknostic](http://riaknostic.basho.com/).

To start up Riaknostic, ensure that Riak is running on the node and issue the following command:

```bash
riak-admin diag
```

More extensive documentation for Riaknostic can be found in the [[Inspecting a Node]] guide.

## Now what?

You have a working Riak node!

From here you might want to check out the following resources:

* [[Client Libraries]] to use Riak with your favorite programming language
