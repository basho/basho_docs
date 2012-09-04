Click the link for your operating system to see instructions on how to install Riak. But first, you must [[Install Erlang|Installing Erlang]].

  * [[Installing on Debian and Ubuntu]]
  * [[Installing on RHEL and CentOS]]
  * [[Installing on Mac OS X]]
  * [[Installing Riak from Source]]

## Starting up
To start up a Riak node, change directory as necessary to where you installed Riak (in the source directory, it's `rel/riak/bin` and run the riak command like so:

```bash
riak start
```

To run Riak with an interactive Erlang console:

```bash
riak console
```

Once your node has started, you can double-check that it is running using:

```bash
riak ping
pong
```

The command will respond with **pong** if the node is running, or **pang** if it is not.

<div class="note"><div class="title">Open Files Limit</div>
As you may have noticed, if you haven't adjusted your ulimit settings, Riak will warn you. You're advised to raise your open files limit when running Riak. You can read more about why on the [[Open Files Limit]] page.</div>

## Does it work?
The easiest way to test whether Riak is working is to use the `curl` command-line tool. Now that you have Riak running on your local machine, run this command:

```bash
curl -v http://127.0.0.1:8098/riak/test
```

You should get a response that looks like this:

```bash
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
GET /riak/test HTTP/1.1
User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
Host: 127.0.0.1:8098
Accept: */*

HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.6 (eat around the stinger)
Date: Mon, 08 Mar 2010 16:42:49 GMT
Content-Type: application/json
Content-Length: 266

* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"props":{"name":"test","allow_mult":false,"big_vclock":50,"chash_keyfun":{"mod":"riak_util","fun":"chash_std_keyfun"},"linkfun":{"mod":"raw_link_walker_resource","fun":"mapreduce_linkfun"},"n_val":3,"old_vclock":86400,"small_vclock":10,"young_vclock":20},"keys":[]}
```

## Now what?
You have a working Riak system! From here you might want to check out:

  * [[The Riak Fast Track]]: A guide for setting up a 3 node cluster and exploring Riak's main features.
  * Check out the [[Client Libraries]] to use Riak with your favorite programming language
  * Get some [[Sample Data]]
  * [[Add more nodes|Basic Cluster Setup]] to your Riak cluster
  * [[Learn about the high level concepts of Riak|Concepts]]
  * [[Learn more about day-to-day system operations|Operations]]
