---
title: Launching and Stopping Riak CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing]
---

To launch Riak CS in the background, enter:

```bash
$ sudo riak-cs start
```

To run Riak CS with an interactive Erlang console:

```bash
$ sudo riak-cs console
```

When Riak CS is running, the Riak CS process appears in the process list. To check for the Riak CS process, enter:

```bash
$ ps -ef | grep riak-cs
```

To stop Riak CS, enter:

```bash
$ sudo riak-cs stop
```

You can use the command

```bash
$ sudo riak-cs attach
```

to attach and obtain an interactive console to a running instance of Riak CS.

You can check the liveness of your Riak CS installation with the `riak-cs ping` command, which should return `pong` if Riak CS is up and able to successfully communicate with Riak.

```bash
$ riak-cs ping
```

<div class="note"><div class="title">Note</div>The <tt>riak-cs ping</tt> command will fail if the Riak CS node is not able to communicate with the supporting Riak node. Ensure that all components of the Riak CS system are running before checking liveness with <tt>riak-cs ping</tt>.</div>