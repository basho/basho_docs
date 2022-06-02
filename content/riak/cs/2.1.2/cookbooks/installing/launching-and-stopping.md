---
title: "Launching and Stopping Riak CS"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Launching and Stopping"
    identifier: "run_launch_stop"
    weight: 101
    parent: "run"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/installing/Launching-and-Stopping-Riak-CS/
  - /riak/cs/2.1.2/cookbooks/installing/Launching-and-Stopping-Riak-CS/
  - /riak/cs/latest/cookbooks/installing/launching-and-stopping/
---

To launch Riak CS in the background:

```bash
sudo riak-cs start
```

To run Riak CS with an interactive Erlang console:

```bash
sudo riak-cs console
```

When Riak CS is running, the Riak CS process appears in the process
list. To check for the Riak CS process, enter:

```bash
ps -ef | grep riak-cs
```

To stop Riak CS, enter:

```bash
sudo riak-cs stop
```

You can use the command

```bash
sudo riak-cs attach
```

to attach and obtain an interactive console to a running instance of
Riak CS.

You can check the liveness of your Riak CS installation with the
`riak-cs ping` command, which should return `pong` if Riak CS is up and
running.

```bash
riak-cs ping
```

Please note that `riak-cs ping` tests only the liveness of Riak CS and
does not test the connection between Riak CS and Riak. In order to test
that, you can run a `GET` request against the `/riak-cs/ping` endpoint of a Riak
CS node, as in the example below:

```curl
curl http://localhost:8080/riak-cs/ping
```
