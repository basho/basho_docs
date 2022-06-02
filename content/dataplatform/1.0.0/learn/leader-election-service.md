---
title: "Leader Election Service"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Leader Election Service"
    identifier: "learn_leader_election_service"
    weight: 102
    parent: "learn"
toc: true
aliases:
  - /dataplatform/1.0.0/learn-about-dataplatform/leader-election-service/
  - /dataplatform/latest/learn/leader-election-service/
---

[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html
[riak_ensemble]: https://github.com/basho/riak_ensemble


> Leader Election Service is available to [Enterprise users only][ee].

## Overview

The Basho Data Platform (BDP) Leader Election Service enables Spark clusters to run without a ZooKeeper instance. 

The Leader Election Service uses a simple, line-based, ascii protocol to interact with Spark. This protocol is incompatible with the ZooKeeper protocol, and requires a BDP-specific patch to Spark for compatibility purposes. The Protocol Details section of this page further details the Leader Election Service protocol.

The service can be run on any or all nodes in a Riak cluster, so long as those nodes have [`riak_ensemble`][riak_ensemble]. Because it uses a strongly consistent backing store that is spread across the entire cluster, it does not normally matter which nodes are running the service. As long as there are no network partitions or outages interfering with normal operation, you should be able to point Spark to any Riak node that is running the election service, and everything will work the same way regardless.

Many groups of clients can use the election service at the same time by defining different election group names when they connect. If a client connects to a group that does not currently exist, it will be automatically created. That client is then elected leader. Leaders remain elected indefinitely until they disconnect from the service. A new leader is chosen from the remaining clients.

## Setup and Configuration
Before enabling the leader election service, strong consistency must be enabled on your Riak cluster, as described in the Managing Strong Consistency documentation. You can verify that strong consistency has been successfully enabled by using the ‘riak-admin ensemble-status’ command.

By default, any new data platform installation will include a commented template configuration for the leader election service in `riak.conf`. This should look something like this:

```riakconf
## listener.leader_latch.internal = 127.0.0.1:5323
```

If you uncomment this line on a given node and restart Riak, that node will start listening for leader election clients over the loopback interface on TCP port 5323. Changing the IP address here changes which network interface the service listens on. This be necessary if Spark is running on a different host from Riak. 

>**Warning**
>
>Please note that this service provides no authentication mechanism. Whatever network interface you use must be shielded from external connection attempts. Otherwise an outside attacker could potentially perform a denial of service attack against your cluster.


The example configuration above specifies a single interface/port pair to listen on. The “internal” part of this configuration line is a name given to this instance of the service. 

You can replace “internal” with whatever name you like. You can also specify as many listener interfaces as you choose, as long as multiple listeners on the same node have different names. 

For example:

```riakconf
listener.leader_latch.internal = 127.0.0.1:5323
listener.leader_latch.external = 10.10.1.2:5323
listener.leader_latch.testing = 192.168.0.42:12345
```

Note that changes to these settings will not have an impact on a live running node. You must restart the given Riak node for changes to take effect.

## Operational Concerns

The leader service relies on a strongly consistent backing store, shared by all the nodes in the Riak cluster. To guarantee liveness of this backing store, a majority of nodes in the cluster must be available and able to communicate with one another. If a node running the election service loses contact with a majority of other nodes (due to downed nodes, network outages, performance problems, etc.), then the service will become unavailable until the situation is resolved.

As mentioned before, this service provides no security guarantees or authentication mechanisms of any kind. It is your responsibility to prevent outside access to the leader election service.

## Protocol Details
The following section details the implementation of the election service. This may be helpful for debugging and testing the election service, or creating a new client beyond the existing Spark connector.

### Implementation
The protocol used for this service is a simple, line-based, ascii protocol. You can test and interact with the service using telnet (or an equivalent utility). 

There is currently only one command a client can send to the service: 

```telnet
JOIN »group_name« »client_name«
```

This command tells the election service to add a client named `»client_name«` to an election group named »group_name«`. 

Both the client name and the group name must be ascii strings with no spaces in them. Under normal circumstances, the service should respond with:

```
LEADER »current_leader«
```

The connection must remain open as long as the client wishes to remain a part of the election group. Any subsequent leader changes will be asynchronously pushed to the client across this same connection.

If the service becomes unavailable (for example, an outage involving a majority of Riak nodes) and a client attempts to join an election group, the server may respond with an error message reading:

```
JOIN_FAILED SERVICE_UNAVAILABLE
```

If a client attempts to execute any additional joins (to the same or other group names) on the same connection after the initial join is performed, the server will respond with: 

```
ALREADY_JOINED »group_name« »client_name«
```

And the additional join attempt(s) will fail. 

If a client attempts to send an invalid command the server will respond with `ERROR`.

### Example Session
Following is an example interaction with the election service:

```bash
telnet 127.0.0.1 5323
```

```telnet
Trying 127.0.0.1...

Connected to localhost.

Escape character is '^]'.

JOIN test_group 1

JOIN_FAILED SERVICE_UNAVAILABLE

Connection closed by foreign host.
```

In this case the cluster only had one out of three members online, because we forgot to start the other two nodes. Because a majority of members were offline, we were unable to join the election group. If we start up the other two nodes and try again::

```bash
telnet 127.0.0.1 5323
```

```telnet
Trying 127.0.0.1...

Connected to localhost.

Escape character is '^]'.

JOIN test_group 1

LEADER 1
```


Great. We have one member named "1" in test_group, so as we expect, they are elected leader. Now let's leave that session running, then open another terminal and join another member to the group:

```bash
telnet 127.0.0.1 5323
```

```telnet
Trying 127.0.0.1...

Connected to localhost.

Escape character is '^]'.

JOIN test_group 2

LEADER 1
```

Since 1 is still up and running, they're still the leader. When we join 2 to the group, it tells us that 1 is the current leader. If we close our first terminal window, then this new session should be elected leader after a short timeout. Let's try it out:

```telnet
LEADER 2
```

Our new connection is notified that client 2 is the new leader.
