---
title: "Multi Data Center Replication: Configuration"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, configuration]
---

The configuration for replication is kept in the riak_repl section of `etc/app.config`.

    {riak_repl, [
                 {fullsync_on_connect, true},
                 {fullsync_interval, 360},
                 % Debian/Centos/RHEL:
                 {data_root, "/var/lib/riak/data/riak_repl"},
                 % Solaris:
                 % {data_root, "/opt/riak/data/riak_repl"},
                 % FreeBSD/SmartOS:
                 % {data_root, "/var/db/riak/riak_repl"},
                 {queue_size, 104857600},
                 {server_max_pending, 5},
                 {client_ack_frequency, 5}
              ]}

**fullsync_on_connect**  
Whether to initiate a fullsync on initial connection from the secondary cluster.

  * *Value:* true | false
  * *Default:* true
  * *Available:* Riak Enterprise (All)

**fullsync_strategies**
A list of full synchronization strategies to be used by replication. Please contact Basho support for more information.

  * *Value:* path [string]
  * *Default:* [syncv1]
  * *Available:* Riak Enterprise (All)

**fullsync_interval**  
How often to initiate a full synchronization of data, in minutes. This is measured from the completion of one full-sync operation to the initiation of the next. This setting only applies to the primary cluster (listener). To disable full synchronization, use: `disabled`.

  * *Value:* minutes [integer] | disabled
  * *Default:* 360 (6 hours)
  * *Available:* Riak Enterprise (All)

**keyfile**  
Fully qualified path to an ssl .pem key file/

  * *Value:* path [string]
  * *Default:* undefined
  * *Available:* Riak Enterprise (1.2+)

**data_root**  
Path (relative or absolute) to the working directory for the replication process.

  * *Value:* path [string]
  * *Default:* data/riak_repl
  * *Available:* Riak Enterprise (All)

**cacertdir**

The cacertdir is a fully-qualified directory containing all the CA certificates needed to verify the CA chain back to the root.

  * *Value:* path [string]
  * *Default:* undefined
  * *Available:* Riak Enterprise (1.2+)

**certfile**

Fully qualified path to a .pem cert file.

  * *Value:* path [string]
  * *Default:* undefined
  * *Available:* Riak Enterprise (1.2+)

**queue_size**  
The size of the replication queue in bytes before the replication leader will drop requests. If requests are dropped, a full_sync will be required. Information about dropped requests is available by using the command `riak-repl status`.

  * *Value:* bytes [integer]
  * *Default:* 104857600 (100 MiB/104.9 MB)
  * *Available:* Riak Enterprise (All)

**server_max_pending**  
The maximum number of objects the leader will wait to get an acknowledgement from the remote location before queuing the request.

  * *Value:* number of objects [integer]
  * *Default:* 5
  * *Available:* Riak Enterprise (All)

**client_ack_frequency**  
The number of requests a remote leader will handle before sending an acknowledgment to the remote cluster.

  * *Value:* number of requests [integer]
  * *Default:* 5
  * *Available:* Riak Enterprise (All)

**client_connect_timeout**  
The number of milliseconds to wait before a client connection timeout occurs.

  * *Value:* number of requests [integer]
  * *Default:* 15000
  * *Available:* Riak Enterprise (All)

**client_retry_timeout**  
The number of milliseconds to wait before trying to connect after a retry has occured.

  * *Value:* number of requests [integer]
  * *Default:* 30000
  * *Available:* Riak Enterprise (All)
 
**sndbuf**  
The buffer size for the listener (server) socket measured in bytes.

  * *Value:* bytes [integer]
  * *Default:* OS dependent
  * *Available:* Riak Enterprise (1.1+)

**ssl_depth**  
Set the depth to check for SSL CA certs. See: http://erlang.org/pipermail/erlang-questions/2012-April/065806.html

  * *Value:* depth [integer]
  * *Default:* 1
  * *Available:* Riak Enterprise (1.2+)

**ssl_enabled**  
Enable SSL communications.

  * *Value:* true | false
  * *Default:* false
  * *Available:* Riak Enterprise (1.2+)

**recbuf**  
The buffer size for the site (client) socket measured in bytes.

  * *Value:* bytes [integer]
  * *Default:* OS dependent
  * *Available:* Riak Enterprise (1.1+)

**vnode_gets**  
If true, repl will do a direct get against the vnode, rather than use a GET finite state machine.

  * *Value:* true | false
  * *Default:* true
  * *Available:* Riak Enterprise (1.1+)

**shuffle_ring**  
Toggles whether the ring is traversed in-order or shuffled randomly.

  * *Value:* true | false
  * *Default:* true (shuffle enabled)
  * *Available:* Riak Enterprise (1.1+)

**diff_batch_size**  
Defines how many fullsync objects to send before waiting for an acknowledgement from the client site.

  * *Value:* object count [integer]
  * *Default:* 100
  * *Available:* Riak Enterprise (1.1+)

**max_get_workers**  
The maximum number of get workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send.

_Note:_ Each get worker spawns 2 processes, one for the work, and one for the get FSM. Be sure you don't run over the maximum number of allowed processes in an Erlang VM (check vm.args for a +P property)

  * *Value:* worker count [integer]
  * *Default:* 100
  * *Available:* Riak Enterprise (1.1+)

**max_put_workers**  
The maximum number of put workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send.

_Note:_ Each put worker spawns 2 processes, one for the work, and one for the put FSM. Be sure you don't run over the maximum number of allowed processes in an Erlang VM (check vm.args for a +P property)

  * *Value:* worker count [integer]
  * *Default:* 100
  * *Available:* Riak Enterprise (1.1+)

**min_get_workers**  
The minimum number of get workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send.

_Note:_ Each get worker spawns 2 processes, one for the work, and one for the get FSM.

  * *Value:* worker count [integer]
  * *Default:* 5
  * *Available:* Riak Enterprise (1.1+)

**min_put_workers**  
The minimum number of put workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send.

_Note:_ Each put worker spawns 2 processes, one for the work, and one for the put FSM.

  * *Value:* worker count [integer]
  * *Default:* 5
  * *Available:* Riak Enterprise (1.1+)

**peer_common_name_acl**  
Verify an SSL peer’s certificate common name. You can provide multiple ACLs as a list of strings, and you can wildcard the leftmost part of the common name, so `*.basho.com` would match `site3.basho.com` but not `foo.bar.basho.com` or `basho.com`.

_Note:_ If the ACL is specified and not the special value `*`, certificates not matching any of the rules will not be allowed to connect. If no ACLs are configured, no checks on the common name are done.

  * *Value:* ssl cert name [string]
  * *Default:* "*"
  * *Available:* Riak Enterprise (1.2+)

