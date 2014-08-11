---
title: "Multi Data Center Replication: Configuration"
project: riak
header: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, configuration]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-Configuration'
}
---

## File

The configuration for replication is kept in the `riak_repl` section of `app.config`.

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

## Usage

These settings are configured using the standard Erlang config file syntax `{Setting, Value}`. For example, if you wished to set `ssl_enabled` to `true`, you would insert the following line into the `riak_repl` section (appending a comma if you have more settings to follow):

```erlang
{ssl_enabled, true}
```

## Settings

Once your configuration is set, you can verify its correctness by running the command-line tool:

```bash
$ riak chkconfig
```

Setting | Options | Default | Description
--------|---------|---------|------------
`fullsync_on_connect` | `true`, `false` | `true` | Whether or not to initiate a fullsync on initial connection from the secondary cluster
`fullsync_strategies` | {{#2.0.0-}} `keylist`, `syncv1`{{/2.0.0-}} {{#2.0.0+}}`keylist`{{/2.0.0+}} | {{#2.0.0-}}`[keylist, syncv1]`{{/2.0.0-}} {{#2.0.0+}}`[keylist]`{{/2.0.0+}} | A *list* of fullsync strategies to be used by replication. <br /> **Note**: Please contact Basho support for more information.
`fullsync_interval`   | `mins` (integer), `disabled` | `360` | How often to initiate a fullsync of data, in minutes. This is measured from the completion of one fullsync operation to the initiation of the next. This setting only applies to the primary cluster (listener). **To disable fullsync, use *fullsync_interval* `disabled` with *fullsync_on_connect* set to `false`.**
`keyfile` | `path` (string) | `undefined` | Fully qualified path to an ssl `.pem` key file
`data_root` | `path` (string) | `data/riak_repl` | Path (relative or absolute) to the working directory for the replication process
`cacertdir` | `path` (string) | `undefined` | The `cacertdir` is a fully-qualified directory containing all the CA certificates needed to verify the CA chain back to the root
`certfile` | `path` (string) | `undefined` | Fully qualified path to a `.pem` cert file
`queue_size` | `bytes` (integer) | `104857600` (100 MiB) | The size of the replication queue in bytes before the replication leader will drop requests. If requests are dropped, a fullsync will be required. Information about dropped requests is available by using the command `riak-repl status`
`server_max_pending` | `max` (integer) | `5` | The maximum number of objects the leader will wait to get an acknowledgment from&mdash;from the remote location&mdash;before queuing the request
`client_ack_frequency` | `freq` (integer) | `5` | The number of requests a leader will handle before sending an acknowledgment to the remote cluster
`client_connect_timeout` | `ms` (integer) | `15000` | The number of milliseconds to wait before a client connection timeout occurs
`client_retry_timeout` | `ms` (integer) | `30000` | The number of milliseconds to wait before trying to connect after a retry has occurred
`sndbuf` | `bytes` (integer) | OS dependent | The buffer size for the listener (server) socket measured in bytes
`ssl_depth` | `depth` (integer) | `1` | Set the depth to check for SSL CA certs. See <a href="/ops/mdc/v2/configuration/#f1">1</a>.
`ssl_enabled` | `true`, `false` | `false` | Enable SSL communications
`recbuf` | `bytes` (integer) | OS dependent | The buffer size for the site (client) socket measured in bytes
`vnode_gets` | `true`, `false` | `true` | If `true`, repl will do a direct get against the vnode, rather than use a `GET` finite state machine
`shuffle_ring` | `true`, `false` | `true `| If `true`, the ring is shuffled randomly. If `false`, the ring is traversed in order. Useful when a sync is restarted to reduce the chance of syncing the same partitions.
`diff_batch_size` | `objects` (integer) | `100` | Defines how many fullsync objects to send before waiting for an acknowledgment from the client site
`max_get_workers` | `max` (integer) | `100` | The maximum number of get workers spawned for fullsync. Every time a replication difference is found, a `GET` will be performed to get the actual object to send. See <a href="/ops/mdc/v2/configuration/#f2">2</a>.
`max_put_workers` | `max` (integer) | `100` | The maximum number of put workers spawned for fullsync. Every time a replication difference is found, a `GET` will be performed to get the actual object to send. See <a href="/ops/mdc/v2/configuration/#f3">3</a>.
`min_get_workers` | `min` (integer) | `5` | The minimum number of get workers spawned for fullsync. Every time a replication difference is found, a `GET` will be performed to get the actual object to send. See <a href="/ops/mdc/v2/configuration/#f2">2</a>.
`min_put_workers` | `min` (integer) | `5` | The minimum number of put workers spawned for fullsync. Every time a replication difference is found, a `GET` will be performed to get the actual object to send. See <a href="/ops/mdc/v2/configuration/#f3">3</a>.
`peer_common_name_acl` | `cert` (string) | `"*"` | Verify an SSL peer’s certificate common name. You can provide multiple ACLs as a list of strings, and you can wildcard the leftmost part of the common name, so `*.basho.com` would match `site3.basho.com` but not `foo.bar.basho.com` or `basho.com`. See <a href="/ops/mdc/v2/configuration/#f4">4</a>.


1. <a name="f1"></a>SSL depth is the maximum number of non-self-issued intermediate certificates that may follow the peer certificate in a valid certification path. If depth is `0`, the PEER must be signed by the trusted ROOT-CA directly; if `1` the path can be PEER, CA, ROOT-CA; if depth is `2` then PEER, CA, CA, ROOT-CA and so on.

2. <a name="f2"></a>Each get worker spawns 2 processes, one for the work and one for the get FSM (an Erlang finite state machine implementation for `GET` requests). Be sure that you don't run over the maximum number of allowed processes in an Erlang VM (check `vm.args` for a `+P` property).

3. <a name="f3"></a>Each put worker spawns 2 processes, one for the work, and
  one for the put FSM (an Erlang finite state machine implementation for `PUT` requests). Be sure that you don't run over the maximum number of allowed
  processes in an Erlang VM (check `vm.args` for a `+P` property).

4. <a name="f4"></a>If the ACL is specified and not the special value `*`,
  certificates not matching any of the rules will not be allowed to connect.
  If no ACLs are configured, no checks on the common name are done.
