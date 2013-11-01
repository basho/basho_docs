---
title: Configuration Files
project: riak
version: 0.10.0+
document: reference
toc: true
audience: intermediate
keywords: [operator]
moved: {
    '1.4.0-': '/references/Configuration-Files'
}
---

Riak has two configuration files located in `etc/` if you are using a source
install and in `/etc/riak` if you used a binary install. The files are
`app.config` and `vm.args`.

The `app.config` file is used to set various attributes for the node such as the
backend the node will use to store data. The `vm.args` file is used to pass
parameters to the Erlang node such as the name or cookie of the Erlang node.

## app.config

Riak and the Erlang applications it depends on are configured by settings in the app.config file in the etc directory of the Riak node.

```erlang
[
    {riak_core, [
        {ring_state_dir, "data/ring"}
        %% More riak_core settings...
            ]},
    {riak_kv, [
        {storage_backend, riak_kv_bitcask_backend},
        %% More riak_kv settings...
            ]},
    %% Other application configurations...
].
```

<div class="note">
Note that lines prefixed with `%%` are comments
</div>

{{#1.2.0+}}

### riak_api settings

 * **pb_ip**
    The IP address that the Protocol Buffers interface will bind to.
    (default: `"127.0.0.1"`)

    If not set, the PBC interface will not be
    started. {{#1.3.0+}} The IP address may be specified as a string or
    tuple of address components as integers (4 for IPv4, 8 for IPv6). For
    example:

    ```erlang
    %% binds to specific IPv4 interface
    {pb_ip, {10,1,1,56}}

    %% binds to all IPv6 interfaces
    {pb_ip, "::0"}

    %% binds to a specific IPv6 interface
    {pb_ip, {65152,0,0,0,64030,57343,65250,15801}}
    ```
    {{/1.3.0+}}

 * **pb_port**
The port that the Protocol Buffers interface will bind to. (default: `8087`)

 * **pb_backlog**
The maximum length to which the queue of pending *simultaneous*
protocol buffers connections may grow. If set, it must be an integer >= 0.
If you anticipate a larger number of connections than the default being
simultaneously initialized, set this number to a higher value accordingly.
You should adjust this value to meet your anticipated simultaneous
connection demand or if experiencing connection resets. (default: `5`)

 * **disable_pb_nagle** Turns off Nagle's algorithm (aka TCP
slow-start) for Protocol Buffers connections. This is equivalent to
setting the TCP_NODELAY option on the socket. (default:
{{#1.3.0+}}`false`{{/1.3.0+}}{{#1.3.0-}}`true`{{/1.3.0-}})

{{/1.2.0+}}

### riak_core settings

* **choose_claim_fun**
`{Module, Function}` to claim vnodes from the passed in ring and return the resulting ring.

* **cluster_name**
The name of the cluster. This currently has no visible effect, but could be useful for identifying multiple clusters within a larger infrastructure.

* **default_bucket_props**
    These are properties used for buckets that have not been explicitly defined (as outlined in the HTTP API). They are useful for setting default bucket behavior such as:

    ```erlang
    {default_bucket_props, [
        {n_val,3},
        {allow_mult,false},
        {last_write_wins,false},
        {precommit, []},
        {postcommit, []},
        {chash_keyfun, {riak_core_util, chash_std_keyfun}},
        {linkfun, {modfun, riak_kv_wm_link_walker, mapreduce_linkfun}}
        ]}
    ```

    * n_val - the number of replicas stored. *Note: See [[CAP Controls]] for further discussion.*
    * Read, Write and Delete quorum values. Valid options include numeric values (e.g. ```{r, 2}```), and the following symbolic values:<br />
    ```quorum``` (a majority of the replicas must respond, equivalent to ```n_val / 2 + 1```)<br />
    ```all``` (all N replicas must respond)
        * r - Read quorum value (the number of Riak nodes which must return results for a GET request before it is considered
        successful). Default: ```quorum```.
        * pr - Primary read quorum (the number of primary, non-fallback nodes that must return results for a successful GET request).
        Default: ```0```.
        *Note: See [[Eventual Consistency]] for an explanation of primary nodes.*
        * w - Write quorum value (the number of Riak nodes which must *accept* a PUT request). Default: ```quorum```.
        * dw - Durable write quorum (the number of Riak nodes which have received an acknowledgment of the write from the storage backend).
        Default: ```quorum```.
        * pw - Primary write quorum  (the number of primary, non-fallback nodes that must accept a PUT request).
        Default: ```0```.
        * rw - Delete quorum. Default: ```quorum```.
    * allow_mult - whether or not siblings are allowed. *Note: See [[Vector Clocks]] for a discussion of sibling resolution.*
    * precommit - global [[pre-commit hook|Using Commit Hooks#Pre-Commit-Hooks]] functions, either in Javascript or Erlang.
    * postcommit - global [[post-commit hook|Using Commit Hooks#Post-Commit-Hooks]] functions. Erlang only.

* **delayed_start**
Sleep a number of milliseconds before starting riak_core. Default: `unset`

* **disable_http_nagle**
When set to `true`, this option will disable the Nagle buffering algorithm for HTTP traffic. This is equivalent to setting the TCP_NODELAY option on the HTTP socket. The setting defaults to false. If you experience consistent minimum latencies in multiples of 20 milliseconds, setting this option to true may reduce latency.

* **gossip_interval**
How often nodes in the cluster will share information about their ring state, in milliseconds. (default: `60000`)

* **handoff_concurrency**
Number of vnodes, per physical node, allowed to perform handoff at once. (default: `2`)

* **handoff_port**
TCP port number for the handoff listener. (default: `8099`)

* **handoff_ip**
The IP address the handoff listener will bind to. (default: `"0.0.0.0"`)
{{#1.3.0+}} The IP address may be specified as a string or tuple of
address components as integers (4 for IPv4, 8 for IPv6). See `pb_ip`
above for examples. {{/1.3.0+}}

* **http**
    A list of IP addresses and ports on which Riak's HTTP interface should listen. (default: `{"127.0.0.1", 8091 }`)

    *Riak's HTTP interface will not start if this setting is not defined.*

* **http_logdir**
Override the default location of the access logs. See webmachine_logger_module settings to enable access logs.

* **https**
    A list of IP addresses and ports on which Riak's HTTPS interface should listen. (default: not enabled)

    *Riak's HTTPS interface will not start if this setting is not defined.*

* **legacy_vnode_routing**
(boolean) for compatibility with older versions.

* **platform_data_dir**
Base directory for backend data storage. (default: `./data`)

* **ring_state_dir**
    The directory on-disk in which to store the ring state. (default: `data/ring`)

    Riak's ring state is stored on-disk by each node, such that each node may be restarted at any time (purposely, or via automatic failover) and know what its place in the cluster was before it terminated, without needing immediate access to the rest of the cluster.

* **ring_creation_size**
    The number of partitions to divide the hash space into (default: `64`)

    By default, each Riak node will own ring_creation_size/(number of nodes in the cluster) partitions. It is generally a good idea to specify a "ring_creation_size" a few times the number of nodes in your cluster (e.g. specify 64-256 partitions for a 4-node cluster). This gives you room to expand the number of nodes in the cluster, without worrying about under-use due to owning too few partitions. This number should be a power of 2 (64, 128, 256, etc.).

    {{#1.4.0-}}
    <div class="info">
    <div class="title">Ring Size Tip</div>
    The `ring_creation_size` should be established before your cluster is started, and should not be changed thereafter.
    </div>
    {{/1.4.0-}}

* **ssl**
You can override the default SSL key and certificate settings (default: etc/cert.pem, etc/key.pem)

* **target_n_val**
    The highest n_val that you generally intend to use. This affects how partitions are distributed amongst the cluster and how preflists are calculated, helping ensure that data is never stored to the same physical node more than once. You will only need to change this setting in rare circumstances.

    Assuming ring_creation_size is a power of 2, the ideal value for this setting is both greater than or equal to the largest n_val of any bucket, and an even divisor of the number of partitions in your ring (ring_creation_size).
    The default value is 4. For this to be effective at preventing hot spots, your cluster size (number of physical nodes) must be equal to or larger than target_n_val.

* **vnode_management_timer** (milliseconds) By default Riak checks every 10
  seconds for primary partitions which need to be transferred. This frequency
  can be changed by specifying a value in milliseconds. {{1.1.2+}}

* **wants_claim_fun**
{Module, Function} that returns boolean - true if this node wants to claim more vnodes.

* **enable_health_checks** `true` or `false`. `true` if all health checks should be enabled. {{1.3.0+}}

* **stat_cache_ttl**
    {{#1.2.0-1.3.1}}The time-to-live in seconds for stats in the cache. If stats are requested from the cache and they're older than TTL seconds, they will be calculated. (default: `1`){{/1.2.0-1.3.1}}
    {{#1.3.2+}}The interval, in seconds, between stat cache population runs. (default: `1`)
    All Riak stats are served from the stat cache. This setting controls how frequently that cache is refreshed. {{/1.3.2+}}

### riak_kv settings

 * **anti_entropy**
    Enable active anti-entropy subsystem + optional debug messages

    `{anti_entropy, {on|off, []}},`

    `{anti_entropy, {on|off, [debug]}},`

 * **anti_entropy_build_limit**
    Restrict how fast AAE can build hash trees. Building the tree for a given partition requires a full scan over that partition's data. Once built, trees stay built until they are expired. The format is `{number-of-builds, per-timespan-in-milliseconds}`.

    `{anti_entropy_build_limit, {1, 3600000}},`

 * **anti_entropy_expire**
    Determine how often hash trees are expired after being built. Periodically expiring a hash tree ensures the on-disk hash tree data stays consistent with the actual k/v backend data. It also helps Riak identify silent disk failures and bit rot. However, expiration is not needed for normal AAE operation and should be infrequent for performance reasons. The time is specified in milliseconds. The default is 1 week.

    `{anti_entropy_expire, 604800000},`

 * **anti_entropy_concurrency**
    Limit how many AAE exchanges/builds can happen concurrently.

    `{anti_entropy_concurrency, 2},`

 * **anti_entropy_tick**
    The tick determines how often the AAE manager looks for work to do (building/expiring trees, triggering exchanges, etc). The default is every 15 seconds. Lowering this value will speedup the rate that all replicas are synced across the cluster. Increasing the value is not recommended.

    `{anti_entropy_tick, 15000},`

 * **anti_entropy_data_dir**
    The directory where AAE hash trees are stored.

    `{anti_entropy_data_dir, "./data/anti_entropy"},`

 * **anti_entropy_leveldb_opts**
    The LevelDB options used by AAE to generate the LevelDB-backed on-disk hashtrees.

    `{anti_entropy_leveldb_opts, [{write_buffer_size, 4194304}, {max_open_files, 20}]},`

 * **add_paths** A list of paths to add to the Erlang code path.

    This setting is especially useful for allowing Riak to use external modules during MapReduce queries.

 * **delete_mode** Specifies behavior for the window of time between Riak
identifying an object for deletion and actual deletion of the object.
There are three modes of operation: `delay` (in milliseconds),
`immediate`, and `keep`. Delay of 3 seconds is the default mode. Setting
delete_mode to immediate removes the tombstone for the object when the delete request is received. Setting delete_mode to keep disables tombstone
removal altogether.

 * **mapred_name**
The base of the path in the URL exposing MapReduce via HTTP. (default: `mapred`)

 * **mapred_queue_dir**
The directory used to store a transient queue for pending map tasks. Only valid when mapred_system is set to legacy. (default: `data/mrqueue`) {{1.3.0+}}

 * **mapred_system**
Indicates which version of the MapReduce system should be used: 'pipe' means riak_pipe will power MapReduce queries, while 'legacy' means that luke will be used. (default: `pipe`) {{1.3.0+}}

 * **map_js_vm_count**
The number of Javascript VMs started to handle map phases. (default: `8`)

 * **reduce_js_vm_count**
The number of Javascript VMs started to handle reduce phases. (default: `6`)

 * **hook_js_vm_count**
The number of Javascript VMs started to handle pre-commit hooks.(default: `2`)

 * **mapper_batch_size**
Number of items the mapper will fetch in one request. Larger values can impact read/write performance for non-MapReduce requests. Only valid when mapred_system is legacy (default: `5`) {{1.3.0+}}

 * **js_max_vm_mem**
The maximum amount of memory allocated to each Javascript virtual machine, in megabytes. (default: `8`)

 * **js_thread_stack**
The maximum amount of thread stack space to allocate to Javascript virtual machines, in megabytes. (default: `16`)

 * **map_cache_size**
Number of objects held in the MapReduce cache. These will be ejected when the cache runs out of room or the bucket/key pair for that entry changes. Only valid when mapred_system is legacy. (default: `10000`) {{1.3.0+}}

 * **js_source_dir**
Where to load user-defined built in Javascript functions (default: unset)

 * **http_url_encoding**
Determines how Riak treats URL encoded buckets, keys, and links over the REST API. When set to on Riak always decodes encoded values sent as URLs and Headers. Otherwise, Riak defaults to compatibility mode where links are decoded, but buckets and keys are not. The compatibility mode will be removed in a future release. (default: `off`)

 * **vnode_vclocks**
When set to true uses vnode-based vclocks rather than client ids. This significantly reduces the number of vclock entries. Only set true if all nodes in the cluster are upgraded to 1.0. (default: `false`)

 * **legacy_keylisting**
This option enables compatibility of bucket and key listing with 0.14 and earlier versions. Once a rolling upgrade to a version >= 1.0 is completed for a cluster, this should be set to false for improved performance for bucket and key listing operations. (default: `true`)

 * **pb_ip** The IP address that the Protocol Buffers interface will bind to. (default: `"127.0.0.1"`)

 If not set, the PBC interface will not be started. {{1.2.0-}}

 * **pb_port** The port that the Protocol Buffers interface will bind to. (default: `8087`) {{1.2.0-}}

 * **pb_backlog** The maximum length to which the queue of pending connections may grow. If set, it must be an integer >= 0. If you anticipate a huge number of connections being initialized simultaneously, set this number higher. (default: `5`) {{1.2.0-}}

 * **raw_name**
    The base of the path in the URL exposing Riak's HTTP interface (default: `riak`)

    The default value will expose data at `/riak/Bucket/Key`. For example, changing this setting to "bar" would expose the interface at `/bar/Bucket/Key`.

 * **riak_kv_stat**
    Enables the statistics-aggregator (`/stats` URL and riak-admin status command) if set to true. (default is `true`)

 * **stats_urlpath**
    The base of the path in the URL exposing the statistics-aggregator. (default: `stats`)

 * **storage_backend**
    The module name of the storage backend that Riak should use. (default: `riak_kv_bitcask_backend`)

    Riak will refuse to start if no storage backend is specified. Available backends:

    * `riak_kv_bitcask_backend` - Data is stored in Bitcask append-only storage. See the Bitcask configuration page for more information.
    * `riak_kv_eleveldb_backend` - Data is stored in LevelDB. See the LevelDB configuration page for more information.
    * `riak_kv_memory_backend` - A backend that behaves as an LRU-with-timed-expiry cache. Read the Memory backend configuration page for more information.
    * `riak_kv_multi_backend` - Enables storing data for different buckets in different backends. See the Multi configuration page for more details.

 * **riak_search**
Riak Search is now enabled via the `app.config`. To enable it in your app, simply set it to `true` in Riak Search Config section (shown below).

 * **vnode_mr_timeout**
How long a map function is permitted to execute on a vnode before it times out and is retried on another vnode, in milliseconds. (default: `1000`)

 * **vnode_mailbox_limit** `{EnableThreshold, DisableThreshold}` - configures the riak_kv health check that monitors message queue lengths of riak_kv vnodes. If a KV vnode's message queue length reaches `DisableThreshold` the `riak_kv` service is disabled on the node. The service will not be re-enabled until the message queue length drops below `EnableThreshold`. {{1.3.0+}}

 * **secondary_index_timeout**
 The number of milliseconds before a secondary index query times out. The default value is `0`, which indicates that no timeout will occur. {{1.4.1+}}

### webmachine_logger_module
This needs to be set in order to enable access logs.

`{webmachine, [{webmachine_logger_module, webmachine_logger}]}`

<div class="info">
<div class="title">Note</div>
The additional disk I/O of an access log imposes a performance cost you may not wish to pay. Therefore, by default, Riak does not produce access logs.
</div>
### riak_search settings

 * **enabled**
Enable Search functionality. (default: `false`)

 * **max_search_results**
Maximum number of results to accumulate before erroring. (Prevent, reduce memory exhaustion that could bring down the entire VM.) (default: `100000`)

```erlang
%% Riak Search Config
{riak_search, [
    %% To enable Search functionality set this 'true'.
    {enabled, false}
    ]},
```

### lager

Lager is the logging engine introduced in Riak 1.0. It is designed to be more stable than Erlang's error_logger, as well as play nicely with standard logging tools.

  * **async_threshold** 
The maximum number of log messages to be queued in asynchronous mode before switching to synchronous mode. (default: 20)

  * **async_threshold_window**
How far below the `async_threshold` the log message queue must be before re-enabling asynchronous mode. (default: 5)

    Prior to lager 2.0, the gen_event at the core of lager operated purely in synchronous mode. Asynchronous mode is faster, but has no protection against message queue overload. In lager 2.0, the gen_event takes a hybrid approach. it polls its own mailbox size and toggles the messaging between synchronous and asynchronous depending on mailbox size.

```erlang
{async_threshold, 20},
{async_threshold_window, 5}
```

    This will use async messaging until the mailbox exceeds 20 messages, at which point synchronous messaging will be used, and switch back to asynchronous, when size reduces to 20 - 5 = 15.

    If you wish to disable this behaviour, simply set it to 'undefined'. It defaults to a low number to prevent the mailbox growing rapidly beyond the limit and causing problems. In general, lager should process messages as fast as they come in, so getting 20 behind should be relatively exceptional anyway.

 * **colored**
Enable color-coding message logged to the console by level. Requires  Erlang >=R16 (default: false)

 * **colors**
Configure the colors to use for console messages, using ANSI escape sequences.  The default colors are:

```erlang
{colors, [
    {debug,     "\e[0;38m" },
    {info,      "\e[1;37m" },
    {notice,    "\e[1;36m" },
    {warning,   "\e[1;33m" },
    {error,     "\e[1;31m" },
    {critical,  "\e[1;35m" },
    {alert,     "\e[1;44m" },
    {emergency, "\e[1;41m" }
]}
```

 * **crash_log**
Whether to write a crash log, and where. (default: no crash logger)
   
 * **crash_log_count**
Number of rotated crash logs to keep, 0 means keep only the current one - default is 0

 * **crash_log_date**
What time to rotate the crash log - default is no time-based rotation.
    {crash_log_date, "$D0"},
The syntax for the value field is taken from the `when` section of [newsyslog.conf]( http://www.freebsd.org/cgi/man.cgi?query=newsyslog.conf&sektion=5)

    Day, week and month time format: The lead-in character
    for day, week and month specification is a `$'-sign.
    The particular format of day, week and month
    specification is: [Dhh], [Ww[Dhh]] and [Mdd[Dhh]],
    respectively.  Optional time fields default to
    midnight.  The ranges for day and hour specifications are:

    -  hh      hours, range 0 ... 23
    -  w       day of week, range 0 ... 6, 0 = Sunday
    -  dd      day of month, range 1 ... 31, or the
    -  letter L or l to specify the last day of the month.
    - Some examples:  
         $D0 -  rotate every night at midnight  
         $D23 - rotate every day at 23:00 hr  
         $W0D23 - rotate every week on Sunday at 23:00 hr  
         $W5D16 - rotate every week on Friday at 16:00 hr  
         $M1D0 -  rotate on the first day of every month at midnight (i.e., the start of the day)  
         $M5D6 -  rotate on every 5th day of the month at 6:00 hr

 * **crash_log_msg_size**
Maximum size in bytes of events in the crash log. (default: `65536`)

 * **crash_log_size**
Maximum size of the crash log in bytes, before it is rotated. Set to zero to
disable rotation. (default: `0`)

 * **error_logger_hwm**
Maximum number of messages per second allowed from error_logger.  Permits weathering a flood of messages when many related processes crash.

 * **error_logger_redirect**
Whether to redirect error_logger messages into lager. (default: true)

```erlang
{error_logger_hwm, 200}
```

 * **handlers**
Allows the selection of log handlers with differing options.

* lager_console_backend - Logs to the the console, with the specified log level.
* lager_file_backend - Logs to the given list of files, each with their own log level.

    Lager can rotate its own logs or have it done via an external process. To use internal rotation, use the 'size', 'date' and 'count' values in the file backend's config:
    See crash_log_date above for a description of the date field.

```erlang
[{name, "error.log"}, {level, error}, {size, 10485760}, {date, "$D0"}, {count, 5}]
```

 * **error_logger_redirect**
Whether to redirect sasl error_logger messages into lager. (default: `true`)

 * **traces**
Traces can be configured at startup by adding the following to the lager configuration section.
Refer to [Lager Tracing](https://github.com/basho/lager#tracing) for more information.

```erlang
{traces,[
   {handler1,filter1,level1},
   ...
   {handler2,filter2,level2}
]},
```



_The default lager options are like so:_

```erlang
{lager, [
    {handlers, [
        {lager_console_backend, info},
        {lager_file_backend, [
            {"/opt/riak/log/error.log", error},
            {"/opt/riak/log/console.log", info}
        ]},
     ]},
    {crash_log, "{{platform_log_dir}}/crash.log"},
    {crash_log_msg_size, 65536},
    {crash_log_size, 0},
    {crash_log_count, 0},
    {error_logger_redirect, true},
    {error_logger_hwm, 50},
    {async_threshold, 20},
    {async_threshold_window, 5}
    {colored, false}
]},
```

## vm.args

Parameters for the Erlang node on which Riak runs are set in the vm.args file in the etc directory of the embedded Erlang node. Most of these settings can be left at their defaults until you are ready to tune performance.

Two settings you may be interested in right away, though, are `-name` and `-setcookie`. These control the Erlang node names (possibly host-specific), and Erlang inter-node communication access (cluster-specific), respectively.

The format of the file is fairly loose: lines which do not begin with the "#" character are concatenated, and passed to the erl on the command line as is.

More details about each of these settings can be found in the Erlang documentation for the erl Erlang virtual machine.

Riak CS and Enterprise may make different choices for some of these;
please rely on the `vm.args` file supplied with those packages.

#### -name
Name of the Erlang node. (default: `riak@127.0.0.1`)

The default value, `riak@127.0.0.1`, will work for running Riak locally, but for distributed (multi-node) use, the portion of the name after the "@" should be changed to the IP address of the machine on which the node is running.

If you have properly-configured DNS, the short-form of this name can be used (for example: `riak`). The name of the node will then be `riak@Host.Domain`.

#### -setcookie
Cookie of the Erlang node. (default: `riak`)

Erlang nodes grant or deny access based on the sharing of a previously-shared cookie. You should use the same cookie for every node in your Riak cluster, but it should be a not-easily-guessed string unique to your deployment, to prevent non-authorized access.

#### -heart
Enable "heart" node monitoring. (default: `disabled`)

Heart will restart nodes automatically, should they crash. However, heart is so good at restarting nodes that it can be difficult to prevent it from doing so. Enable heart once you are sure that you wish to have the node restarted automatically on failure.

#### +K
Enable kernel polling. (default: `true`)

#### +A
Number of threads in the async thread pool. (default: `64`)

#### -pa
Adds the specified directories to the beginning of the code path,
similar to
[`code:add_pathsa/1`](http://www.erlang.org/doc/man/code.html#add_pathsa-1). As
an alternative to `-pa`, if several directories are to be prepended to
the code and the directories have a common parent directory, that
parent directory could be specified in the `ERL_LIBS` environment
variable.

#### -env
Set host environment variables for Erlang.

#### -smp
Enables Erlang's SMP support. (default: `enable`)

#### +zdbbl
Configures the buffer size for outbound messages between nodes. This
is commented out by default because the ideal value varies
significantly depending on available system memory, typical object
size, and amount of traffic to the database. (default: `1024` unless
configured in `vm.args`, `32768` is the commented-out value)

Systems with lots of memory and under a heavy traffic load should
consider increasing our default value; systems under lighter load but
storing large objects may wish to lower it. [[Basho Bench]] is highly
recommended to help determine the best values for this (and other
tuning parameters) in your environment.

#### +P
Defines the Erlang process limit. Under the versions of Erlang
supported by Riak through 1.4.x, the limit is very low, and thus using
this to raise the limit is very important. (default: `256000`)

**Note**: For anyone concerned about configuring such a high value, be
aware that Erlang processes are not the same as system processes. All
of these processes will exist solely inside a single system process,
the Erlang beam.

#### +sfwi
If using an
[appropriately patched Erlang VM](https://gist.github.com/evanmcc/a599f4c6374338ed672e)
(such as one downloaded directly from Basho) this will control the
interval (in milliseconds) at which a supervisor thread wakes to check
run queues for work to be executed. (default: `500`)

#### +W
Determines whether warning messages sent to Erlang's `error_logger`
are treated as errors, warnings, or informational. (default: `w` for
warnings)

#### -env ERL_LIBS
Alternate method to add directories to the code path (see `-pa` above)

#### -env ERL_MAX_PORTS

Maximum number of concurrent ports/sockets. (default: `64000`)

**Note**: As with processes, Erlang ports and system ports are similar
but distinct.

#### -env ERL_FULLSWEEP_AFTER

Run garbage collection more often. (default: `0`)

#### -env ERL_CRASH_DUMP

Set the location of crash dumps. (default: `./log/erl_crash.dump`)

## Rebar Overlays

If you are going to be rebuilding Riak often, you will want to edit the
`vm.args` and `app.config` files in the `rel/files` directory. These files are
used whenever a new release is generated using "make rel" or "rebar generate".
Each time a release is generated any existing release must first be destroyed.
Changes made to release files (`rel/riak/etc/vm.args`,
`rel/riak/etc/app.config`, etc.) would be lost when the release is destroyed.
