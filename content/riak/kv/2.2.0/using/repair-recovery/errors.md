---
title: "Errors & Messages"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Errors"
    identifier: "repair_recover_errors"
    weight: 101
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/2.2.0/ops/running/recovery/errors
  - /riak/kv/2.2.0/ops/running/recovery/errors
---

[config reference]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference

This is not a comprehensive listing of every error that Riak may
encounter -- screws fall out all of the time, the world is an imperfect
place. This is an attempt at capturing the most common recent errors
that users do encounter, as well as give some description to non
critical error atoms which you may find in the logs.

Discovering the source of an error can take some detective work, since
one error can cause a cascade of errors.

The tables in this document do not specify which logs these error
messages may appear in. Depending upon your log configuration some may
appear more often (i.e., if you set the log to debug), while others may
output to your console (eg. if you tee'd your output or started as `riak
console`).

You can optionally customize your log message format via the
`lager_default_formatter` field under `lager` in `app.config`. If you
do, your messages will look different from those shown in this document.

Finally, this document is organized to be able to lookup portions of a
log message, since printing every variation would be a bit unwieldy. For
example, this message:

```
12:34:27.999 [error] gen_server riak_core_capability terminated with reason:\
no function clause matching orddict:fetch('riak@192.168.2.81', []) line 72
```

Starts with a date (`12:34:27.999`), followed by the log severity
(`[error]`), with a message formatted by lager (found in the Lager table
below as *gen_server `Mod` terminated with reason: `Reason`*)

### Lager Formats

Riak's main logging mechanism is the project Lager, so it's good to note
some of the more common message formats. In almost every case the
reasons for the error are described as variables, such as `Reason` of
`Mod` (meaning the Erlang module which is generally the source of the
error).

Riak does not format all error messages that it receives into
human-readable sentences. However, It does output errors as objects.

The above example error message corresponds with the first message in
this table, where the Erlang `Mod` value is `riak_core_capability` and
the reason was an Erlang error: `no function clause matching
orddict:fetch('riak@192.168.2.81', []) line 72`.

Error | Message
------|--------
 | `gen_server <Mod> terminated with reason: <Reason>`
 | `gen_fsm <Mod> in state <State> terminated with reason: <Reason>`
 | `gen_event <ID> installed in <Mod> terminated with reason: <Reason>`
`badarg` | `bad argument in call to <Mod1> in <Mod2>`
`badarith` | `bad arithmetic expression in <Mod>`
`badarity` | `fun called with wrong arity of <Ar1> instead of <Ar2> in <Mod>`
`badmatch` | `no match of right hand value <Val> in <Mod>`
`bad_return` | `bad return value <Value> from <Mod>`
`bad_return_value` | `bad return value: <Val> in <Mod>`
`badrecord` | `bad record <Record> in <Mod>`
`case_clause` | `no case clause matching <Val> in <Mod>`
`emfile` | `maximum number of file descriptors exhausted, check ulimit -n`
`function_clause` | `no function clause matching <Mod>`
`function not exported` | `call to undefined function <Func> from <Mod>`
`if_clause` | `no true branch found while evaluating if expression in <Mod>`
`noproc` | `no such process or port in call to <Mod>`
`{system_limit, {erlang, open_port}}` | `maximum number of ports exceeded`
`{system_limit, {erlang, spawn}}` | `maximum number of processes exceeded`
`{system_limit, {erlang, spawn_opt}}` | `maximum number of processes exceeded`
`{system_limit, {erlang, list_to_atom}}` | `tried to create an atom larger than 255, or maximum atom count exceeded`
`{system_limit, {ets, new}}` | `maximum number of Erlang Term Storage (ETS) tables exceeded`
`try_clause` | `no try clause matching <Val> in <Mod>`
`undef` | `call to undefined function <Mod>`

### Error Atoms

Since Erlang programming support is a "happy path/fail fast" style, one
of the more common error log strings you might encounter contain
`{error,{badmatch,{...`.  This is Erlang's way of telling you that an
unexpected value was assigned, so these errors can prefix the more
descriptive parts. In this case, `{error,{badmatch,{...` prefixes the
more interesting `insufficient_vnodes_available` error, which can be
found in the `riak_kv` table later on in this document.

```log
2012-01-13 02:30:37.015 [error] <0.116.0> webmachine error: path="/riak/contexts"\
{error,{error,{badmatch,{error,insufficient_vnodes_available}},\
[{riak_kv_wm_keylist,produce_bucket_body,2},{webmachine_resource,resource_call,3},\
{webmachine_resour,resource_call,1},{webmachine_decision_core,decision,1},\
{webmachine_decision_core,handle_request,2},\
{webmachine_mochiweb,loop,1},{mochiweb_http,headers,5}]}}
```

## Erlang Errors

Although relatively rare once a Riak cluster is running in production,
users new to Riak or Erlang occasionally encounter errors on initial
installation. These spring from a setup Erlang does not expect,
generally due to network, permission, or configuration problems.

Error | Description | Resolution
:-----|:------------|:----------
`{error,duplicate_name}` | You are trying to start a new Erlang node, but another node with the same name is already running | You might be attempting to start multiple nodes on the same machine with the same `vm.args` `-name` value; or if Riak is already running, check for `beam.smp`; or epmd thinks Riak is running, check/kill epmd
`{error,econnrefused}` | Remote Erlang node connection refused | Ensure your cluster is up and nodes are able to communicate with each other. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more">Step 1</a>.
`{error,ehostunreach}` | Remote node cannot be connected to | Ensure that nodes are able to communicate with each other. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more">Step 1</a>.
`{error,eacces}` | Cannot write a given file | Ensure the Riak beam process has permission to write to all `*_dir` values in `app.config`, for example, `ring_state_dir`, `platform_data_dir`, and others
`{error,enoent}` | Missing an expected file or directory | Ensure all `*_dir` values in `app.config` exist, for example, `ring_state_dir`, `platform_data_dir`, and others
`{error,erofs}` | A file/directory is attempted to be written to a read-only filesystem | Only set Riak directories to read/write filesystems
`system_memory_high_watermark` | Often a sign than an <a href="http://www.erlang.org/doc/man/ets.html">ETS table</a> has grown too large | Check that you are using a backend appropriate for your needs (LevelDB for very large key counts) and that your vnode count is reasonable (measured in dozens per node rather than hundreds)
`temp_alloc` | Erlang attempting to allocate memory | Often associated with `Cannot allocate X bytes of memory`, which means that you're either creating too large of an object or that you simply don't have enough RAM. Base minimum suggested RAM per node is 4GB.

## Riak Errors and Messages

Many KV errors have prescriptive messages. For such cases we leave it to
Riak to explain the correct course of action. For example, the
`map/reduce` `parse_input` phase will respond like this when it
encounters an invalid input:

{{% note title="Note on inputs" %}}
Inputs must be a binary bucket, a tuple of bucket and key-filters, a list of
target tuples, a search index, or modfun tuple: `INPUT`.
{{% /note %}}

For the remaining common error codes, they are often marked by Erlang
atoms (and quite often wrapped within an `{error,{badmatch,{...` tuple,
as described in the [Error](#erlang-errors) section
above). This table lays out those terse error codes and related log
messages, if they exist.

### Riak Core

Riak Core is the underlying implementation for KV. These are errors
originating from that framework, and can appear whether you use KV,
Search, or any Core implementation.

Error | Message | Description | Resolution
:-----|:--------|:------------|:----------
`behavior` | | Attempting to execute an unknown behavior | Ensure that your configuration file choices (e.g. backends) support the behaviors you're attempting to use, such as configuring LevelDB to use secondary indexes
`already_leaving` | `Node is already in the process of leaving the cluster` | An error marking a node to leave when it is already leaving | No need to duplicate the `leave` command
`already_replacement` |  | This node is already in the replacements request list | You cannot replace the same node twice
`{different_owners, N1, N2}` |  | Two nodes list different partition owners, meaning the ring is not ready | When the ring is ready, the status should be ok
`different_ring_sizes` |  | The joining ring is a different size from the existing cluster ring | Don't join a node already joined to a cluster
`insufficient_vnodes_available` |  | When creating a query coverage plan, not enough vnodes are available | Check the `riak-admin ring-status` and ensure all of your nodes are healthy and connected
`invalid_replacement` |  | A new node is currently joining from a previous operation, so a replacement request is invalid until it is no longer joining | Wait until the node is finished joining
`invalid_ring_state_dir` | `Ring state directory <RingDir> does not exist, and could not be created: <Reason>` | The ring directory does not exist and no new dir can be created in expected location | Ensure that the Erlang proc can write to `ring_state_dir`or has permission to create that dir
`is_claimant` |  | A node cannot be the claimant of its own remove request | Remove/replace nodes from another node
`is_up` |  | Node is expected to be down but is up | When a node is downed, it should be down
`legacy` |  | Attempting to stage a plan against a legacy ring | Staging is a feature only of Riak versions 1.2.0+
`max_concurrency` | `Handoff receiver for partition <Partition> exited abnormally after processing <Count> objects: <Reason>` | Disallow more handoff processes than the `riak_core` `handoff_concurrency` setting (defaults to 2) | If this routinely kills vnodes, this issue has been linked to LevelDB compactions which can build up and block writing, which will also be accompanied by LevelDB logs saying `Waiting...` or `Compacting`
`{nodes_down, Down}` |  | All nodes must be up to check |
`not_member` |  | This node is not a member of the ring | Cannot leave/remove/down when this is not a ring member
`not_reachable` |  | Cannot join unreachable node | Check your network connections, ensure Erlang cookie setting `vm.args` `-setcookie`
`{not_registered, App}` |  | Attempting to use an unregistered process | Ensure that your `app.config` choices contain the app you're attempting to use `{riak_kv_stat, true}`
`not_single_node` |  | There are no other members to join | Join with at least one other node
`nothing_planned` |  | Cannot commit a plan without changes | Ensure at least one ring change is planned before running commit
`only_member` |  | This is the only member of the ring | Cannot leave/remove/down when this is the only member of the ring
`ring_not_ready` |  | Ring not ready to perform command | Attempting to plan a ring change before the ring is ready to do so
`self_join` |  | Cannot join node with itself | Join another node to form a valid cluster
`timeout` | `<Type> transfer of <Module> from <SrcNode> <SrcPartition> to <TargetNode> <TargetPartition> failed because of TCP recv timeout` |  | Ensure that ports chosen in your configuration files do not overlap with ports being used by your system, or with each other
`unable_to_get_join_ring` |  | Cannot access cluster ring to join | Possible corrupted ring
`{unknown_capability, Capability}` |  | Attempting to use a capability unsupported by this implementation | Ensure that your configuration choices support the capability you're attempting to use, such as Pipe MapReduce (setting a `mapred_2i_pipe` value in `app.config`)
`vnode_exiting` | `<Mod> failed to store handoff obj: <Err>` |  | A vnode fails to hand off data because the handoff state is deleted
`vnode_shutdown` |  | The vnode worker pool is shutting down | Various reasons can cause a shutdown, check other log messages
 | `Bucket validation failed <Detail>` |  | Only set value bucket properties
 | `set_recv_data called for non-existing receiver` | Cannot connect to receiver during handoff | Ensure receiver node is still up and running, and that the standard
 | `An <Dir> handoff of partition <M> was terminated because the vnode died` | Handoff stopped because of vnode was `DOWN` and sender must be killed | An expected message if a vnode dies during handoff. Check the logs for other causes.
 | `status_update for non-existing handoff <Target>` | Cannot get the status of a handoff `Target` module that doesn't exist | An expected message. Check the logs for other causes.
 | `SSL handoff config error: property <FailProp>: <BadMat>.` | The receiver may reject the senders attempt to start a handoff | Ensure your SSL settings and certificates are proper
 | `Failure processing SSL handoff config <Props>:<X>:<Y>` |  | Ensure your SSL settings and certificates are proper
 | `<Type> transfer of <Module> from <SrcNode> <SrcPartition> to <TargetNode> <TargetPartition> failed because of <Reason>` | Nodes cannot hand off data | Ensure that your cluster is up and nodes are able to communicate with each other. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more"> Step 1</a>.
 | `Failed to start application: <App>` | Expected application cannot load | This relates to an Erlang application, and not necessarily the Riak application in general. The app may fail to load for many reasons, such as a missing native library. Read other log messages for clues
 | `Failed to read ring file: <Reason>` | Gives a reason why the ring file cannot be read on startup | The reason given explains the problem, such as `eacces` meaning the Erlang process does not have permission to read
 | `Failed to load ring file: <Reason>` | Gives a reason why the ring file cannot be loaded on startup | The reason given explains the problem, such as `enoent` meaning the expected file cannot be found
 | `ring_trans: invalid return value: <Other>` | Transferring ring data between nodes received an invalid value | Often associated with ring corruption, or an unexpected exit from the transferring node
 | `Error while running bucket fixup module <Fixup> from application <App> on bucket <BucketName>: <Reason>` |  | Various sources for a fixup error, read associated errors
 | `Crash while running bucket fixup module <Fixup> from application <App> on bucket <BucketName> : <What>:<Why>` |  | Various source for a fixup error, read associated errors
 | `<Index> <Mod> worker pool crashed <Reason>` |  | Various reasons can be the source of a worker pool crash, read associated errors
 | `Received xfer_complete for non-existing repair: <ModPartition>` | Unexpected repair message | Not much to do here, but a node did not expect to receive a `xfer_complete` status

### Riak KV

Riak KV is the key/value implementation, generally just considered to be
Riak proper. This is the source of most of the code, and consequently,
most of the error messages.

Error | Message | Description | Resolution
:-----|:--------|:------------|:----------
`all_nodes_down` |  | No nodes are available | Check `riak-admin member-status` and ensure that all expected nodes in the cluster are of `valid` Status
`{bad_qterm, QueryTerm}` |  | Bad query when performing MapReduce | Fix your MapReduce query
`{coord_handoff_failed, Reason}` | `Unable to forward put for <Key> to <CoordNode> - <Reason>` | Vnodes unable to communicate | Check that coordinating vnode is not down. Ensure your cluster is up and nodes are able to communicate with each other. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more"> Step 1</a>.
`{could_not_reach_node, Node}` |  | Erlang process was not reachable | Check network settings; ensure remote nodes are running and reachable; ensure all nodes have the same Erlang cookie setting `vm.args` `-setcookie`. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more"> Step 1</a>.
`{deleted, Vclock}` |  | The value was already deleted, includes the current vector clock | Riak will eventually clean up this tombstone
`{dw_val_violation, DW}` |  | Same as `w_val_violation` but concerning durable writes | Set a valid DW value
`{field_parsing_failed, {Field, Value}}` | `Could not parse field
<Field>, value <Value>.` | Could not parse an index field | Most commonly an `_int` field which cannot be parsed. For example a query like this is invalid: `/buckets/X/index/Y_int/BADVAL`, since BADVAL should instead be an integer
`{hook_crashed, {Mod, Fun, Class, Exception}}` | `Problem invoking pre-commit hook` | Precommit process exited due to some failure | Fix the precommit function code, follow the message's exception and stacktrace to help debug
`{indexes_not_supported, Mod}` |  | The chosen backend does not support indexes (only LevelDB currently supports secondary indexes) | Set your configuration to use the LevelDB backend
`{insufficient_vnodes, NumVnodes, need, R}` |  | R was set greater than the total vnodes | Set a proper R value; or too many nodes are down; or too many nodes are unavailable due to crash or network partition. Ensure all nodes are available by running riak-admin ring-status.
`{invalid_hook_def, HookDef}` | `Invalid post-commit hook definition <Def>` | No Erlang module and function or JavaScript function name | Define the hook with the correct settings
`{invalid_inputdef, InputDef}` |  | Bad inputs definitions when running MapReduce | Fix inputs settings; set `mapred_system` from `legacy` to `pipe`
`invalid_message` | | Unknown event sent to module | Ensure you're running similar versions of Riak across (and specifically poolboy) across all nodes
`{invalid_range, Args}` |  | Index range query hasStart > End | Fix your query
`{invalid_return, {Mod, Fun, Result}}` | `Problem invoking pre-commit hook <Mod>:<Fun>, invalid return <Result>` | The given precommit function gave an invalid return for the given `Result` | Ensure your pre-commit functions return a valid result
`invalid_storage_backend` | `storage_backend <Backend> is non-loadable.` | Invalid backend choice when starting up Riak | Set a valid backend in your configuration files
`key_too_large` |  | The key was larger than 65536 bytes | Use a smaller key
`local_put_failed` |  | A local vnode PUT operation failed | This has been linked to a LevelDB issue related to restricted memory usage and inability to flush a write to disk.  If this happens repetitively, stop/start the riak node, forcing a memory realloc
`{n_val_violation, N}` |  | (W > N) or (DW > N) or (PW > N) or (R > N) or (PR > N) | No W or R values may be greater than N
`{nodes_not_synchronized, Members}` |  | Rings of all members are not synchronized | Backups will fail if nodes are not synchronized
`{not_supported, mapred_index, FlowPid}` |  | Index lookups for MapReduce are only supported with Pipe | Set mapred_system from legacy to pipe
`notfound` |  | No value found | Value was deleted, or was not yet stored or replicated
`{pr_val_unsatisfied, PR, Primaries}` |  | Same as `r_val_unsatisfied` but only counts `Primary` node replies | Too many primary nodes are down or the `PR` value was set too high
`{pr_val_violation, R}` |  | Same as `r_val_violation` but concerning `Primary` reads | Set a valid `PR` value
`precommit_fail` | `Pre-commit hook <Mod>:<Fun> failed with reason <Reason>` | The given precommit function failed for the given `Reason` | Fix the precommit function code
`{pw_val_unsatisfied, PR, Primaries}` | | Same as `w_val_unsatisfied` but only counts `Primary` node replies | Too many primary nodes are down or the `PW` value was set too high
`{pw_val_violation, PW}` |  | Same as `w_val_violation` but concerning primary writes | Set a valid `PW` value
`{r_val_unsatisfied, R, Replies}` |  | Not enough nodes replied to satisfy the `R` value, contains the given `R` value and the actual number of `Replies` | Too many nodes are down or the R value was set too high
`{r_val_violation, R}` |  | The given R value was non-numeric and not a valid setting (`on`, `all`, `quorum`) | Set a valid R value
`receiver_down` |  | Remote process failed to acknowledge request | Can occur when listkeys is called
`{rw_val_violation, RW}` |  | The given `RW` property was non-numeric and not a valid setting (`one`, `all`, `quorum`) | Set a valid `RW` value
`{siblings_not_allowed, Object}` | `Siblings not allowed: <Object>` | The hook to index cannot abide siblings | Set the buckets `allow_mult` property to `false`
`timeout`|  | The given action took too long to reply | Ensure your cluster is up and nodes are able to communicate with each other. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more"> Step 1</a>. Or check you have a reasonable `ulimit` size. Note that listkeys commands can easily timeout and shouldn't be used in production.
`{too_few_arguments, Args}` |  | Index query requires at least one argument | Fix your query format
`{too_many_arguments, Args}` |  | Index query is malformed with more than 1 (exact) or 2 (range) values | Fix your query format
`too_many_fails` |  | Too many write failures to satisfy W or DW | Try writing again. Or ensure your nodes/network is healthy. Or set a lower W or DW value
`too_many_results` | | Too many results are attempted to be returned | This is a protective error. Either change your query to return fewer results, or change your `max_search_results` setting in `app.config` (it defaults to 100,000)
`{unknown_field_type, Field}` | `Unknown field type for field: <Field>.` | Unknown index field extension (begins with underscore) | The only value field types are `_int` and `_bin`
`{w_val_unsatisfied, RepliesW, RepliesDW, W, DW}` | | Not enough nodes replied to satisfy the W value, contains the given W value and the actual number of `Replies*` for either `W` or `DW` | Too many nodes are down or the `W` or `DW` value was set too high
`{w_val_violation, W}` |  | The given W property was non-numeric and not a valid setting (on, all, quorum) | Set a valid W value
 | `Invalid equality query <SKey>` | Equality query is required and must be binary for an index call | Pass in an equality value when performing a 2i equality query
 | `Invalid range query: <Min> -> <Max>` | Both range query values are required and must be binary an index call | Pass in both range values when performing a 2i equality query
 | `Failed to start <Mod> <Reason>:<Reason>` | Riak KV failed to start for given `Reason` | Several possible reasons for failure, read the attached reason for insight into resolution

### Backend Errors

These errors tend to stem from server-based problems. Backends are
sensitive to low or corrupt disk or memory resources, native code, and
configuration differences between nodes. Conversely, a network issue is
unlikely to affect a backend.

Error | Message | Description | Resolution
:-----|:--------|:------------|:----------
`data_root_not_set` | | Same as `data_root_unset` | Set the `data_root` directory in config
`data_root_unset` | `Failed to create bitcask dir: data_root is not set` | The `data_root` config setting is required | Set `data_root` as the base directory where to store bitcask data, under the `bitcask` section
`{invalid_config_setting, multi_backend, list_expected}` | | Multi backend configuration requires a list | Wrap `multi_backend` config value in a list
`{invalid_config_setting, multi_backend, list_is_empty`} | | Multi backend configuration requires a value | Configure at least one backend under `multi_backend` in `app.config`
`{invalid_config_setting, multi_backend_default, backend_not_found}` | | | Must choose a valid backend type to configure
`multi_backend_config_unset` | | No configuration for Multi backend | Configure at least one backend under `multi_backend` in `app.config`
`not_loaded` | | Native driver not loading | Ensure your native drivers exist (.dll or .so files {riak_kv_multi_backend, undefined_backend, BackendName} | | Backend defined for a bucket is invalid | Define a valid backed before using this bucket under lib/`project`/priv, where `project` is most likely eleveldb).
`reset_disabled` | | Attempted to reset a Memory backend in production | Don't use this in production

### JavaScript

These are some errors related to JavaScript pre-commit functions,
MapReduce functions, or simply the management of the pool of JavaScript
VMs. If you do not use JavaScript, these should not be encountered. If
they are, check your configuration for high `*js_vm*` values or as an
epiphenomenon to a real issue, such as low resources.

Error    | Message | Description | Resolution
---------|---------|-------------|-------
`no_vms` | `JS call failed: All VMs are busy.` | All JavaScript VMs are in use | Wait and run again; increase JavaScript VMs in `app.config` (`map_js_vm_count`, `reduce_js_vm_count`, or `hook_js_vm_count`)
`bad_utf8_character_code` | `Error JSON encoding arguments: <Args>` | A UTF-8 character give was a bad format | Only use correct UTF-8 characters for JavaScript code and arguments
`bad_json` | | Bad JSON formatting | Only use correctly formatted JSON for JavaScript command arguments
 | `Invalid bucket properties: <Details>` | Listing bucket properties will fail if invalid | Fix bucket properties
`{load_error, "Failed to load spidermonkey_drv.so"}` | | The JavaScript driver is corrupted or missing | In OS X you may have compiled with `llvm-gcc` rather than `gcc`.

### MapReduce

These are possible errors logged by Riak's MapReduce implementation,
both legacy as well as Pipe. If you never use or call MapReduce, you
should not run across these.

Error | Message | Description | Resolution
:-----|:--------|:------------|:----------
`bad_mapper_props_no_keys` | | At least one property should be found by default. *Unused in Riak 1.3+* | Set mapper properties, or don't use it
`bad_mapred_inputs` | | A bad value sent to MapReduce. *Unused in Riak 1.3+* | When using the Erlang client interface, ensure all MapReduce and search queries are correctly binary
`bad_fetch` | | An expected local query was not retrievable.  *Unused in Riak 1.3+* | Placing javascript MapReduce query code as a riak value must first be stored before execution
`{bad_filter, <Filter>}` | | An invalid keyfilter was used | Ensure your MapReduce keyfilter is correct
`{dead_mapper, <Stacktrace>, <MapperData>}` | | Getting a reply from a mapper for a job that has already exited.  *Unused in Riak 1.3+* | Check for a stuck Erlang process, or if using legacy MR ensure `map_cache_size` is set (Both issues may require a node restart)
`{inputs, Reason}` | `An error occurred parsing the "inputs" field.` | MapReduce request has invalid input field | Fix MapReduce fields
`{invalid_json, Message}` | `The POST body was not valid JSON. The error from the parser was: <Message>` | Posting a MapReduce command requires correct JSON | Format MapReduce requests correctly
`javascript_reduce_timeout` | | JavaScript reduce function taking too long | For large numbers of objects, your JavaScript functions may become bottlenecks. Decrease the quantity of values being passed to and returned from the reduce functions, or rewrite as Erlang functions
`missing_field` | `The post body was missing the "inputs" or "query" field.` | Either an inputs or query field is required | Post MapReduce request with at least one
`{error,notfound}` | | Used in place of a RiakObject in the mapping phase | Your custom Erlang map function should deal with this type of value
`not_json` | `The POST body was not a JSON object.` | Posting a MapReduce command requires correct JSON | Format MapReduce requests correctly
`{no_candidate_nodes, exhausted_prefist, <Stacktrace>, <MapperData>}` | | Some map phase workers died | Possibly a long running job hitting MapReduce timeout, upgrade to Pipe
`{<query>, Reason}` | `An error occurred parsing the "query" field.` | MapReduce request has invalid query field | Fix MapReduce query
`{unhandled_entry, Other}` | `Unhandled entry: <Other>` | The `reduce_identity` function is unused | If you don't need `reduce_identity`, just don't set reduce phase at all
`{unknown_content_type, ContentType}` | | Bad content type for MapReduce query | Only `application/json` and `application/x-erlang-binary` are accepted
 | `Phase <Fitting>: <Reason>` | A general error when something happens using the Pipe MapReduce implementation with a bad argument or configuration | Can happen with a bad map or reduce implementation, most recent known gotcha is when a JavaScript function improperly deals with tombstoned objects
 | `riak_kv_w_reduce requires a function as argument, not a <Type>` | Reduce requires a function object, not any other type | This shouldn't happen
 
## Specific messages

Although you can put together many error causes with the tables above,
here are some common yet esoteric messages with known causes and
solutions.

 Message | Resolution
:--------|:----------
gen_server riak_core_capability terminated with reason: no function clause matching orddict:fetch('`Node`', []) | The Node has been changed, either through change of IP or `vm.args` `-name` without notifying the ring. Either use the `riak-admin cluster replace` command, or remove the corrupted ring files `rm -rf /var/lib/riak/ring/*` and rejoin to the cluster
gen_server <`PID`> terminated with reason: no function clause matching riak_core_pb:encode(`Args`) line 40 | Ensure you do not have different settings on different nodes (for example, a ttl mem setting on one node's mem backend, and another without)
monitor `busy_dist_port` `Pid` [...{almost_current_function,...] | This message means distributed Erlang buffers are filling up. Try setting zdbbl higher in `vm.args`, such as `+zdbbl 16384`. Or check that your network is not slow. Or ensure you are not slinging large values. If a high bandwidth network is congested, try setting RTO_min down to 0 msec (or 1msec).
<`PID`>@riak_core_sysmon___handler:handle_event:89 Monitor got {suppressed,port_events,1} | Logged as info, you can add `+swt very_low` to your `vm.args`
(in LevelDB LOG files) Compaction error | Turn off the node and run repair on the LevelDB partition. See <a href="{{< baseurl >}}riak/kv/2.2.0/using/repair-recovery/errors/#more">Step 2</a>.
enif_send: env==NULL on non-SMP VM/usr/lib/riak/lib/os_mon-2.2.9/priv/bin/memsup: Erlang has closed. | Riak's Erlang VM is built with SMP support and if Riak is started on a non-SMP system, an error like this one is logged. This is commonly seen in virtualized environments configured for only one CPU core.
exit with reason bad return value: {error,eaddrinuse} in context start_error | An error like this example can occur when another process is already bound to the same address as the process being started is attempting to bind to. Use operating system tools like `netstat`, `ps`, and `lsof` to determine the root cause for resolving this kind of errors; check for existence of stale  `beam.smp` processes.
exited with reason: eaddrnotavail in gen_server:init_it/6 line 320 | An error like this example can result when Riak cannot bind to the addresses specified in the configuration. In this case, you should verify HTTP and Protocol Buffers addresses in `app.config` and ensure that the ports being used are not in the privileged (1-1024) range as the `riak` user will not have access to such ports.
gen_server riak_core_capability terminated with reason: no function clause matching orddict:fetch('riak@192.168.2.2', []) line 72 | Error output like this example can indicate that a previously running Riak node with an original `-name` value in `vm.args` has been modified by simply changing the value in `vm.args` and not properly through `riak-admin cluster replace`.
** Configuration error: [FRAMEWORK-MIB]: missing context.conf file => generating a default file | This error is commonly encountered when starting Riak Enterprise without prior [SNMP]({{<baseurl>}}riak/kv/2.2.0/using/reference/snmp) configuration.
RPC to 'node@example.com' failed: {'EXIT', {badarg, [{ets,lookup, [schema_table,<<"search-example">>], []} {riak_search_config,get_schema,1, [{file,"src/riak_search_config.erl"}, {line,69}]} ...| This error can be caused when attempting to use Riak Search without first enabling it in each node's `app.config`. See the [configuration files][config reference] documentation for more information on enabling Riak Search.


### More

1. <a name="f1"></a>Ensure node inter-communication
  - Check `riak-admin member-status` and ensure the cluster is valid.
  - Check `riak-admin ring-status` and ensure the ring and vnodes are communicating as expected.
  - Ensure your machine does not have a firewall or other issue that prevents traffic to the remote node.
  - Your remote `vm.args` `-setcookie` must be the same value for every node in the cluster.
  - The `vm.args` `-name` value must not change after joining the node (unless you use `riak-admin cluster replace`).

2. <a name="f2"></a>Run LevelDB compaction
  1. `find . -name "LOG" -exec grep -l 'Compaction error' {} \;` *(Finding one compaction error is interesting, more than one might be a strong indication of a hardware or OS bug)*
  2. Stop Riak on the node: `riak stop`
  3. Start an Erlang session (do not start riak, we just want Erlang)
  4. From the Erlang console perform the following command to open the LevelDB database

        ```erlang
        [application:set_env(eleveldb, Var, Val) || {Var, Val} <-
        [{max_open_files, 2000},
        {block_size, 1048576},
        {cache_size, 20*1024*1024*1024},
        {sync, false},
        {data_root, "/var/db/riak/leveldb"}]].
        ```
  5. For each of the corrupted LevelDB databases (found by `find . -name "LOG" -exec` | `grep -l 'Compaction error' {} \; `) run this command substituting in the proper vnode number.

        ```erlang
        eleveldb:repair("/var/db/riak/leveldb/442446784738847563128068650529343492278651453440", []).
        ```
  6. When all have finished successfully you may restart the node: `riak start`
  7. Check for proper operation by looking at log files in /var/log/riak and in the LOG files in the effected LevelDB vnodes.
