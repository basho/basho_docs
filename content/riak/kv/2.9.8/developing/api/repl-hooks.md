---
title_supertext: "Riak Multi-Datacenter Replication:"
title: "Hooks API"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Multi-Datacenter REPL Hooks API"
    identifier: "apis_repl_hooks"
    weight: 100
    parent: "developing_apis"
toc: true
commercial_offering: true
aliases:
  - /riak/2.9.8/ops/mdc/v2/hooks
  - /riak/kv/2.9.8/ops/mdc/v2/hooks
---
[object]: https://github.com/basho/riak_kv/blob/master/src/riak_object.erl 

This document is a guide to developing extensions for Riak's
Multi-Datacenter Replication feature.

## Replication Hooks

Riak allows applications to register replication hooks to control
either of the following:

* when extra objects need to be replicated along with the current object
* when an object should _not_ be replicated.

To register a hook, you must call the following function in an
application-specific Erlang module, where `MyMod` is to be replaced
with the name of your custom module:

```erlang
riak_core:register([{repl_helper, MyMod}]).
```

## Replication Hook API

A replication hook must implement the following functions:

### send_realtime/2

```erlang
(riak_object, RiakClient) -> ok | cancel | [riak_object]
```

This hook controls whether an [object][object]
replicated in realtime should be sent. To send this object, return `ok`;
to prevent the object from being sent, return `cancel`. You can also
return a list of Riak objects to be replicated immediately *before* the
current object. This is useful when you have an object that refers to
other objects, e.g. a chunked file, and want to ensure that all of the
dependency objects are replicated before the dependent object.
   
### send/2

```erlang
(riak_object, RiakClient) -> ok | cancel | [riak_object]
```

This hook is used in fullsync replication. To send this
[object][object],
return `ok`; to prevent the object from being sent, return `cancel`. You
can also return a list of Riak objects to be replicated immediately
*before* the current object. This is useful for when you have an object
that refers to other objects, e.g. a chunked file, and want ensure that
all the  dependency objects are replicated before the dependent object.

### recv/1

```erlang
(riak_object) -> ok | cancel
```

When an [object][object]
is received by the client site, this hook is run. You can use it to
update metadata or to deny the object.

## Implementing a Sample Replication Hook

The following is a simple replication hook that will log when an object
is received via replication. For more information about the functions in
the sample, see the [Replication Hook API](#replication-hook-api) section below.

Here is the relevant Erlang code:

```erlang
%% Riak Enterprise MDC replication hook sample
 
-module(riak_replication_hook_sample).
-export([register/0]).
-export([recv/1, send/2, send_realtime/2]).
 
register() ->
  riak_core:wait_for_service(riak_repl),
  lager:log(info, self(),
              "Automatically registering ~p hook with riak_core",
              [?MODULE_STRING]),
  riak_core:register([{repl_helper, ?MODULE}]),
  case lists:member({undefined,?MODULE},
                      app_helper:get_env(riak_core,repl_helper, [])) of
    true ->
      lager:log(info, self(),
                  "Successfully registered ~p hook with riak_core",
                  [?MODULE_STRING]);
    false ->
      lager:log(info, self(),
                  "Failed to register ~p hook with riak_core",
                  [?MODULE_STRING])
  end,
  ok.
 
recv(Object) ->
  % This is a BLOCKING function.
  % Longer-running processes should be handled asynchronously.
  lager:log(info, self(), "Called recv(~p)", [riak_object:key(Object)]),
  ok.
  
send_realtime(_Object, _RiakClient) ->
  % Do Nothing function -- These hooks are called in predictable
  % but complex ways especially as the number of replication 
  % sites (Version 2 Replication) or sinks (Version 3 Replication)
  % increase.  
  ok.
 
send(_Object, _RiakClient) ->
  % Do Nothing function -- These hooks are called in predictable
  % but complex ways especially as the number of replication 
  % sites (Version 2 Replication) or sinks (Version 3 Replication)
  % increase.  
  ok.
```

Save the above code as `riak_replication_hook_sample.erl`.

To install the sample hook, compile `riak_replication_hook_sample.erl`.

{{% note title="Note on the Erlang compiler" %}}

[erlc]: http://erlang.org/doc/man/erlc.html
You must use the Erlang compiler [`erlc`][erlc]
associated with the Riak installation or the version of Erlang used when
compiling Riak from source. For packaged Riak installations, you can
consult **Table 1** (below) for the default location of
Riak’s `erlc` for each supported platform. If you compiled
from source, use the `erlc` from the Erlang version you used
to compile Riak.
{{% /note %}}

Distribution | Path
:------------|:----
CentOS & RHEL Linux | `/usr/lib64/riak/erts-5.10.3/bin/erlc` |
Debian & Ubuntu Linux | `/usr/lib/riak/erts-5.10.3/bin/erlc` |
FreeBSD | `/usr/local/lib/riak/erts-5.10.3/bin/erlc` |
SmartOS | `/opt/local/lib/riak/erts-5.10.3/bin/erlc`
Solaris 10 | `/opt/riak/lib/erts-5.10.3/bin/erlc`

**Table 1**: Erlang compiler executable location for packaged Riak
installations on supported platforms

Once you have determined the location of the Erlang compiler, e.g. on
Ubuntu, compiling is as simple as:

```bash
/usr/lib/riak/erts-5.10.3/bin/erlc riak_replication_hook_sample.erl
```

This will create a `riak_replication_hook_sample.beam` file in the same
directory as the corresponding `.erl` file. Copy this `.beam` file into
the subdirectory where you want to store the custom hook:
   
```bash
cp riak_replication_hook_sample.beam /path/to/replication/hook
```
   
Add a `-pa` argument to your `vm.args` file to specify the path where
your compiled `.beam` file lives:

```bash
-pa /path/to/replication/hook
```
   
Finally, add a `-run` argument to your `vm.args` file to register the
hook:

```bash
-run riak_replication_hook_sample register
```





