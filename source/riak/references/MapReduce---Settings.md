---
title: Riak MapReduce Settings
project: riak
version: 1.2.0+
document: appendix
toc: true
audience: advanced
keywords: [mapreduce]
---

## Configuring MapReduce

[[MapReduce]] \(M/R) is always enabled, but configurable through the [[app.config|Configuration-Files#app-config]] file as follows under `riak_kv`

```erlang
{riak_kv, [
```

`mapred_name` is the URL directory used to submit M/R requests to Riak. By default `mapred`, making the command path, for example: `http://localhost:8091/mapred`

```erlang
    {mapred_name, "mapred"},
```

{{#<1.3.0}}
`mapred_system` indicates which version of the MapReduce system should be used:

* `pipe` means [riak_pipe](https://github.com/basho/riak_pipe) will power M/R queries
* `legacy` means that [luke](https://github.com/basho/luke) will be used

```erlang
    {mapred_system, pipe},
```
{{/<1.3.0}}

`mapred_2i_pipe` indicates whether [[2i|Secondary Indexes]] MapReduce inputs
are queued in parallel in their own pipe (`true`), or serially through a helper
process (`false` or undefined).

{{#1.1.0+}}
_**Note**: Set to `false` or leave undefined during a rolling upgrade from 1.0._
{{/1.1.0+}}

```erlang
    {mapred_2i_pipe, true},
```

{{#<1.3.0}}
`mapred_queue_dir` directory used to store a transient queue for pending map
tasks.

_Only valid for `{mapred_system, legacy}`, used by [luke](https://github.com/basho/luke)._

```erlang
    %% {mapred_queue_dir, "./data/mr_queue" },
```
{{/<1.3.0}}

Each of these entries control how many Javascript virtual machines
are available for executing map, reduce, pre- and post-commit hook
functions.

This is largely relevant only if you are writing JavaScript M/R jobs.

```erlang
    {map_js_vm_count, 8 },
    {reduce_js_vm_count, 6 },
    {hook_js_vm_count, 2 },
```

{{#<1.3.0}}
`mapper_batch_size` is the number of items the mapper will fetch in one
request. Larger values can impact read/write performance for non-MapReduce
requests.

_Only valid for `{mapred_system, legacy}`, used by [luke](https://github.com/basho/luke)._

```erlang
    %% {mapper_batch_size, 5},
```
{{/<1.3.0}}

`js_max_vm_mem` is the maximum amount of memory, in megabytes, allocated to
the Javascript VMs. If unset, the default is 8MB.

This is largely relevant only if you are writing JavaScript M/R jobs.

```erlang
    {js_max_vm_mem, 8},
```

`js_thread_stack` is the maximum amount of thread stack, in megabyes,
allocated to the Javascript VMs. If unset, the default is 16MB.

_**Note**: This is not the same as the C thread stack._

```erlang
    {js_thread_stack, 16},
```

{{#<1.3.0}}
`map_cache_size` is the number of objects held in the MapReduce cache.
These will be ejected when the cache runs out of room or the bucket/key
pair for that entry changes.

_Only valid for `{mapred_system, legacy}`, used by [luke](https://github.com/basho/luke)._

```erlang
    %% {map_cache_size, 10000},
```
{{/<1.3.0}}

`js_source_dir` should point to a directory containing Javascript source
files which will be loaded when Riak initializes Javascript VMs.

```erlang
    %{js_source_dir, "/tmp/js_source"},
```
