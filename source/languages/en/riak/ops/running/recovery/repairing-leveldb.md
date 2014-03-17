---
title: Repairing LevelDB
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [leveldb, troubleshooting, backend]
---

In the event of major hardware or filesystem problems, LevelDB can become corrupted. These failures are not uncommon, as Riak is a heavily I/O-intensive system

## Checking for Compaction Errors

Any time there is a compaction error, it will be noted in the LevelDB logs. Those logs are located in a `LOG` file in each instance of LevelDB in a Riak node, specifically in `#(platform_data_dir)/leveldb/<vnode>/LOG`. The `platform_data_dir` can be specified in the `[[riak.conf|Configuration Files]]` configuration file. The default is `./data`.

Compaction error messages take the following form:

```
<timestamp> Compaction Error: Corruption: corrupted compressed block contents
```

To check whether your node has experienced such errors, you will need to run a script that searches for `Compaction Error` in each `LOG` file. Here is an example script:

```bash
find . -name "LOG" -exec grep -l 'Compaction error' {} \;
```

If there are compaction errors in any of your vnodes, those will be listed in the console. If a vnodes has experienced such errors, you would see output like this:

```
./442446784738847563128068650529343492278651453440/LOG 
```

<div class="note">
<div class="title">Note</div>
While corruption on one vnode is not uncommon, corruption in several vnodes very likely means that there is a deeper problem that needs to be address, perhaps on the OS or hardware level.
</div>

## Healing Corrupted LevelDBs

The first step in properly addressing this problem is to stop the node.

```bash
riak stop
```

Repairing the corrupted LevelDB can be done through the [Erlang shell](http://learnyousomeerlang.com/starting-out). In the shell, run the following command:

```erlang
[application:set_env(eleveldb, Var, Val) || {Var, Val} <- 
    [{max_open_files, 2000}, 
     {block_size, 1048576}, 
     {cache_size, 20*1024*1024*1024}, 
     {sync, false}, 
     {data_root, ""}]].
```

For each of the corrupted LevelDBs that you found using the `find` command (as demonstrated above), run the following `repair` command, substituting the the path to your LevelDB vnodes and the appropriate vnode number:

```erlang
eleveldb:repair("/path-to-vnode/<vnode_number>", []).
```

This process will likely take several minutes. When it has completed successfully, you can restart the node and continue as usual.

```bash
riak start
```

