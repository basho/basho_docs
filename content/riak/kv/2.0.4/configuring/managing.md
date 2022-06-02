---
title: "Managing Your Configuration"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Managing Configuration"
    identifier: "configuring_managing"
    weight: 103
    parent: "configuring"
toc: true
---

[use admin riak cli]: {{<baseurl>}}riak/kv/2.0.4/using/admin/riak-cli
[use admin riak cli#chkconfig]: {{<baseurl>}}riak/kv/2.0.4/using/admin/riak-cli/#chkconfig
[config reference#search]: {{<baseurl>}}riak/kv/2.0.4/configuring/reference/#search

## Retrieving a Configuration Listing

At any time, you can get a snapshot of currently applied configurations
through the command line. For a listing of *all* of the configs
currently applied in the node:

```bash
riak config effective
```

This will output a long list of the following form:

```
anti_entropy = active
anti_entropy.bloomfilter = on
anti_entropy.concurrency_limit = 2
# and so on
```

For detailed information about a particular configuration variable, use
the `config describe <variable>` command. This command will output a
description of what the parameter configures, which datatype you should
use to set the parameter (integer, string, enum, etc.), the default
value of the parameter, the currently set value in the node, and the
name of the parameter in `app.config` in older versions of Riak (if
applicable).

For in-depth information about the `ring_size` variable, for example:

```bash
riak config describe ring_size
```

This will output the following:

```
Documentation for ring_size
Number of partitions in the cluster (only valid when first
creating the cluster). Must be a power of 2, minimum 8 and maximum
1024.

   Datatype     : [integer]
   Default Value: 64
   Set Value    : undefined
   app.config   : riak_core.ring_creation_size
```

## Checking Your Configuration

The [`riak`][use admin riak cli] command line tool has a
[`chkconfig`][use admin riak cli#chkconfig] command that enables you to
determine whether the syntax in your configuration files is correct.

```bash
riak chkconfig
```

If your configuration files are syntactically sound, you should see the
output `config is OK` followed by a listing of files that were checked.
You can safely ignore this listing. If, however, something is
syntactically awry, you'll see an error output that provides details
about what is wrong. To give an example, the `search.solr.jmx_port`
setting (in the [Search][config reference#search] section below)
must be set as an integer. Imagine that we set it to something else:

```riakconf
search.solr.jmx_port = banana
```

If we run `riak chkconfig` now, we'll get an error:

```
[error] Error generating configuration in phase transform_datatypes
[error] Error transforming datatype for: search.solr.jmx_port
[error] "banana" can't be converted to an integer
```

The error message will specify which configurable parameters are
syntactically unsound and attempt to provide an explanation why.

Please note that the `chkconfig` command only checks for syntax. It will
_not_ be able to discern if your configuration is otherwise unsound,
e.g. if your configuration will cause problems on your operating system
or doesn't activate subsystems that you would like to use.

## Debugging Your Configuration

If there is a problem with your configuration but you're having trouble
identifying the problem, there is a command that you can use to debug
your configuration:

```bash
riak config generate -l debug
```

If there are issues with your configuration, you will see detailed
output that might provide a better sense of what has gone wrong in the
config generation process.
