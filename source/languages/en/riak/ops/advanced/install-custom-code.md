---
title: Installing Custom Code
project: riak
version: 1.3.1+
document: tutorial
toc: true
audience: advanced
keywords: [operators, code, erlang, javascript]
moved: {
    '1.4.0-': '/cookbooks/Erlang-Named-Functions'
}
---

Riak supports the use of Erlang named functions in compiled modules for
[[pre- and post-commit hooks|Advanced Commit Hooks]] and 
[[MapReduce|Using MapReduce]] operations. This document contains 
installation steps with simple examples for each use case.

Your developers can compile [[custom Erlang code|Advanced Commit Hooks]]
which they can send to you as a `.beam` file. You should note that in 
Erlang, a filename must have the same name as the module. So if you are 
given a file named `validate_json.beam`, do not rename it.

## Compiling

If you have been given Erlang code and are expected to compile it for
your developers, keep the following notes in mind.

<div class="info">
<div class="title">Note on the Erlang Compiler</div>
You must use the Erlang compiler (<code>erlc</code>) associated with the
Riak installation or the version of Erlang used when compiling Riak from
source. For packaged Riak installations, you can consult Table 1 below
for the default location of Riak's <code>erlc</code> for each supported 
platform. If you compiled from source, use the <code>erlc</code> from
the Erlang version you used to compile Riak.
</div>

**Table 1**: Erlang compiler executable location for packaged Riak
installations on supported platforms

OS | Location
:--|:--------
**CentOS & Red Hat** | `/usr/lib64/riak/erts-5.9.1/bin/erlc`
**Debian & Ubuntu** | `/usr/lib64/riak/erts-5.9.1/bin/erlc`
**FreeBSD** | `/usr/local/lib/riak/erts-5.9.1/bin/erlc`
**SmartOS** | `/opt/local/lib/riak/erts-5.9.1/bin/erlc`
**Solaris 10** | `/opt/riak/lib/erts-5.9.1/bin/erlc`

Compiling the module is a straightforward process.

```bash
erlc validate_json.erl
```

Next, you'll need to define a path from which compiled modules can be 
stored and loaded. For our example, we'll use a temporary directory 
`/tmp/beams`, but you should choose a directory for production functions 
based on your own requirements such that they will be available where 
and when needed.

<div class="info">
<p>Ensure that the directory chosen above can be read by the
<code>riak</code> user.</p>
</div>

Successful compilation will result in a new `.beam` file,
`validate_json.beam`.

## Configuration

Take the `validate_json.beam` and copy this file to the `/tmp/beams` 
directory.

```bash
cp validate_json.beam /tmp/beams/
```

After copying the compiled module into `/tmp/beams/`, you must update 
your configuration file to enable Riak to load compiled modules from the 
directory where they're stored (in our example `/tmp/beams`).

If you are using the older, `app.config`-based configuration system, you
can add one or more paths using the `add_paths` parameter in the
`riak_kv` section, as below:

```appconfig
{riak_kv, [
  %% ...
  {add_paths, ["/tmp/beams/"]},
  %% ...
```

If you are using the newer configuration system (with most configuration
handled by the `riak.conf` file), you can add custom code paths in the 
same way as shown above but in an `advanced.config` file. More
information can be found in our documentation on
[[advanced configuration|Configuration Files#Advanced-Configuration]].

After updating the appropriate configuration file, your Riak node must
be [[restarted|riak-admin Command Line]].
