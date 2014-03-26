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
[[pre- and post-commit hooks|Advanced Commit Hooks]] and [[MapReduce]] operations. This document contains installation steps with simple examples for each use case.

Your developers can compile [[custom erlang code|Advanced Commit Hooks]] which
they can send to you as a `.beam` file. You should note that in Erlang, a file
name must have the same name as the module. So if you are given a file named
`validate_json.beam`, do not rename it.

**Note**: The [[configure|Installing Custom Code#Configure]] step (`add_paths`) also applies to installing JavaScript files.

### Compiling

If you have been given Erlang code and are expected to compile it for
your developers, keep the following notes in mind.

<div class="info"><div class="title">Note on the Erlang Compiler</div> You
must use the Erlang compiler (<tt>erlc</tt>) associated with the Riak
installation or the version of Erlang used when compiling Riak from source.
For packaged Riak installations, you can consult Table 1 below for the
default location of Riak's <tt>erlc</tt> for each supported platform.
If you compiled from source, use the <tt>erlc</tt> from the Erlang version
you used to compile Riak.</div>

**Table 1**: Erlang compiler executable location for packaged Riak
installations on supported platforms

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>CentOS &amp; RHEL Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/lib64/riak/erts-5.9.1/bin/erlc</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Debian &amp; Ubuntu Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/lib/riak/erts-5.9.1/bin/erlc</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>FreeBSD</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/local/lib/riak/erts-5.9.1/bin/erlc</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>SmartOS</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/opt/local/lib/riak/erts-5.9.1/bin/erlc</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Solaris 10</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/opt/riak/lib/erts-5.9.1/bin/erlc</tt></p>
</td>
</tr>
</tbody>
</table>

Compiling the module is a straightforward process.

```bash
erlc validate_json.erl
```

Next, you'll need to define a path from which compiled modules can be stored
and loaded. For our example, we'll use a temporary directory `/tmp/beams`,
but you should choose a directory for production functions based on your
own requirements such that they will be available where and when needed.

<div class="info">
<p>Ensure that the directory chosen above can be read by the <tt>riak</tt> user.</p>
</div>

Successful compilation will result in a new `.beam` file, `validate_json.beam`.

### Configure

Take the `validate_json.beam` and copy this file to the `/tmp/beams` directory.

```bash
cp validate_json.beam /tmp/beams/
```

After copying the compiled module into `/tmp/beams/`, you must update your configuration file to enable Riak to load compiled modules from the directory where they're stored (in our example `/tmp/beams`):

```riakconf

```

```appconfig
{riak_kv, [
  %% ...
  {add_paths, ["/tmp/beams/"]},
  %% ...
```

After updating your configuration file, Riak must be restarted. In production
cases, you should ensure that if you are adding configuration changes to 
multiple nodes, that you do so in a rolling fashion, taking time to ensure 
that Riak has fully initialized and become available for use.

This is done with the `riak-admin wait-for-service` command as detailed
in the [[Commands documentation|riak-admin Command Line#wait-for-service]].

<div class="note">It is important that you ensure that riak_kv is
active before restarting the next node.</div>
