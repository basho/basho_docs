---
title: "Installing Custom Code"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Installing Custom Code"
    identifier: "managing_ref_custom_code"
    weight: 111
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.0.4/ops/advanced/install-custom-code/
  - /riak/kv/2.0.4/ops/advanced/install-custom-code/
---

Riak supports the use of Erlang named functions in compiled modules for
[pre/post-commit hooks]({{<baseurl>}}riak/kv/2.0.4/developing/usage/commit-hooks), and MapReduce operations. This
doc contains installation steps with simple examples for each use case.

Your developers can compile [custom erlang code]({{<baseurl>}}riak/kv/2.0.4/developing/usage/commit-hooks), which
they can send to you as a *beam* file. You should note that in Erlang, a file
name must have the same name the module. So if you are given a file named
`validate_json.beam`, do not rename it.

> *Note: The [Configure](#configure) step (`add_paths`) also applies to installing JavaScript files.*

### Compiling

If you have been given Erlang code and are expected to compile it for
your developers, keep the following notes in mind.

{{% note title="Note on the Erlang Compiler" %}}
You must use the Erlang compiler (`erlc`) associated with the Riak
installation or the version of Erlang used when compiling Riak from source.
For packaged Riak installations, you can consult Table 1 below for the default
location of Riak's `erlc` for each supported platform. If you compiled from
source, use the `erlc` from the Erlang version you used to compile Riak.
{{% /note %}}

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

Table 1: Erlang compiler executable location for packaged Riak installations
         on supported platforms

Compiling the module is a straightforward process.

```text
erlc validate_json.erl
```

Next, you'll need to define a path from which compiled modules can be stored
and loaded. For our example, we'll use a temporary directory `/tmp/beams`,
but you should choose a directory for production functions based on your
own requirements such that they will be available where and when needed.

{{% note %}}
Ensure that the directory chosen above can be read by the `riak` user.
{{% /note %}}

Successful compilation will result in a new `.beam` file,
`validate_json.beam`.

### Configure

Take the `validate_json.beam` and copy this file to the `/tmp/beams` directory.

```text
cp validate_json.beam /tmp/beams/
```

After copying the compiled module into `/tmp/beams/`, you must update
`app.config` and configure Riak to allow loading of compiled modules from
the directory where they're stored (again in our example case, `/tmp/beams`).

Edit `app.config` and insert an `add_paths` setting into the `riak_kv`
section as shown:

```erlang
{riak_kv, [
  %% ...
  {add_paths, ["/tmp/beams/"]},
  %% ...
```

After updating `app.config`, Riak must be restarted. In production cases, you
should ensure that if you are adding configuration changes to multiple nodes,
that you do so in a rolling fashion, taking time to ensure that the Riak key
value store has fully initialized and become available for use.

This is done with the `riak-admin wait-for-service` command as detailed
in the [Commands documentation]({{<baseurl>}}riak/kv/2.0.4/using/admin/riak-admin/#wait-for-service).

{{% note %}}
It is important that you ensure riak_kv is active before restarting the next
node.
{{% /note %}}
