---
title: Erlang Named Functions
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: intermediate
keywords: [functions, beam, commit hook, mapreduce]
---

Riak supports the use of compiled Erlang named functions as pre-commit hooks,
post-commit hooks, and MapReduce operations. This guide explains the process
for using your own named functions, including compilation, configuration, and
installation of some basic examples.

## Pre-Commit Hook Example

In this example, we'll define a function to enforce limitations on
on object value size to 5MB or less. If you've read the [[Commit Hooks]]
documentation, then this example will be familiar to you.

Here is our example function.

```erlang
-module(limit_size).

-export([precommit/1]).


%% Limits object values to 5MB or smaller
precommit(Object) ->
  case erlang:byte_size(riak_object:get_value(Object)) of
    Size when Size > 5242880 -> {fail, "Object is larger than 5MB."};
    _ -> Object
  end.
```

Save this file as `limit_size.erl` and proceed to compiling.

<div class="info"><div class="title">Note</div>You must use the Erlang
compiler (`erlc`) associated with the Riak installation or the version
of Erlang used when compiling Riak from source. For packaged Riak
installations, you can consult the table below for the default location
of Riak's `erlc` for each supported platform. If you compiled from
source, use the `erlc` from the Erlang version you used to compile Riak.</div>

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

Table 1: Erlang compiler executable location for supported platforms

Compiling the function is straightforward:

```bash
erlc limit_size.erl
```

Next, you'll need to define a path from which to store and load compiled
functions. For our example, we'll use a temporary directory (`/tmp/funs`),
but you should choose a different directory for production functions
such that they will be available where needed.

Successful compilation will result in a new `.beam` file:
`limit_size.beam`. Copy this file to the `/tmp/funs` directory.

```bash
cp limit_size.beam /tmp/funs/
```

After copying the compiled function into place, you'll need to update
`app.config` to instruct Riak to allow loading of named functions from the
directory where they're stored, again in this case, `/tmp/funs`.

Edit `app.config` and insert an `add_paths` setting into the `riak_kv`
section as shown below.

```erlang
{riak_kv, [
  %% ...

  {add_paths, ["/tmp/funs/"]},

  %%...
```

After updating `app.config`, Riak must be restarted. In production cases, you
should ensure that if you are adding configuration changes to multiple nodes,
that you do so in a rolling fashion, taking time to allow the Riak key value
store sufficient time to initialize and become available for use.

This is done with the `riak-admin wait-for-service` command as detailed
in the [Commands documentation](http://docs.basho.com/riak/latest/references/Command-Line-Tools---riak-admin/#wait-for-service).

<div class="note"><strong>It is important that you ensure riak_kv is
active before moving a second node</strong>.</div>

Once Riak is restarted, all that remains is to install the pre-commit
hook on the target bucket(s) you wish it to operate on. In this example,
we've just one bucket, named `avatars`, which we're going to install our
`limit_size` function into.

You can use Riak's HTTP interface and the curl(1) command line utility to
install your named functions into into the relevant buckets. For our example,
we'll install the `limit_size` function into our `avatars` bucket, like this.

```bash
curl -XPUT -H "Content-Type: application/json" \
http://127.0.0.1:8098/buckets/avatars/props    \
-d '{"props":{"precommit":[{"mod": "limit_size", "fun": "precommit"}]}}'
```

## Post-Commit Hook Example

In this example, we'll define a post-commit hook function to log the object
values after they are successfully written to Riak.

Here is our example function.

```erlang
-module(post_commit).

-export([log/1]).

log(Object) ->
  error_logger:info_msg("OBJECT: ~p~n",[Object]).
```

Save this file as `post_commit.erl` and proceed to compiling.

<div class="info"><div class="title">Note</div>You must use the Erlang
compiler (`erlc`) associated with the Riak installation or the version
of Erlang used when compiling Riak from source. For packaged Riak
installations, you can consult Table 1 above for the default location
of Riak's `erlc` for each supported platform. If you compiled from
source, use the `erlc` from the Erlang version you used to compile Riak.</div>

Compiling the function is straightforward:

```bash
erlc post_commit.erl
```

Next, you'll need to define a path from which to store and load compiled
functions. For our example, we'll use a temporary directory (`/tmp/funs`),
but you should choose a different directory for production functions
such that they will be available where needed.

Successful compilation will result in a new `.beam` file:
`limit_size.beam`. Copy this file to the `/tmp/funs` directory.

```bash
cp post_commit.erl /tmp/funs/
```

After copying the compiled function into place, you'll need to update
`app.config` to instruct Riak to allow loading of named functions from the
directory where they're stored, again in this case, `/tmp/funs`.

Edit `app.config` and insert an `add_paths` setting into the `riak_kv`
section as shown below.

```erlang
{riak_kv, [
  %% ...

  {add_paths, ["/tmp/funs/"]},

  %%...
```

After updating `app.config`, Riak must be restarted. In production cases, you
should ensure that if you are adding configuration changes to multiple nodes,
that you do so in a rolling fashion, taking time to allow the Riak key value
store sufficient time to initialize and become available for use.

This is done with the `riak-admin wait-for-service` command as detailed
in the [Commands documentation](http://docs.basho.com/riak/latest/references/Command-Line-Tools---riak-admin/#wait-for-service).

<div class="note"><strong>It is important that you ensure riak_kv is
active before moving a second node</strong>.</div>

Once Riak is restarted, all that remains is to install the post-commit
hook on the target bucket(s) you wish it to operate on. In this example,
we've just one bucket, named `updates`, which we're going to install our
`post_commit` function into.

You can use Riak's HTTP interface and the curl(1) command line utility to
install your named functions into into the relevant buckets. For our example,
we'll install the `limit_size` function into our `avatars` bucket, like this.

```bash
curl -XPUT -H "Content-Type: application/json" \
http://127.0.0.1:8098/buckets/updates/props    \
-d '{"props":{"postcommit":[{"mod": "post_commit", "fun": "log"}]}}'
```

Check that the bucket has your post-commit hook listed in its properties.

```bash
curl localhost:8098/buckets/updates/props | python -mjson.tool

{
    "props": {
        "allow_mult": false,
        "basic_quorum": false,
        "big_vclock": 50,
        "chash_keyfun": {
            "fun": "chash_std_keyfun",
            "mod": "riak_core_util"
        },
        "dw": "quorum",
        "last_write_wins": false,
        "linkfun": {
            "fun": "mapreduce_linkfun",
            "mod": "riak_kv_wm_link_walker"
        },
        "n_val": 3,
        "name": "updates",
        "notfound_ok": true,
        "old_vclock": 86400,
        "postcommit": [
            {
                "fun": "log",
                "mod": "post_commit"
            }
        ],
        "pr": 0,
        "precommit": [],
        "pw": 0,
        "r": "quorum",
        "rw": "quorum",
        "small_vclock": 50,
        "w": "quorum",
        "young_vclock": 20
    }
}
```

You can see that postcommit is indeed set to our module post_commit and log
function. Now you can test the post-commit function by posting an object and
consulting the `console.log` file.

```bash
curl -XPUT localhost:8098/buckets/updates/keys/2 \
-H 'Content-Type: application/json' -d@message.json
```

You can now see the logged value of the object by examining `console.log`.

```text
2012-12-10 13:14:37.840 [info] <0.2101.0> OBJECT: {r_object,<<"updates">>,<<"2">>,[{r_content,{dict,6,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[[<<"Links">>]],[],[],[],[],[],[],[],[[<<"content-type">>,97,112,112,108,105,99,97,116,105,111,110,47,106,115,111,110],[<<"X-Riak-VTag">>,52,114,79,84,75,73,90,73,83,105,49,101,120,53,87,103,106,110,56,71,114,83]],[[<<"index">>]],[],[[<<"X-Riak-Last-Modified">>|{1355,163277,837883}]],[],[[<<"X-Riak-Meta">>]]}}},<<"{    \"id\": 1,    \"jsonrpc\": \"2.0\",    \"total\": 1,    \"result\": [        {            \"id\": 1,            \"author\": \"foo@example.com\",            \"authorName\": \"Foo Bar\",            \"text\": \"Home of the example cocktail\"        }    ]}">>}],[{<<35,9,254,249,80,193,17,247>>,{1,63522382477}}],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[[clean|true]],[]}}},undefined}
```

## MapReduce Example

## References
