---
title: Erlang Named Functions
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: intermediate
keywords: [beam, commit hook, Erlang, function, module, mapreduce]
---

Riak supports the use of Erlang named functions in compiled modules for
pre-commit hooks, post-commit hooks, and MapReduce operations. This cookbook
explains the process for using your own named functions, including
compilation, configuration, and installation of some basic examples.

## Pre-Commit Hook Example

For the pre-commit hook example, we'll define a function to enforce
constraints on on object value size to 5MB or less. If you've read the
[[Commit Hooks]] documentation, then this example will be familiar to you.

Here is our example pre-commit function:

```erlang
-module(pre_commit).

-export([limit_size/1]).


%% Limits object values to 5MB or smaller
limit_size(Object) ->
  case erlang:byte_size(riak_object:get_value(Object)) of
    Size when Size > 5242880 -> {fail, "Object is larger than 5MB."};
    _ -> Object
  end.
```

Save this file as `pre_commit.erl` and proceed to compiling a module.

<div class="info"><div class="title">Note on the Erlang Compiler</div> You
must use the Erlang compiler (<tt>erlc</tt>) associated with the Riak
installation or the version of Erlang used when compiling Riak from source.
For packaged Riak installations, you can consult Table 1 below for the
default location of Riak's <tt>erlc</tt> for each supported platform.
If you compiled from source, use the <tt>erlc</tt> from the Erlang version
you used to compile Riak.</div>

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

Table 1: Erlang compiler executable location for package installations

Compiling the module into is a straightforward process:

```bash
erlc limit_size.erl
```

Next, you'll need to define a path from which to store and load compiled
modules. For our example, we'll use a temporary directory (`/tmp/beams`),
but you should choose a different directory for production functions
such that they will be available where needed.

Successful compilation will result in a new `.beam` file:
`pre_commit.beam`. Copy this file to the `/tmp/beams` directory.

```bash
cp pre_commit.beam /tmp/beams/
```

After copying the compiled module into place, you'll need to update
`app.config` to configure Riak to allow loading of compiled modules from
the directory where they're stored, again in our example case, `/tmp/beams`.

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
in the [Commands documentation](http://docs.basho.com/riak/latest/references/Command-Line-Tools---riak-admin/#wait-for-service).

<div class="note"><strong>It is important that you ensure riak_kv is
active before moving a second node</strong>.</div>

Once Riak is restarted, all that remains is to install the pre-commit
hook into the target bucket(s) you wish it to operate on. In this example,
we've just one bucket, named `avatars`, which we're going to install our
`limit_size` pre-commit function into.

You can use Riak's HTTP interface and the `curl` command line utility to
install your named functions into into the relevant bucket(s). For our
example, we'll install the `pre_commit` module with its `limit_size`
function into our `avatars` bucket, like this:

```bash
curl -XPUT -H "Content-Type: application/json" \
http://127.0.0.1:8098/buckets/avatars/props    \
-d '{"props":{"precommit":[{"mod": "limit_size", "fun": "precommit"}]}}'
```

Check that the bucket has your post-commit hook listed in its properties.

```bash
curl localhost:8098/buckets/avatars/props | python -mjson.tool

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
        "name": "avatars",
        "notfound_ok": true,
        "old_vclock": 86400,
        "postcommit": [],
        "pr": 0,
        "precommit": [
            {
                "fun": "precommit",
                "mod": "limit_size"
            }
        ],
        "pw": 0,
        "r": "quorum",
        "rw": "quorum",
        "small_vclock": 50,
        "w": "quorum",
        "young_vclock": 20
    }
}
```

You can see that precommit is indeed set to our module pre_commit and
limi_size function. Now you can test the pre-commit function by posting
some objects with values under and over the 5MB threshold.

## Post-Commit Hook Example

For the post-commit example, we'll define a function to log the object
values to `cnsole.log` after they are successfully written to Riak.

Here is our example function:

```erlang
-module(post_commit).

-export([log/1]).

log(Object) ->
  error_logger:info_msg("OBJECT: ~p~n",[Object]).
```

Save this file as `post_commit.erl` and proceed to compiling.

<div class="info"><div class="title">Note on the Erlang Compiler</div> You
must use the Erlang compiler (<tt>erlc</tt>) associated with the Riak
installation or the version of Erlang used when compiling Riak from source.
For packaged Riak installations, you can consult Table 1 below for the
default location of Riak's <tt>erlc</tt> for each supported platform.
If you compiled from source, use the <tt>erlc</tt> from the Erlang version
you used to compile Riak.</div>

Compiling the function is straightforward:

```bash
erlc post_commit.erl
```

Next, you'll need to define a path from which to store and load compiled
modules. For our example, we'll use a temporary directory (`/tmp/beams`),
but you should choose a different directory for production functions
such that they will be available where needed.

Successful compilation will result in a new `.beam` file:
`post_commit.beam`. Copy this file to the `/tmp/beams` directory.

```bash
cp post_commit.beam /tmp/beams/
```

After copying the compiled module into place, you'll need to update
`app.config` to configure Riak to allow loading of compiled modules from
the directory where they're stored, again in our example case, `/tmp/beams`.

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
in the [Commands documentation](http://docs.basho.com/riak/latest/references/Command-Line-Tools---riak-admin/#wait-for-service).

<div class="note"><strong>It is important that you ensure riak_kv is
active before moving a second node</strong>.</div>

Once Riak is restarted, all that remains is to install the post-commit
hook on the target bucket(s) you wish it to operate on. In this example,
we've just one bucket, named `updates`, which we're going to install our
`post_commit` function into.

You can use Riak's HTTP interface and the `curl` command line utility to
install your named functions into into the relevant buckets. For our example,
we'll install the `post_commit` module into our `avatars` bucket, like this.

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

1. [Commit Hooks](http://docs.basho.com/riak/latest/references/appendices/concepts/Commit-Hooks/)
2. [MapReduce via the Erlang API](http://docs.basho.com/riak/latest/references/appendices/MapReduce-Implementation/#MapReduce-via-the-Erlang-API)
3. [app.config](http://docs.basho.com/riak/latest/references/Configuration-Files/#app-config)
4. [Curl](http://curl.haxx.se/)
