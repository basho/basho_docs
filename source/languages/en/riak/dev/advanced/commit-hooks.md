---
title: Advanced Commit Hooks
project: riak
version: 1.1.0+
document: guide
toc: true
audience: advanced
keywords: [developers, commit-hooks, beam]
moved: {
  '1.4.0-': '/cookbooks/Erlang-Named-Functions'
}
---

Riak supports the use of Erlang named functions in compiled modules for
pre-commit hooks, post-commit hooks, and MapReduce operations. This cookbook
explains the process for using your own named functions, including module
compilation, configuration, and installation steps with simple examples
detailed for each use case.

## Pre-Commit Hook Example

For the pre-commit hook example, we'll define a function to validate the
JSON content of a key's value before writing the key and value to the
bucket that the pre-commit hook is installed into.

Here is our example pre-commit `validate_json` module and its
corresponding `validate` function:

```erlang
-module(validate_json).
-export([validate/1]).

validate(Object) ->
  try
    mochijson2:decode(riak_object:get_value(Object)),
    Object
  catch
    throw:invalid_utf8 ->
      {fail, "Invalid JSON: Illegal UTF-8 character"};
    error:Error ->
      {fail, "Invalid JSON: " ++ binary_to_list(list_to_binary(io_lib:format("~p", [Error])))}
  end.
```

Save this file as `validate_json.erl` and proceed to compiling the module.

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

<div class="info">Ensure that the directory chosen above can be read by
the <tt>riak</tt> user.</div>

Successful compilation will result in a new `.beam` file,
`validate_json.beam`.

Send this file to your operator, or read about [[installing custom code]]
on your Riak nodes.

Once Riak is restarted, all that remains is to install the pre-commit
hook into the target bucket(s) you wish it to operate on. In this example,
we've just one bucket, named `messages`, which we're going to install our
`validate` pre-commit function into.

You can use Riak's HTTP interface and the `curl` command line utility to
install your named functions into into the relevant bucket(s). For our
example, we'll install the `validate_json` module with its `validate`
function into our `messages` bucket, like this:

```curl
$ curl -XPUT -H "Content-Type: application/json" \
  http://127.0.0.1:8098/buckets/messages/props    \
  -d '{"props":{"precommit":[{"mod": "validate_json", "fun": "validate"}]}}'
```

Check that the bucket has your pre-commit hook listed in its properties.

```curl
$ curl http://localhost:8098/buckets/messages/props | jsonpp
```

The output should look like this:

```json
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
        "name": "messages",
        "notfound_ok": true,
        "old_vclock": 86400,
        "postcommit": [],
        "pr": 0,
        "precommit": [
            {
                "fun": "validate_json",
                "mod": "validate"
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

You can see that `precommit` is indeed set to our `validate_json` module and
`validate` function. Now you can test the pre-commit hook function by posting
some objects with JSON values including some with invalid JSON.

```curl
$ curl -XPUT localhost:8098/buckets/messages/keys/1 \
       -H 'Content-Type: application/json' -d@msg3.json
```

The response when `msg3.json` contains invalid JSON:

```bash
Invalid JSON: {case_clause,{{const,<<"authorName">>},{decoder,null,160,1,161,comma}}}
```

## Post-Commit Hook Example

For the post-commit example, we'll define a simple function to log the object
values to `console.log` after they are successfully written to Riak.

Here is our example post-commit function:

```erlang
-module(log_object).
-export([log/1]).

log(Object) ->
  error_logger:info_msg("OBJECT: ~p~n",[Object]).
```

Save this file as `log_object.erl` and proceed to compiling the module.

<div class="info"><div class="title">Note on the Erlang Compiler</div> You
must use the Erlang compiler (<tt>erlc</tt>) associated with the Riak
installation or the version of Erlang used when compiling Riak from source.
For packaged Riak installations, you can consult Table 1 above for the
default location of Riak's <tt>erlc</tt> for each supported platform.
If you compiled from source, use the <tt>erlc</tt> from the Erlang version
you used to compile Riak.</div>

Compiling the module is straightforward:

```bash
$ erlc log_object.erl
```

Next, you'll need to define a path from which compiled modules can be stored
and loaded.

Just like pre-commit hooks, send this file to your operator,
or read about [[installing custom code]] on your Riak nodes.

Once Riak is restarted, all that remains is to install the post-commit
hook on the target bucket(s) you wish it to operate on. In this example,
we've just one bucket, named `updates`, which we're going to install our
`log` function into.

You can use Riak's HTTP interface and the `curl` command line utility to
install your named functions into into the relevant buckets. For our example,
we'll install the `log_object` module and its `log` function into our
`messages` bucket, like this.

```curl
$ curl -XPUT -H "Content-Type: application/json" \
  http://127.0.0.1:8098/buckets/updates/props    \
  -d '{"props":{"postcommit":[{"mod": "log_object", "fun": "log"}]}}'
```

Check that the bucket has your post-commit hook listed in its properties.

```curl
curl localhost:8098/buckets/updates/props | jsonpp
```

The output should look like this:

```json
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
                "mod": "log_object"
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

You can see that `postcommit` is indeed set to our `log_object` module and
`log` function. Now you can test the post-commit function by posting an
object and viewing `console.log`.

```curl
$ curl -XPUT localhost:8098/buckets/updates/keys/2 \
       -H 'Content-Type: application/json' -d@msg2.json
```

You can see the logged value of the object by viewing `console.log`.

```log
2012-12-10 13:14:37.840 [info] <0.2101.0> OBJECT: {r_object,<<"updates">>,<<"2">>,[{r_content,{dict,6,16,16,8,80,48,{[],[],[],
[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[[<<"Links">>]],[],[],[],[],
[],[],[],[[<<"content-type">>,97,112,112,108,105,99,97,116,105,111,110,47,
106,115,111,110],[<<"X-Riak-VTag">>,52,114,79,84,75,73,90,73,83,105,49,101,
120,53,87,103,106,110,56,71,114,83]],[[<<"index">>]],[],
[[<<"X-Riak-Last-Modified">>|{1355,163277,837883}]],[],
[[<<"X-Riak-Meta">>]]}}},<<"{    \"id\": 1,    \"jsonrpc\": \"2.0\",
\"total\": 1,    \"result\": [        {            \"id\": 1,
\"author\": \"foo@example.com\",            \"authorName\": \"Foo Bar\",
\"text\": \"Home of the example cocktail\"        }
]}">>}],[{<<35,9,254,249,80,193,17,247>>,{1,63522382477}}],{dict,1,16,16,8,
80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],
[],[],[],[],[],[],[],[],[[clean|true]],[]}}},undefined}
```
