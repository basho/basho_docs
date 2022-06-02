---
title: "Using Commit Hooks"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Using Commit Hooks"
    identifier: "usage_commit_hooks"
    weight: 109
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.9.0p5/dev/using/commit-hooks
  - /riak/kv/2.9.0p5/dev/using/commit-hooks
  - /riak/2.9.0p5/developing/usage/commit-hooks/
  - /riak/2.9.0/developing/usage/commit-hooks/
  - /riak/kv/2.9.0/developing/usage/commit-hooks/
  - /riak/kv/2.9.0p1/developing/usage/commit-hooks/
  - /riak/kv/2.9.0p2/developing/usage/commit-hooks/
  - /riak/kv/2.9.0p3/developing/usage/commit-hooks/
  - /riak/kv/2.9.0p4/developing/usage/commit-hooks/
---


[usage bucket types]: {{<baseurl>}}riak/kv/2.9.0p5/developing/usage/bucket-types

Pre- and post-commit hooks are functions that are invoked before or
after an object has been written to Riak. To provide a few examples,
commit hooks can:

- allow a write to occur with an unmodified object
- modify an object
- fail an update and prevent any modifications to the object

Post-commit hooks are notified _after the fact_ and should not modify
the object directly. Updating Riak objects while post-commit hooks are
invoked can cause nasty feedback loops which will wedge the hook into an
infinite cycle unless the hook functions are carefully written to detect
and short-circuit such cycles.

Pre- and post-commit hooks are applied at the [bucket]({{<baseurl>}}riak/kv/2.9.0p5/learn/concepts/buckets) level,
[using bucket types][usage bucket types]. They are run once per successful response to the
client.

Both pre- and post-commit hooks are named [Erlang](http://learnyousomeerlang.com/)
functions.

## Setting Commit Hooks Using Bucket Types

Because hooks are defined at the bucket level, you can create [bucket types]({{<baseurl>}}riak/kv/2.9.0p5/developing/usage/bucket-types)
that associate one or more hooks with any bucket that bears that type.
Let's create a bucket type called `with_post_commit` that adds a
post-commit hook to operations on any bucket that bears the
`with_post_commit` type.

The format for specifying commit hooks is to identify the module (`mod`)
and then the name of the function (`fun`) as a JavaScript object. The
following specifies a commit hook called `my_custom_hook` in the module
`commit_hooks_module`:

```json
{
  "mod": "commit_hooks_module",
  "fun": "my_custom_hook"
}
```

When we create our `with_post_commit` bucket type, we add that object
to either the `precommit` or `postcommit` list in the bucket type's
properties. Pre- and post-commit hooks are stored in lists named
`precommit` and `postcommit`, respectively. Let's add the hook we
specified above to the `postcommit` property when we create our bucket
type:

```bash
riak-admin bucket-type create with_post_commit /
  '{"props":{"postcommit":["my_post_commit_hook"]}'
```

Once our bucket type has been created, we must activate it so that it
will be usable through our Riak cluster:

```bash
riak-admin bucket-type activate with_post_commit
```

If the response is `with_post_commit has been activated`, then the
bucket type is ready for use.

## Pre-Commit Hooks

Pre-commit hook Erlang functions should take a single argument, the
Riak object being modified. Remember that deletes are also considered
"writes," and so pre-commit hooks will be fired when a delete occurs in
the bucket as well. This means that hook functions will need to inspect
the object for the `X-Riak-Deleted` metadata entry (more on this in our
documentation on [object deletion]({{<baseurl>}}riak/kv/2.9.0p5/using/reference/object-deletion)) to determine whether a delete is
occurring.

Erlang pre-commit functions are allowed three possible return values:

- A Riak object --- This can either be the same object passed to the function or an updated version of the object. This allows hooks to modify the object before they are written.
- `fail` --- The atom `fail` will cause Riak to fail the write and send a 403 Forbidden error response (in the [HTTP API]({{<baseurl>}}riak/kv/2.9.0p5/developing/api/http)) along with a generic error message about why the write was blocked.
- `{fail, Reason}` --- The tuple `{fail, Reason}` will cause the same behavior as in the case above, but with the addition of `Reason` used as the error text.

Errors that occur when processing Erlang pre-commit hooks will be
reported in the `sasl-error.log` file with lines that start with
`problem invoking hook`.

#### Object Size Example

This Erlang pre-commit hook will limit object values to 5 MB or smaller:

```erlang
precommit_limit_size(Object) ->
  case erlang:byte_size(riak_object:get_value(Object)) of
    Size when Size > 5242880 -> {fail, "Object is larger than 5MB."};
    _ -> Object
  end.
```

The Erlang function `precommit_limit_size` takes the Riak object
(`Object`) as its input and runs a pattern-matching operation on the
object. If the [`erlang:byte_size`](http://www.erlang.org/doc/man/erlang.html#byte_size-1)
function determines that the object's size (determined by the `riak_object:get_value`
function) is greater than 5,242,880 (5 MB in bytes), then the commit
will return failure and the message `Object size is larger than 5 MB`.
This will stop the write. If the object is not larger than 5 MB, Riak
will return the object and allow the write to proceed.

### Chaining

The default value of the bucket type's `precommit` property is an empty
list, meaning that no pre-commit hooks are specified by default. Adding
one or more pre-commit hook functions to this list, as documented above,
will cause Riak to start evaluating those hook functions when bucket
entries are created, updated, or deleted. Riak stops evaluating
pre-commit hooks when a hook function fails the commit.

#### JSON Validation Example

Pre-commit hooks can be used in many ways in Riak. One such way to use
pre-commmit hooks is to validate data before it is written to Riak.
Below is an example that uses Javascript to validate a JSON object
before it is written to Riak.

Below is a sample JSON object that will be evaluated by the hook:

```json
{
  "user_info": {
    "name": "Mark Phillips",
    "age": "25"
  },
  "session_info": {
    "id": 3254425,
    "items": [29, 37, 34]
  }
}
```

The following hook will validate the JSON object:

```erlang
validate(Object) ->
  try
    mochijson2:decode(riak_object:get_value(Object)),
    Object
  catch
    throw:invalid_utf8 ->
      {fail, "Invalid JSON: Illegal UTF-8 character"};
    error:Error ->
      {fail, lists:flatten(io_lib:format("Invalid JSON: ~p",[Error]))}
  end.
```

**Note**: All pre-commit hook functions are executed for each create and update operation.

## Post-Commit Hooks

Post-commit hooks are run after a write has completed successfully. More
specifically, the hook function is called immediately before the calling
process is notified of the successful write.

Hook functions must accept a single argument: the object instance just
written. The return value of the function is ignored. As with pre-commit
hooks, deletes are considered writes, so post-commit hook functions will
need to inspect the object's metadata for the presence of `X-Riak-Deleted`
to determine whether a delete has occurred. As with pre-commit hooks,
errors that occur when processing post-commit hooks will be reported in
the `sasl-error.log` file with lines that start with `problem invoking hook`.

#### Example

The following post-commit hook creates a secondary index on the `email`
field of a JSON object:

```erlang
postcommit_index_on_email(Object) ->
    %% Determine the target bucket name
    Bucket = erlang:iolist_to_binary([riak_object:bucket(Object),"_by_email"]),

    %% Decode the JSON body of the object
    {struct, Properties} = mochijson2:decode(riak_object:get_value(Object)),

    %% Extract the email field
    {<<"email">>,Key} = lists:keyfind(<<"email">>,1,Properties),

    %% Create a new object for the target bucket
    %% NOTE: This doesn't handle the case where the
    %%       index object already exists!
    IndexObj = riak_object:new(
        Bucket, Key, <<>>, %% no object contents
            dict:from_list(
                [
                    {<<"content-type">>, "text/plain"},
                    {<<"Links">>,
                        [
                            {
                                {riak_object:bucket(Object), riak_object:key(Object)},
                                <<"indexed">>
                            }]}
                        ]
            )
    ),

    %% Get a riak client
    {ok, C} = riak:local_client(),

    %% Store the object
    C:put(IndexObj).
```


### Chaining

The default value of the bucket `postcommit` property is an empty list,
meaning that no post-commit hooks are specified by default. Adding one
or more post-commit hook functions to the list, as documented above,
will cause Riak to start evaluating those hook functions immediately
after data has been created, updated, or deleted. Each post-commit hook
function runs in a separate process so it's possible for several hook
functions, triggered by the same update, to execute in parallel.

**Note**: All post-commit hook functions are executed for each create,
update, or delete.
