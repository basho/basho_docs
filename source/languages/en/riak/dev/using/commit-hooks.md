---
title: Using Commit Hooks
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: beginner
keywords: [developers, commit-hooks]
moved: {
  '1.4.0-': '/references/appendices/concepts/Commit-Hooks'
}
---

## Overview

Pre- and post-commit hooks are invoked before or after a `riak_object` is persisted and can greatly enhance the functionality of any application. Commit hooks can accomplish, for example, something like the following:

- Allow a write to occur with an unmodified object
- Modify the object
- Fail the update and prevent any modifications

Post-commit hooks are invoked after the fact and should not modify the Riak object itself. Updating a Riak in post-commit hooks can cause nasty feedback loops which will wedge the hook into an infinite cycle unless the hook functions are carefully written to detect and short-circuit such cycles.

Pre- and post-commit hooks are defined on a per-bucket basis and applied using [[bucket types|Using Bucket Types]]. They are run once per successful response to the client.

## Configuration

Configuring pre- and post-commit hooks is very easy. Simply add a reference to your hook function to the list of functions stored in the correct bucket property in your bucket type. Pre-commit hooks are stored under the property `precommit`, whereas post-commit hooks use the bucket property `postcommit`.

Pre-commit hooks can be implemented either as named Javascript functions or as Erlang functions. The configuration for each is given below:

#### JavaScript

```json
{
  "name": "Foo.beforeWrite"
}
```

#### Erlang

```json
{
  "mod": "foo",
  "fun": "beforeWrite"
}
```

Post-commit hooks can be implemented in Erlang only, which is described in more detail under [[Advanced Commit Hooks]]. The reason for this restriction is that Javascript cannot call Erlang code and is thus prevented from doing anything useful in Riak. Post-commit hooks use the same function reference syntax as pre-commit hooks.

## Pre-Commit Hooks

### API & Behavior

Pre-commit hook functions should take a single argument: the Riak object (`riak_object`) being modified. Remember that deletes are also considered "writes," so pre-commit hooks will be fired when a delete occurs. Hook functions will need to inspect the object for the `X-Riak-Deleted` metadata entry to determine when a delete is occurring.

Erlang pre-commit functions are allowed three possible return values:

- A `riak_object` --- This can be either the same object passed to the function or an updated version. This allows hooks to modify the object before the hooks are written.
- `fail` --- The atom `fail` will cause Riak to fail the write and send a `403 Forbidden` message along with a generic error message about why the write was blocked.
- `{fail, Reason}` --- The tuple `{fail, Reason}` will cause the same behavior as when `fail` is used, with the addition of `Reason` used as the error text.

Errors that occur when processing Erlang pre-commit hooks will be reported in the `sasl-error.log` file with lines that start with `problem invoking hook`.

#### Erlang Pre-commit Example

The following pre-commit hook limits object values to 5 MB or smaller:

```erlang
precommit_limit_size(Object) ->
  case erlang:byte_size(riak_object:get_value(Object)) of
    Size when Size > 5242880 -> {fail, "Object is larger than 5MB."};
    _ -> Object
  end.
```

Javascript pre-commit functions should also take a single argument: the JSON-encoded version of the `riak_object` being modified. The JSON format is exactly the same as Riak's MapReduce. Javascript pre-commit functions are allowed three possible return values:

- A JSON-encoded Riak object --- Aside from using JSON, this is exactly the
same as using `riak_object` for Erlang functions. Riak will automatically convert it back to its native format before writing.
- `fail` --- The Javascript string `fail` will cause Riak to fail the write in exactly the same way as for Erlang functions.
- `{"fail": Reason}` --- The JSON hash will have the same effect as for Erlang functions. `Reason` must be a Javascript string.

#### Javascript Pre-commit Example

```javascript
// Makes sure the object has JSON contents
function precommitMustBeJSON(object){
  try {
    Riak.mapValuesJson(object);
    return object;
  } catch(e) {
    return {"fail":"Object is not JSON"};
  }
}
```

### Chaining

The default value of the bucket `precommit` property is an empty list. Adding one or more pre-commit hook functions to the list, as documented above, will cause Riak to start evaluating those hook functions when bucket entries are created, updated, or deleted. Riak stops evaluating pre-commit hooks when a hook function fails the commit.

#### Example

Pre-commit hooks can be used in many ways in Riak. One such way is to validate data before it is written to Riak. Below is an example that uses Javascript to validate a JSON object before it is written to Riak:

```javascript
//Sample Object
{
  "user_info": {
    "name": "Mark Phillips",
    "age": "25",
  },
  "session_info": {
    "id": 3254425,
    "items": [29, 37, 34]
  }
}

var PreCommit = {
  validate: function(obj) {

    // A delete is a type of put in Riak so check and see what this
    // operation is doing

    if (obj.values[[0]][['metadata']][['X-Riak-Deleted']]){
      return obj;
    }

    // Make sure the data is valid JSON
    try {
       data = JSON.parse(obj.values[[0]].data);
       validateData(data);

    } catch(error) {
      return { "fail": "Invalid Object: " + error }
    }
    return obj;
  }
};

function validateData(data) {
  // Validates that user_info object is in the data
  // and that name and age aren't empty, finally
  // the session_info items array is checked and validated as
  // being populated

  if (
    data.user_info != null &&
    data.user_info.name != null &&
    data.user_info.age != null &&
    data.session_info.items.length > 0
  ) {
    return true;
  } else {
    throw("Invalid data");
  }
}
```

## Post-Commit Hooks

### API & Behavior

Post-commit hooks are run after a write has completed successfully. More specifically, the hook function is called by `riak_kv_put_fsm` immediately before the calling process is notified of the successful write. Hook functions must accept a single argument: the `riak_object` instance just written. The return value of the function is ignored. As with pre-commit hooks, deletes are considered writes, so post-commit hook functions will need to inspect object metadata for the presence of `X-Riak-Deleted` to determine when a delete has occurred.  Errors that occur when processing post-commit hooks will be reported in the `sasl-error.log` file with lines that start with `problem invoking hook`.

##### Example

The following Erlang post-commit hook creates a native secondary index in the `email` field of a JSON object:

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
    IndexObj = riak_object:new(Bucket, Key,<<>>, %% no object contents
                               dict:from_list(
                                 [
                                  {<<"content-type">>, "text/plain"},
                                  {<<"Links">>,
                                   [
                                    {{riak_object:bucket(Object), riak_object:key(Object)},<<"indexed">>}]}
                                 ])),
    %% Get a riak client
    {ok, C} = riak:local_client(),
    %% Store the object
    C:put(IndexObj).
```


### Chaining

The default value of the `postcommit` bucket property is an empty list. Adding one or more post-commit hook functions to the list, as documented above, will cause Riak to start evaluating those hook functions immediately after data has been created, updated, or deleted. Each post-commit hook function runs in a separate process, so it's possible for several hook functions, triggered by the same update, to execute in parallel. _All post-commit hook functions are executed for each create, update, or delete._
