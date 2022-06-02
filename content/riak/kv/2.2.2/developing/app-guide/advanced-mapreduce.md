---
title: "Advanced MapReduce"
description: ""
project: "riak_kv"
project_version: "2.2.2"
menu:
  riak_kv-2.2.2:
    name: "Advanced MapReduce"
    identifier: "app_guide_mapreduce"
    weight: 103
    parent: "developing_app_guide"
toc: true
aliases:
  - /riak/2.2.2/dev/advanced/mapreduce/
  - /riak/kv/2.2.2/dev/advanced/mapreduce/
---

[usage 2i]: {{<baseurl>}}riak/kv/2.2.2/developing/usage/secondary-indexes
[apps replication properties]: {{<baseurl>}}riak/kv/2.2.2/developing/app-guide/replication-properties
[use ref custom code]: {{<baseurl>}}riak/kv/2.2.2/using/reference/custom-code
[usage bucket types]: {{<baseurl>}}riak/kv/2.2.2/developing/usage/bucket-types
[glossary vnode]: {{<baseurl>}}riak/kv/2.2.2/learn/glossary/#vnode
[config reference]: {{<baseurl>}}riak/kv/2.2.2/configuring/reference
[google mr]: http://research.google.com/archive/mapreduce.html
[mapping list]: http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:map
[function contrib]: https://github.com/basho/riak_function_contrib
[erlang client]: https://github.com/basho/riak-erlang-client
[`set-union`]: http://en.wikipedia.org/wiki/Union_(set_theory)#Definition


> **Use MapReduce sparingly**
>
> In Riak KV, MapReduce is the primary method for non-primary-key-based
querying. Although useful for tasks such as batch
processing jobs, MapReduce operations can be very computationally
expensive, to the extent that they can degrade performance in
production clusters operating under load. Because of this potential for performance degradation, we recommend running
MapReduce operations in a controlled, rate-limited fashion and never for
realtime querying purposes.

MapReduce, the data processing paradigm popularized by
[Google][google mr], is provided by Riak KV to aggregate
results as background batch processes.

## MapReduce

In Riak KV, MapReduce is one of the primary methods for
non-primary-key-based querying alongside
[secondary indexes][usage 2i].  Riak KV allows you to
run MapReduce jobs using Erlang or JavaScript.

{{% note title="Deprecation Warning" %}}
Javascript MapReduce is deprecated and will be removed in a future version.
{{% /note %}}


### Why Do We Use MapReduce for Querying Riak KV?

Key/value stores like Riak KV generally do not offer the kinds of complex
querying capabilities found in other data storage systems, such as
relational databases. MapReduce enables you to perform powerful queries
over the data stored in Riak KV but should be used with caution.

The main goal of MapReduce is to spread the processing of a query across
many systems to take advantage of parallel processing power. This is
generally done by dividing the query into several steps, i.e. dividing
the dataset into several chunks and then running those step/chunk pairs
on separate physical hosts. Riak KV's MapReduce has an additional goal:
increasing data locality. When processing a large dataset, it's often
much more efficient to take the computation to the data than it is to
bring the data to the computation.

"Map" and "Reduce" are phases in the query process. Map functions take
one piece of data as input and produce zero or more results as output.
If you're familiar with [mapping over a list][mapping list]
in functional programming languages, you're already familiar with the
"Map" steps in a MapReduce query.

## MapReduce caveats

MapReduce should generally be treated as a fallback rather than a
standard part of an application. There are often ways to model data
such that dynamic queries become single key retrievals, which are
dramatically faster and more reliable in Riak KV, and tools such as Riak
search and 2i are simpler to use and may place less strain on a
cluster.

### R=1

One consequence of Riak KV's processing model is that MapReduce queries
have an effective `R` value of 1. The queries are distributed
to a representative sample of the cluster where the data is expected to
be found, and if one server lacks a copy of data it's supposed to have,
a MapReduce job will not attempt to look for it elsewhere.

For more on the value of `R`, see our documentation on [replication properties][apps replication properties].

### Key lists

Asking Riak KV to generate a list of all keys in a production environment
is generally a bad idea. It's an expensive operation.

Attempting to constrain that operation to a bucket (e.g.,
`mapred_bucket` as used below) does not help because Riak KV must still
pull all keys from storage to determine which ones are in the
specified bucket.

If at all possible, run MapReduce against a list of known keys.

### Code distribution

As we'll discuss in this document, the functions invoked from Erlang
MapReduce must be available on all servers in the cluster unless
using the client library from an Erlang shell.

### Security restrictions

If Riak's security functionality is enabled, there are two
restrictions on MapReduce that come into play:

* The `riak_kv.mapreduce` permission must be granted to the user (or
  via the user's groups)
* Other than the module `riak_kv_mapreduce`, any Erlang modules
  distributed with Riak KV will **not** be accessible to custom MapReduce
  code unless made available via the `add_path` mechanism documented
  in [Installing Custom Code][use ref custom code].

## How Riak KV's MapReduce Queries Are Specified

MapReduce queries in Riak KV have two components: (1) a list of inputs and
(2) a list of "steps," or "phases."

Each element of the input list is an object location, as specified by
[bucket type][usage bucket types], bucket, and key. This location may
also be annotated with "key-data," which will be passed as an
argument to a map function when evaluated on the object stored under
that bucket-key pair.

Each element of the phases list is a description of a map function, a
reduce function, or a link function. The description includes where to
find the code for the phase function (for map and reduce phases), static
data passed to the function every time it is executed during that phase,
and a flag indicating whether or not to include the results of that
phase in the final output of the query.

The phase list describes the chain of operations through which each
input will flow. That is, the initial inputs will be fed to the first
phase in the list and the output of that phase will be fed as input to
the next phase in the list. This stream will continue through the final
phase.

## How Phases Work

### Map Phase

The input list to a map phase must be a list of (possibly annotated)
bucket-key pairs. For each pair, Riak KV will send the request to evaluate
the map function to the partition that is responsible for storing the
data for that bucket-key. The [vnode][glossary vnode] hosting that partition
will look up the object stored under that bucket-key and evaluate the
map function with the object as an argument. The other arguments to the
function will be the annotation, if any is included, with the
bucket-key, and the static data for the phase, as specified in the
query.

{{% note title="Tombstones" %}}
Be aware that most Riak KV clusters will retain deleted objects for some
period of time (3 seconds by default), and the MapReduce framework does
not conceal these from submitted jobs. These tombstones can be
recognized and filtered out by looking for `X-Riak-Deleted`
in the object metadata with a value of `true`.
{{% /note %}}

### Reduce Phase

Reduce phases accept any list of data as input, and produce any list of
data as output. They also receive a phase-static value, specified in the
query definition.

The most important thing to understand is that the function defining the
reduce phase may be evaluated multiple times, and the input of later
evaluations will include the output of earlier evaluations.

For example, a reduce phase may implement the
[`set-union`] function. In that case, the first set of inputs might be `[1,2,2,3]`,
and the output would be `[1,2,3]`. When the phase receives more inputs,
say `[3,4,5]`, the function will be called with the concatenation of the
two lists: `[1,2,3,3,4,5]`.

Other systems refer to the second application of the reduce function as
a "re-reduce." There are at least a few reduce-query implementation
strategies that work with Riak KV's model.

One strategy is to implement the phase preceding the reduce phase such
that its output is "the same shape" as the output of the reduce phase.
This is how the examples in this document are written, and the way that
we have found produces the cleanest code.

An alternative strategy is to make the output of a reduce phase
recognizable such that it can be extracted from the input list on
subsequent applications. For example, if inputs from the preceding phase
are numbers, outputs from the reduce phase could be objects or strings.
This would allow the function to find the previous result and apply new
inputs to it.

### How a Link Phase Works in Riak KV

Link phases find links matching patterns specified in the query
definition. The patterns specify which buckets and tags links must have.

"Following a link" means adding it to the output list of this phase. The
output of this phase is often most useful as input to a map phase or to
another reduce phase.

## Invoking MapReduce

To illustrate some key ideas, we'll define a simple module that
implements a map function to return the key value pairs contained in a
bucket and use it in a MapReduce query via Riak KV's HTTP API.

Here is our example MapReduce function:

```erlang
-module(mr_example).

-export([get_keys/3]).

% Returns bucket and key pairs from a map phase
get_keys(Value,_Keydata,_Arg) ->
  [{riak_object:bucket(Value),riak_object:key(Value)}].
```

Save this file as `mr_example.erl` and proceed to compiling the module.

{{% note title="Note on the Erlang Compiler" %}}
You must use the Erlang compiler (`erlc`) associated with the
Riak KV installation or the version of Erlang used when compiling Riak KV from
source.
{{% /note %}}

Compiling the module is a straightforward process:

```bash
erlc mr_example.erl
```

Successful compilation will result in a new `.beam` file, `mr_example.beam`.

Send this file to your operator, or read about [installing custom code][use ref custom code]
on your Riak KV nodes. Once your file has been installed, all that
remains is to try the custom function in a MapReduce query. For
example, let's return keys contained within a bucket named `messages`
(please pick a bucket which contains keys in your environment).

```curl
curl -XPOST localhost:8098/mapred \
  -H 'Content-Type: application/json'   \
  -d '{"inputs":"messages","query":[{"map":{"language":"erlang","module":"mr_example","function":"get_keys"}}]}'
```

The result should be a JSON map of bucket and key names expressed as key/value pairs.

{{% note %}}
Be sure to install the MapReduce function as described above on all of
the nodes in your cluster to ensure proper operation.
{{% /note %}}


## Phase functions

MapReduce phase functions have the same properties, arguments, and
return values whether you write them in Javascript or Erlang.

### Map phase functions

Map functions take three arguments (in Erlang, arity-3 is required).
Those arguments are:

  1. `Value`: the value found at a key. This will be a Riak object, which
    in Erlang is defined and manipulated by the `riak_object` module.
    In Javascript, a Riak object looks like this:

    ```javascript
    {
     "bucket_type" : BucketTypeAsString,
     "bucket" : BucketAsString,
     "key" : KeyAsString,
     "vclock" : VclockAsString,
     "values" : [
            {
                "metadata" : {
                    "X-Riak-VTag":VtagAsString,
                    "X-Riak-Last-Modified":LastModAsString,
                    "Links":[...List of link objects],
                    // ...other metadata...
                },
                "data" : ObjectData
            },
            // ...other metadata/data values (siblings)...
        ]
    }
    ```
  2. *KeyData* : key data that was submitted with the inputs to the query or phase.
  3. *Arg* : a static argument for the entire phase that was submitted with the query.

A map phase should produce a list of results. You will see errors if
the output of your map function is not a list. Return the empty list if
your map function chooses not to produce output. If your map phase is
followed by another map phase, the output of the function must be
compatible with the input to a map phase - a list of bucket-key pairs or
`bucket-key-keydata` triples.

#### Map function examples

These map functions return the value (data) of the object being mapped:

```erlang
fun(Value, _KeyData, _Arg) ->
    [riak_object:get_value(Value)]
end.
```

These map functions filter their inputs based on the arg and return bucket-key pairs for a subsequent map phase:

```erlang
fun(Value, _KeyData, Arg) ->
  Key = riak_object:key(Value),
  Bucket = riak_object:bucket(Value),
  case erlang:byte_size(Key) of
    L when L > Arg ->
      [{Bucket,Key}];
    _ -> []
  end
end.
```

### Reduce phase functions

Reduce functions take two arguments. Those arguments are:

1. *ValueList*: the list of values produced by the preceding phase in the MapReduce query.
2. *Arg* : a static argument for the entire phase that was submitted with the query.

A reduce function should produce a list of values, but it must also be
true that the function is commutative, associative, and idempotent. That
is, if the input list `[a,b,c,d]` is valid for a given F, then all of
the following must produce the same result:


```erlang
  F([a,b,c,d])
  F([a,d] ++ F([c,b]))
  F([F([a]),F([c]),F([b]),F([d])])
```

#### Reduce function examples

These reduce functions assume the values in the input are numbers and
sum them:

```erlang
fun(Values, _Arg) ->
  [lists:foldl(fun erlang:'+'/2, 0, Values)]
end.
```

These reduce functions sort their inputs:

```erlang
fun(Values, _Arg) ->
  lists:sort(Values)
end.
```

## MapReduce Examples

Riak KV supports describing MapReduce queries in Erlang syntax through the
Protocol Buffers API. This section demonstrates how to do so using the
Erlang client.

{{% note title="Distributing Erlang MapReduce Code" %}}
Any modules and functions you use in your Erlang MapReduce calls must be
available on all nodes in the cluster. Please read about
[installing custom code]({{<baseurl>}}riak/kv/2.2.2/using/reference/custom-code).
{{% /note %}}

### Erlang Example

Before running some MapReduce queries, let's create some objects to
run them on.  Unlike the first example when we compiled
`mr_example.erl` and distributed it across the cluster, this time
we'll use the [Erlang client library][erlang client] and shell.

```erlang
1> {ok, Client} = riakc_pb_socket:start("127.0.0.1", 8087).
2> Mine = riakc_obj:new(<<"groceries">>, <<"mine">>,
                        term_to_binary(["eggs", "bacon"])).
3> Yours = riakc_obj:new(<<"groceries">>, <<"yours">>,
                         term_to_binary(["bread", "bacon"])).
4> riakc_pb_socket:put(Client, Yours, [{w, 1}]).
5> riakc_pb_socket:put(Client, Mine, [{w, 1}]).
```

Now that we have a client and some data, let's run a query and count how
many occurrences of groceries.

```erlang
6> Count = fun(G, undefined, none) ->
             [dict:from_list([{I, 1}
              || I <- binary_to_term(riak_object:get_value(G))])]
           end.
7> Merge = fun(Gcounts, none) ->
             [lists:foldl(fun(G, Acc) ->
                            dict:merge(fun(_, X, Y) -> X+Y end,
                                       G, Acc)
                          end,
                          dict:new(),
                          Gcounts)]
           end.
8> {ok, [{1, [R]}]} = riakc_pb_socket:mapred(
                         Client,
                         [{<<"groceries">>, <<"mine">>},
                          {<<"groceries">>, <<"yours">>}],
                         [{map, {qfun, Count}, none, false},
                          {reduce, {qfun, Merge}, none, true}]).
9> L = dict:to_list(R).
```

{{% note title="Riak Object Representations" %}}
Note how the `riak_object` module is used in the MapReduce
function but the `riakc_obj` module is used on the client.
Riak objects are represented differently internally to the cluster than
they are externally.
{{% /note %}}

Given the lists of groceries we created, the sequence of commands above
would result in L being bound to `[{"bread",1},{"eggs",1},{"bacon",2}]`.

### Erlang Query Syntax

`riakc_pb_socket:mapred/3` takes a client and two lists as arguments.
The first list contains bucket-key pairs.  The second list contains
the phases of the query.

`riakc_pb_socket:mapred_bucket/3` replaces the first list of
bucket-key pairs with the name of a bucket; see the warnings above
about using this in a production environment.

#### Inputs

The `mapred/3` input objects are given as a list of tuples in the
format `{Bucket, Key}` or `{{Bucket, Key}, KeyData}`. `Bucket` and
`Key` should be binaries, and `KeyData` can be any Erlang term.  The
former form is equivalent to `{{Bucket,Key},undefined}`.

#### Query

The query is given as a list of map, reduce and link phases. Map and
reduce phases are each expressed as tuples in the following form:


```erlang
{Type, FunTerm, Arg, Keep}
```

`Type` is an atom, either `map` or `reduce`. `Arg` is a static argument
(any Erlang term) to pass to each execution of the phase. `Keep` is
either `true` or `false` and determines whether results from the phase
will be included in the final value of the query. Riak KV assumes that the
final phase will return results.

`FunTerm` is a reference to the function that the phase will execute and
takes any of the following forms:

* `{modfun, Module, Function}` where `Module` and `Function` are atoms
  that name an Erlang function in a specific module
* `{qfun,Fun}` where `Fun` is a callable fun term (closure or anonymous
  function)
* `{jsfun,Name}` where `Name` is a binary that, when evaluated in
  Javascript, points to a built-in Javascript function
* `{jsanon, Source}` where `Source` is a binary that, when evaluated in
  Javascript is an anonymous function
* `{jsanon, {Bucket, Key}}` where the object at `{Bucket, Key}` contains
  the source for an anonymous Javascript function

{{% note title="qfun Note" %}}
Using `qfun` in compiled applications can be a fragile
operation. Please keep the following points in mind:

1. The module in which the function is defined must be present and
exactly the same version on both the client and Riak KV nodes.

2. Any modules and functions used by this function (or any function in
the resulting call stack) must also be present on the Riak KV nodes.

Errors about failures to ensure both 1 and 2 are often surprising,
usually seen as opaque missing-function  or function-clause
errors. Especially in the case of differing module versions, this can be
difficult to diagnose without expecting the issue and knowing of
`Module:info/0`.

When using the Erlang shell, anonymous MapReduce functions can be
defined and sent to Riak KV instead of deploying them to all servers in
advance, but condition #2 above still holds.
{{% /note %}}

Link phases are expressed in the following form:


```erlang
{link, Bucket, Tag, Keep}
```


`Bucket` is either a binary name of a bucket to match, or the atom `_`,
which matches any bucket. `Tag` is either a binary tag to match, or the
atom `_`, which matches any tag. `Keep` has the same meaning as in map
and reduce phases.


> There are a small group of prebuilt Erlang MapReduce functions available
with Riak KV. Check them out [on GitHub](https://github.com/basho/riak_kv/blob/master/src/riak_kv_mapreduce.erl).

## Bigger Data Examples

### Loading Data

This Erlang script will load historical stock-price data for Google
(ticker symbol "GOOG") into your existing Riak KV cluster so we can use it.
Paste the code below into a file called `load_data.erl` inside the `dev`
directory (or download it below).

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    io:format("Requires one argument: filename with the CSV data~n");
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = tl(re:split(Data, "\r?\n", [{return, binary},trim])),
    lists:foreach(fun(L) -> LS = re:split(L, ","), format_and_insert(LS) end, Lines).

format_and_insert(Line) ->
    JSON = io_lib:format("{\"Date\":\"~s\",\"Open\":~s,\"High\":~s,\"Low\":~s,\"Close\":~s,\"Volume\":~s,\"Adj. Close\":~s}", Line),
    Command = io_lib:format("curl -XPUT http://127.0.0.1:8098/buckets/goog/keys/~s -d '~s' -H 'content-type: application/json'", [hd(Line),JSON]),
    io:format("Inserting: ~s~n", [hd(Line)]),
    os:cmd(Command).
```

Make the script executable:

```bash
chmod +x load_data.erl
```

Download the CSV file of stock data linked below and place it in the
`dev` directory where we've been working.

* [goog.csv](https://github.com/basho/basho_docs/raw/master/extras/data/goog.csv) --- Google historical stock data
* [load_stocks.rb](https://github.com/basho/basho_docs/raw/master/extras/code-examples/load_stocks.rb) --- Alternative script in Ruby to load the data
* [load_data.erl](https://github.com/basho/basho_docs/raw/master/extras/code-examples/load_data.erl) --- Erlang script to load data (as shown in snippet)

Now load the data into Riak KV.

```bash
./load_data.erl goog.csv
```


### Map only: find the days on which the high was over $600.00

From the Erlang shell with the client library loaded, let's define a
function which will check each value in our `goog` bucket to see if
the stock's high for the day was above $600.

```erlang
> HighFun = fun(O, _, LowVal) ->
>   {struct, Map} = mochijson2:decode(riak_object:get_value(O)),
>   High = proplists:get_value(<<"High">>, Map, -1.0),
>   case High > LowVal of
>      true -> [riak_object:key(O)];
>      false -> []
> end end.
#Fun<erl_eval.18.80484245>
```

Now we'll use `mapred_bucket/3` to send that function to the cluster.

```erlang
> riakc_pb_socket:mapred_bucket(Riak, <<"goog">>, [{map, {qfun, HighFun}, 600, true}]).
    {ok,[{0,
      [<<"2007-11-29">>,<<"2008-01-02">>,<<"2008-01-17">>,
       <<"2010-01-08">>,<<"2007-12-05">>,<<"2007-10-24">>,
       <<"2007-10-26">>,<<"2007-10-11">>,<<"2007-11-09">>,
       <<"2007-12-06">>,<<"2007-12-19">>,<<"2007-11-01">>,
       <<"2007-11-07">>,<<"2007-11-16">>,<<"2009-12-28">>,
       <<"2007-12-26">>,<<"2007-11-05">>,<<"2008-01-16">>,
       <<"2007-11-13">>,<<"2007-11-08">>,<<"2007-12-07">>,
       <<"2008-01-"...>>,<<"2007"...>>,<<...>>|...]}]}
```

#### Map only: find the days on which the close is lower than open

This example is slightly more complicated: instead of comparing a
single field against a fixed value, we're looking for days when the
stock declined.

```erlang
> CloseLowerFun = fun(O, _, _) ->
>    {struct, Map} = mochijson2:decode(riak_object:get_value(O)),
>    Close = proplists:get_value(<<"Close">>, Map, -1.0),
>    Open = proplists:get_value(<<"Open">>, Map, -2.0),
>    case Close < Open of
>       true -> [riak_object:key(O)];
>       false -> []
> end end.
#Fun<erl_eval.18.80484245>

> riakc_pb_socket:mapred_bucket(Riak, <<"goog">>, [{map, {qfun, CloseLowerFun}, none, true}]).
{ok,[{0,
      [<<"2008-05-13">>,<<"2008-12-19">>,<<"2009-06-10">>,
       <<"2006-07-06">>,<<"2006-07-07">>,<<"2009-02-25">>,
       <<"2009-07-17">>,<<"2005-10-05">>,<<"2006-08-18">>,
       <<"2008-10-30">>,<<"2009-06-18">>,<<"2006-10-26">>,
       <<"2008-01-17">>,<<"2010-04-16">>,<<"2007-06-29">>,
       <<"2005-12-12">>,<<"2008-08-20">>,<<"2007-03-30">>,
       <<"2006-07-20">>,<<"2006-10-24">>,<<"2006-05-26">>,
       <<"2007-02-"...>>,<<"2008"...>>,<<...>>|...]}]}
```

#### Map and Reduce: find the maximum daily variance in price by month

Here things start to get tricky. We'll use map to determine each day's
rise or fall, and our reduce phase will identify each month's largest
variance.

```erlang
DailyMap = fun(O, _, _) ->
   {struct, Map} = mochijson2:decode(riak_object:get_value(O)),
   Date = binary_to_list(proplists:get_value(<<"Date">>, Map, "0000-00-00")),
   High = proplists:get_value(<<"High">>, Map, 0.0),
   Low = proplists:get_value(<<"Low">>, Map, 0.0),
   Month = string:substr(Date, 1, 7),
   [{Month, abs(High - Low)}]
end.

MonthReduce = fun(List, _) ->
    {Highs, _} = lists:foldl(
      fun({Month, _Value}=Item, {Accum, PrevMonth}) ->
              case Month of
                  PrevMonth ->
                      %% Highest value is always first in the list, so
                      %% skip over this one
                      {Accum, PrevMonth};
                  _ ->
                      {[Item] ++ Accum, Month}
              end
      end,
      {[], ""},
      List),
    Highs
    end.
> riakc_pb_socket:mapred_bucket(Riak, <<"goog">>, [{map, {qfun, DailyMap}, none, false}, {reduce, {qfun, MonthReduce}, none, true}]).
{ok,[{1,
      [{"2010-02",10.099999999999909},
       {"2006-02",11.420000000000016},
       {"2004-08",8.100000000000009},
       {"2008-08",14.490000000000009},
       {"2006-05",11.829999999999984},
       {"2005-10",4.539999999999964},
       {"2006-06",7.300000000000011},
       {"2008-06",9.690000000000055},
       {"2006-03",11.770000000000039},
       {"2006-12",4.880000000000052},
       {"2005-09",9.050000000000011},
       {"2008-03",15.829999999999984},
       {"2008-09",14.889999999999986},
       {"2010-04",9.149999999999977},
       {"2008-06",14.909999999999968},
       {"2008-05",13.960000000000036},
       {"2005-05",2.780000000000001},
       {"2005-07",6.680000000000007},
       {"2008-10",21.390000000000043},
       {"2009-09",4.180000000000007},
       {"2006-08",8.319999999999993},
       {"2007-08",5.990000000000009},
       {[...],...},
       {...}|...]}]}
```

#### A MapReduce Challenge

Here is a scenario involving the data you already have loaded.

MapReduce Challenge: Find the largest day for each month in terms of
dollars traded, and subsequently the largest overall day.

*Hint*: You will need at least one each of map and reduce phases.

## Streaming MapReduce

Because Riak KV distributes the map phases across the cluster to increase
data locality, you can gain access to the results of those individual
computations as they finish via streaming.  Streaming can be very
helpful when getting access to results from a high latency MapReduce job
that only contains map phases.  Streaming of results from reduce phases
isn't as useful, but if your map phases return data (keep: true), they
will be returned to the client even if the reduce phases haven't
executed. This will let you use streaming with a reduce phase to collect
the results of the map phases while the jobs are run and then get the
result to the reduce phase at the end.

### Streaming via the HTTP API

You can enable streaming with MapReduce jobs submitted to the `/mapred`
resource by adding `?chunked=true` to the url. The response will be sent
using HTTP 1.1 chunked transfer encoding with `Content-Type: multipart/mixed`.
Be aware that if you are streaming a set of serialized objects (like
JSON objects), the chunks are not guaranteed to be separated along the
same boundaries that your serialized objects are. For example, a chunk
may end in the middle of a string representing a JSON object, so you
will need to decode and parse your responses appropriately in the
client.

### Streaming via the Erlang API

You can use streaming with Erlang via the Riak KV local client or the
Erlang Protocol Buffers API.  In either case, you will provide the call
to `mapred_stream` with a `Pid` that will receive the streaming results.

For examples, see [MapReduce pbstream.erl]({{<baseurl>}}data/MapReduceExamples/pbstream.erl)


## Troubleshooting MapReduce, illustrated

The most important advice: when developing Erlang MapReduce against
Riak KV, prototype against a development environment using the Erlang
shell. The shell allows for rapid feedback and iteration; once code
needs to be deployed to a server for production use, changing it is
more time-consuming.

### Module not in path

```bash
$ curl -XPOST localhost:8098/mapred \
>   -H 'Content-Type: application/json'   \
>   -d '{"inputs":"messages","query":[{"map":{"language":"erlang","module":"mr_example","function":"get_keys"}}]}'

{"phase":0,"error":"invalid module named in PhaseSpec function:\n must be a valid module name (failed to load mr_example: nofile)"}
```

### Node in process of starting

```bash
$ curl -XPOST localhost:8098/mapred   -H 'Content-Type: application/json'     -d '{"inputs":"messages","query":[{"map":{"language":"erlang","module":"mr_example","function":"get_keys"}}]}'

<html><head><title>500 Internal Server Error</title></head><body><h1>Internal Server Error</h1>The server encountered an error while processing this request:<br><pre>{error,{error,function_clause,
              [{chashbin,itr_value,
                         [done],
                         [{file,"src/chashbin.erl"},{line,139}]},
               {chashbin,itr_next_while,2,
                         [{file,"src/chashbin.erl"},{line,183}]},
...
```

### Erlang errors

```erlang
> riakc_pb_socket:mapred_bucket(Riak, <<"goog">>, [{map, {qfun, DailyFun}, none, true}]).
{error,<<"{\"phase\":0,\"error\":\"function_clause\",\"input\":\"{ok,{r_object,<<\\\"goog\\\">>,<<\\\"2009-06-10\\\">>,[{r_content,{dic"...>>}
```

The Erlang shell truncates error messages; when using MapReduce, typically the information you need is buried more deeply within the stack.

We can get a longer error message this way:

```erlang
> {error, ErrorMsg} = riakc_pb_socket:mapred_bucket(Riak, <<"goog">>, [{map, {qfun, DailyFun}, none, true}]).
{error,<<"{\"phase\":0,\"error\":\"function_clause\",\"input\":\"{ok,{r_object,<<\\\"goog\\\">>,<<\\\"2009-06-10\\\">>,[{r_content,{dic"...>>}

> io:format("~p~n", [ErrorMsg]).
<<"{\"phase\":0,\"error\":\"function_clause\",\"input\":\"{ok,{r_object,<<\\\"goog\\\">>,<<\\\"2009-06-10\\\">>,[{r_content,{dict,6,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[[<<\\\"Links\\\">>]],[],[],[],[],[],[],[],[[<<\\\"content-type\\\">>,97,112,112,108,105,99,97,116,105,111,110,47,106,115,111,110],[<<\\\"X-Riak-VTag\\\">>,55,87,101,79,53,120,65,121,50,67,49,77,72,104,54,100,89,65,67,74,55,70]],[[<<\\\"index\\\">>]],[],[[<<\\\"X-Riak-Last-Modified\\\">>|{1405,709865,48668}]],[],[[<<\\\"X-Riak-Meta\\\">>]]}}},<<\\\"{\\\\\\\"Date\\\\\\\":\\\\\\\"2009-06-10\\\\\\\",\\\\\\\"Open\\\\\\\":436.23,\\\\\\\"High\\\\\\\":437.89,\\\\\\\"L...\\\">>}],...},...}\",\"type\":\"error\",\"stack\":\"[{string,substr,[\\\"2009-06-10\\\",0,7],[{file,\\\"string.erl\\\"},{line,207}]},{erl_eval,do_apply,6,[{file,\\\"erl_eval.erl\\\"},{line,573}]},{erl_eval,expr,5,[{file,\\\"erl_eval.erl\\\"},{line,364}]},{erl_eval,exprs,5,[{file,\\\"erl_eval.erl\\\"},{line,118}]},{riak_kv_mrc_map,map,3,[{file,\\\"src/riak_kv_mrc_map.erl\\\"},{line,172}]},{riak_kv_mrc_map,process,3,[{file,\\\"src/riak_kv_mrc_map.erl\\\"},{line,144}]},{riak_pipe_vnode_worker,process_input,3,[{file,\\\"src/riak_pipe_vnode_worker.erl\\\"},{line,446}]},{riak_pipe_vnode_worker,wait_for_input,...}]\"}">>
```

Still truncated, but this provides enough context to see the problem:
`string,substr,[\\\"2009-06-10\\\",0,7]`. Erlang's `string:substr`
function starts indexing strings at 1, not 0.

### Exceptional tip

When experimenting with MapReduce from the Erlang shell, it is helpful
to avoid breaking the connection to Riak KV when an exception is trapped
by the shell. Use `catch_exception`:

```erlang
> catch_exception(true).
false
```
