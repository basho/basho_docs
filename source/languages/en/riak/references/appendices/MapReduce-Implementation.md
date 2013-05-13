---
title: MapReduce Implementation
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [implementation, mapreduce]
---

This page details how Riak implements MapReduce, the programming paradigm popularized by [[Google|http://research.google.com/archive/mapreduce.html]]. It covers how Riak spreads processing across the cluster, the mechanics of how queries are specified and run, how to run MapReduce queries through the HTTP and Erlang APIs, streaming MapReduce, phase functions, and configuration details.

<div class="info">
<div class="title">Hands On Resources</div>
If you're new to MapReduce in Riak, check out these resources.

* [[Starting With MapReduce|MapReduce]]
* [[Loading Data and Running MapReduce Queries]] on [[The Riak Fast Track]]
* [[Riak Contributed Functions|http://contrib.basho.com]]
</div>

## How Riak Spreads Processing

When processing a large dataset, it's often much more efficient to take the computation to the data than it is to bring the data to the computation.  In practice, your MapReduce job code is likely less than 10 kilobytes, it is more efficient to send the code to the gigs of data being processed, than to stream gigabytes of data to your 10k of code.

It is Riak's solution to the data-locality problem that determines how Riak spreads the processing across the cluster.  In the same way that any Riak node can coordinate a read or write by sending requests directly to the other nodes responsible for maintaining that data, any Riak node can also coordinate a MapReduce query by sending a map-step evaluation request directly to the node responsible for maintaining the input data. Map-step results are sent back to the coordinating node, where reduce-step processing can produce a unified result.

Put more simply: Riak runs map-step functions right on the node holding the input data for those functions, and it runs reduce-step functions on the node coordinating the MapReduce query.

## How Riak's MR Queries Are Specified

MapReduce queries in Riak have two components: a list of inputs and a list of "steps", or "phases".

Each element of the input list is a bucket-key pair.  This bucket-key pair may also be annotated with "key-data", which will be passed as an argument to a map function, when evaluated on the object stored under that bucket-key pair.

Each element of the phases list is a description of a map function, a reduce function, or a link function.  The description includes where to find the code for the phase function (for map and reduce phases), static data passed to the function every time it is executed during that phase, and a flag indicating whether or not to include the results of that phase in the final output of the query.

The phase list describes the chain of operations each input will flow through.  That is, the initial inputs will be fed to the first phase in the list, and the output of that phase will be fed as input to the next phase in the list.  This stream will continue through the final phase.

## How a Map Phase Works in Riak

The input list to a map phase must be a list of (possibly annotated) bucket-key pairs.  For each pair, Riak will send the request to evaluate the map function to the partition that is responsible for storing the data for that bucket-key.  The vnode hosting that partition will lookup the object stored under that bucket-key, and evaluates the map function with the object as an argument.  The other arguments to the function will be the annotation, if any is included, with the bucket-key, and the static data for the phase, as specified in the query.

## How a Reduce Phase Works in Riak

Reduce phases accept any list of data as input, and produce any list of data as output.  They also receive a phase-static value, specified in the query definition.

The important thing to understand is that the function defining the reduce phase may be evaluated multiple times, and the input of later evaluations will include the output of earlier evaluations.

For example, a reduce phase may implement the <a href="http://en.wikipedia.org/wiki/Union_(set_theory)#Definition" target="_blank">set-union</a> function.  In that case, the first set of inputs might be `[1,2,2,3]`, and the output would be `[1,2,3]`.  When the phase receives more inputs, say `[3,4,5]`, the function will be called with the concatenation of the two lists: `[1,2,3,3,4,5]`.

Other systems refer to the second application of the reduce function as a "re-reduce".  There are at least a couple of reduce-query implementation strategies that work with Riak's model.

One strategy is to implement the phase preceding the reduce phase, such that its output is "the same shape" as the output of the reduce phase.  This is how the examples in this document are written, and the way that we have found produces cleaner code.

An alternate strategy is to make the output of a reduce phase recognizable, such that it can be extracted from the input list on subsequent applications.  For example, if inputs from the preceding phase are numbers, outputs from the reduce phase could be objects or strings.  This would allow the function to find the previous result, and apply new inputs to it.

### How a Link Phase Works in Riak

Link phases find links matching patterns specified in the query definition.  The patterns specify which buckets and tags links must have.

"Following a link" means adding it to the output list of this phase.  The output of this phase is often most useful as input to a map phase, or another reduce phase.

## MapReduce via the HTTP API

Riak supports writing MapReduce query functions in JavaScript and Erlang, as well as specifying query execution over the [[HTTP API]].


<div class="note"><div class="title">"bad encoding" error</div>If you receive an error "bad encoding" from a MapReduce query that includes phases in Javascript, verify that your data does not contain incorrect Unicode escape sequences.  Data being transferred into the Javascript VM must be in Unicode format.</div>


### HTTP Example

This example will store several chunks of text in Riak, and then compute word counts on the set of documents, using MapReduce via the HTTP API.

#### Load data

We will use the Riak HTTP interface to store the texts we want to process:


```bash
$ curl -XPUT -H "content-type: text/plain" \
    http://localhost:8098/riak/alice/p1 --data-binary @-<<\EOF
Alice was beginning to get very tired of sitting by her sister on the
bank, and of having nothing to do: once or twice she had peeped into the
book her sister was reading, but it had no pictures or conversations in
it, 'and what is the use of a book,' thought Alice 'without pictures or
conversation?'
EOF

$ curl -XPUT -H "content-type: text/plain" \
    http://localhost:8098/riak/alice/p2 --data-binary @-<<\EOF
So she was considering in her own mind (as well as she could, for the
hot day made her feel very sleepy and stupid), whether the pleasure
of making a daisy-chain would be worth the trouble of getting up and
picking the daisies, when suddenly a White Rabbit with pink eyes ran
close by her.
EOF

$ curl -XPUT -H "content-type: text/plain" \
    http://localhost:8098/riak/alice/p5 --data-binary @-<<\EOF
The rabbit-hole went straight on like a tunnel for some way, and then
dipped suddenly down, so suddenly that Alice had not a moment to think
about stopping herself before she found herself falling down a very deep
well.
EOF
```


#### Run query

With data loaded, we can now run a query:


```bash
$ curl -X POST -H "content-type: application/json" \
    http://localhost:8098/mapred --data @-<<\EOF
{"inputs":[["alice","p1"],["alice","p2"],["alice","p5"]]
,"query":[{"map":{"language":"javascript","source":"
function(v) {
  var m = v.values[0].data.toLowerCase().match(/\w*/g);
  var r = [];
  for(var i in m) {
    if(m[i] != '') {
      var o = {};
      o[m[i]] = 1;
      r.push(o);
    }
  }
  return r;
}
"}},{"reduce":{"language":"javascript","source":"
function(v) {
  var r = {};
  for(var i in v) {
    for(var w in v[i]) {
      if(w in r) r[w] += v[i][w];
      else r[w] = v[i][w];
    }
  }
  return [r];
}
"}}]}
EOF
```

And we end up with the word counts for the three documents.

```javascript
[{"the":8,"rabbit":2,"hole":1,"went":1,"straight":1,"on":2,"like":1,"a":6,"tunnel":1,"for":2,"some":1,"way":1,"and":5,"then":1,"dipped":1,"suddenly":3,"down":2,"so":2,"that":1,"alice":3,"had":3,"not":1,"moment":1,"to":3,"think":1,"about":1,"stopping":1,"herself":2,"before":1,"she":4,"found":1,"falling":1,"very":3,"deep":1,"well":2,"was":3,"considering":1,"in":2,"her":5,"own":1,"mind":1,"as":2,"could":1,"hot":1,"day":1,"made":1,"feel":1,"sleepy":1,"stupid":1,"whether":1,"pleasure":1,"of":5,"making":1,"daisy":1,"chain":1,"would":1,"be":1,"worth":1,"trouble":1,"getting":1,"up":1,"picking":1,"daisies":1,"when":1,"white":1,"with":1,"pink":1,"eyes":1,"ran":1,"close":1,"by":2,"beginning":1,"get":1,"tired":1,"sitting":1,"sister":2,"bank":1,"having":1,"nothing":1,"do":1,"once":1,"or":3,"twice":1,"peeped":1,"into":1,"book":2,"reading":1,"but":1,"it":2,"no":1,"pictures":2,"conversations":1,"what":1,"is":1,"use":1,"thought":1,"without":1,"conversation":1}]
```


#### Explanation

For more details about what each bit of syntax means, and other syntax options, read the following sections.  As a quick explanation of how this example map/reduce query worked, though:

* The objects named *p1*, *p2*, and *p5* from the `alice` bucket were given as inputs to the query.
* The map function from the phase was run on each object.  The function:

```javascript
function(v) {
  var words = v.values[0].data.toLowerCase().match('\\w*','g');
  var counts = [];
  for(var word in words)
    if (words[word] != '') {
      var count = {};
      count[words[word]] = 1;
      counts.push(count);
    }
  return counts;
}

```

creates a list of JSON objects, one for each word (non-unique) in the text.  The object has as a key, the word, and as the value for that key, the integer 1.

* The reduce function from the phase was run on the outputs of the map functions.  The function:

```javascript
function(values) {
  var result = {};
  for (var value in values) {
    for(var word in values[value]) {
      if (word in result)
        result[word] += values[value][word];
      else
        result[word] = values[value][word];
    }
  }
  return [result];
}
```

looks at each JSON object in the input list.  It steps through each key in each object, and produces a new object. That new object has a key for each key in every other object, the value of that key being the sum of the values of that key in the other objects.  It returns this new object in a list, because it may be run a second time on a list including that object and more inputs from the map phase.

* The final output is a list with one element: a JSON object with a key for each word in all of the documents (unique), with the value of that key being the number of times the word appeared in the documents.

### HTTP Query Syntax

Map/Reduce queries are issued over HTTP via a *POST* to the `/mapred` resource.  The body should be `application/json` of the form `{"inputs":[...inputs...],"query":[...query...]}`

Map/Reduce queries have a default timeout of 60000 milliseconds (60 seconds). The default timeout can be overridden by supplying a different value, in milliseconds, in the JSON document `{"inputs":[...inputs...],"query":[...query...],"timeout": 90000}`

When the timeout hits, the node coordinating the MapReduce request cancels it and returns an error to the client. When and if you are going to hit the default timeout depends on the size of the data involved and on the general load of your cluster. If you find yourself hitting the timeout regularly, consider increasing it even more or reduce the amount of data required to run the MapReduce request.

#### Inputs

The list of input objects is given as a list of 2-element lists of the form `[Bucket,Key]` or 3-element lists of the form `[Bucket,Key,KeyData]`.

You may also pass just the name of a bucket `({"inputs":"mybucket",...})`, which is equivalent to passing all of the keys in that bucket as inputs (i.e. "a map/reduce across the whole bucket").  You should be aware that this triggers the somewhat expensive "list keys" operation, so you should use it sparingly. A bucket input may also be combined with [[Key Filters]] to limit the number of objects processed by the first query phase.

If you're using Riak Search, the list of inputs can also [[reference a search query|Riak-Search---Querying#Querying-Integrated-with-Map-Reduce]] to be used as inputs.

If you've enabled Secondary Indexes, the list of inputs can also [[reference a Secondary Index query|Secondary-Indexes#Examples]].

#### Query

The query is given as a list of phases, each phase being of the form `{PhaseType:{...spec...}}`.  Valid `{PhaseType}` values are "map", "reduce", and "link".

Every phase spec may include a `keep` field, which must have a boolean value: `true` means that the results of this phase should be included in the final result of the map/reduce, `false` means the results of this phase should be used only by the next phase. Omitting the `keep` field accepts its default value, which is `false` for all phases except the final phase (Riak assumes that you were most interested in the results of the last phase of your map/reduce query).

##### Map

Map phases must be told where to find the code for the function to execute, and what language that function is in.

The function source can be specified directly in the query by using the "source" spec field.  It can also be loaded from a pre-stored riak object by providing "bucket" and "key" fields in the spec.  Or, a builtin JavaScript function can be used by providing a "name" field. Erlang map functions can be specified using the "module" and "function" fields in the spec.

<div class="info"> Riak comes with some prebuilt JavaScript functions. You can check them out at: [[https://github.com/basho/riak_kv/blob/master/priv/mapred_builtins.js|https://github.com/basho/riak_kv/blob/master/priv/mapred_builtins.js]] </div>

For example:


```javascript
{"map":{"language":"javascript","source":"function(v) { return [v]; }","keep":true}}
```

would run the JavaScript function given in the spec, and include the results in the final output of the m/r query.

```javascript
{"map":{"language":"javascript","bucket":"myjs","key":"mymap","keep":false}}
```

would run the JavaScript function declared in the content of the Riak object under *mymap* in the `myjs` bucket, and the results of the function would not be included in the final output of the m/r query.

```javascript
   {"map":{"language":"javascript","name":"Riak.mapValuesJson"}}
```

would run the builtin JavaScript function `mapValuesJson`, if you choose to store your JavaScript functions on disk. Any JS files should live in a directory defined by the `js_source_dir` field in your `app.config` file.

```javascript
{"map":{"language":"erlang","module":"riak_mapreduce","function":"map_object_value"}}
```

The above would run the Erlang function `riak_mapreduce:map_object_value/3`, whose compiled beam file should be discoverable by each Riak node process (more details can be found under [[Erlang Named Functions]]).

Map phases may also be passed static arguments by using the `arg` spec field.

For example, the following map function will perform a regex match on object values using "arg" and return how often "arg" appears in each object:

```javascript
{"map":
  {"language":"javascript",
  "source":"function(v, keyData, arg) {
    var re = RegExp(arg, \"gi\");
    var m = v.values[0].data.match(re);
    if (m == null) {
      return [{\"key\":v.key, \"count\":0}];
    } else {
      return [{\"key\":v.key, \"count\":m.length}];
    }
  }",
  "arg":"static data used in map function"}
}
```

##### Reduce

Reduce phases look exactly like map phases, but are labeled "reduce".

##### Link

Link phases accept `bucket` and `tag` fields that specify which links match the link query.  The string `_` (underscore) in each field means "match all", while any other string means "match exactly this string".  If either field is left out, it is considered to be set to `_` (match all).

The following example would follow all links pointing to objects in the `foo` bucket, regardless of their tag:

```javascript
{"link":{"bucket":"foo","keep":false}}
```

## MapReduce via the Protocol Buffers API using Erlang Syntax

Riak also supports describing MapReduce queries in Erlang syntax via the Protocol Buffers API.  This section demonstrates how to do so using the Erlang client.

<div class="note"><div class="title">Distributing Erlang MapReduce Code</div>Any modules and functions you use in your Erlang MapReduce calls must be available on all nodes in the cluster.  You can add them in Erlang applications by specifying the *-pz* option in [[vm.args|Configuration Files]] or by adding the path to the `add_paths` setting in `app.config`.</div>

### Erlang Example

Before running some MapReduce queries, let's create some objects to run them on.

```erlang
1> {ok, Client} = riakc_pb_socket:start("127.0.0.1", 8087).
2> Mine = riakc_obj:new(<<"groceries">>, <<"mine">>,
                        term_to_binary(["eggs", "bacon"])).
3> Yours = riakc_obj:new(<<"groceries">>, <<"yours">>,
                         term_to_binary(["bread", "bacon"])).
4> riakc_pb_socket:put(Client, Yours, [{w, 1}]).
5> riakc_pb_socket:put(Client, Mine, [{w, 1}]).
```

Now that we have a client and some data, let's run a query and count how many occurances of groceries.

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

<div class="note"><div class="title">Riak Object Representations</div>Note how the `riak_object` module is used in the MapReduce function, but the `riakc_obj` module is used on the client. Riak objects are represented differently internally to the cluster than they are externally.</div>

Given the lists of groceries we created, the sequence of commands above would result in L being bound to `[{"bread",1},{"eggs",1},{"bacon",2}]`.

### Erlang Query Syntax

`riakc_pb_socket:mapred/3` takes a client and two lists as arguments.  The first list contains bucket-key pairs, inputs to the MapReduce query.  The second list contains the phases of the query.

#### Inputs

The input objects are given as a list of tuples in the format `{Bucket, Key}` or `{{Bucket, Key}, KeyData}`. `Bucket` and `Key` should be binaries, and `KeyData` can be any Erlang term.  The former form is equivalent to `{{Bucket,Key},undefined}`.

#### Query

The query is given as a list of map, reduce and link phases. Map and reduce phases are each expressed as tuples in the following form:


```erlang
{Type, FunTerm, Arg, Keep}
```

*Type* is an atom, either *map* or *reduce*. *Arg* is a static argument (any Erlang term) to pass to each execution of the phase. *Keep* is either *true* or *false* and determines whether results from the phase will be included in the final value of the query.  Riak assumes the final phase will return results.

*FunTerm* is a reference to the function that the phase will execute and takes any of the following forms:

* `{modfun, Module, Function}` where *Module* and *Function* are atoms that name an Erlang function in a specific module.
* `{qfun,Fun}` where *Fun* is a callable fun term (closure or anonymous function).
* `{jsfun,Name}` where *Name* is a binary that, when evaluated in Javascript, points to a built-in Javascript function.
* `{jsanon, Source}` where *Source* is a binary that, when evaluated in Javascript is an anonymous function.
* `{jsanon, {Bucket, Key}}` where the object at `{Bucket, Key}` contains the source for an anonymous Javascript function.

<div class="info"><div class="title">qfun Note</div>
Using `qfun` can be a fragile operation. Please keep the following points in mind.

1. The module in which the function is defined must be present and **exactly the same version** on both the client and Riak nodes.

2. Any modules and functions used by this function (or any function in the resulting call stack) must also be present on the Riak nodes.

Errors about failures to ensure both 1 and 2 are often surprising, usually seen as opaque **missing-function** or **function-clause** errors. Especially in the case of differing module versions, this can be difficult to diagnose without expecting the issue and knowing of `Module:info/0`.

</div>

Link phases are expressed in the following form:


```erlang
{link, Bucket, Tag, Keep}
```


`Bucket` is either a binary name of a bucket to match, or the atom `_`, which matches any bucket. `Tag` is either a binary tag to match, or the atom `_`, which matches any tag. `Keep` has the same meaning as in map and reduce phases.


<div class="info">There is a small group of prebuilt Erlang MapReduce functions available with Riak. Check them out here: [[https://github.com/basho/riak_kv/blob/master/src/riak_kv_mapreduce.erl|https://github.com/basho/riak_kv/blob/master/src/riak_kv_mapreduce.erl]]</div>

## Streaming MapReduce

Because Riak distributes the map phases across the cluster to increase data-locality, you can gain access to the results of those individual computations as they finish via streaming.  Streaming can be very helpful when getting access to results from a high latency MapReduce job that only contains map phases.  Streaming of results from reduce phases isn't as useful, but if your map phases return data (keep: true), they will be returned to the client even if the reduce phases haven't executed.  This will let you use streaming with a reduce phase to collect the results of the map phases while the jobs are run and then get the result to the reduce phase at the end.

### Streaming via the HTTP API

You can enable streaming with MapReduce jobs submitted to the `/mapred` resource by adding `?chunked=true` to the url.  The response will be sent using HTTP 1.1 chunked transfer encoding with `Content-Type: multipart/mixed`.  Be aware that if you are streaming a set of serialized objects (like JSON objects), the chunks are not guaranteed to be separated along the same boundaries your that serialized objects are. For example, a chunk may end in the middle of a string representing a JSON object, so you will need to decode and parse your responses appropriately in the client.

### Streaming via the Erlang API

You can use streaming with Erlang via the Riak local client or the Erlang protobuffs API.  In either case, you will provide the call to `mapred_stream` with a `Pid` that will receive the streaming results.

For examples, see:

1. [MapReduce localstream.erl](/data/MapReduce-localstream.erl){{<1.3.0}}
2. [MapReduce pbstream.erl](/data/MapReduce-pbstream.erl)

## Phase functions

MapReduce phase functions have the same properties, arguments and return values whether you write them in Javascript or Erlang.

### Map phase functions

*Map functions take three arguments* (in Erlang, arity-3 is required).  Those arguments are:

  1. *Value* : the value found at a key.  This will be a Riak object, which
    in Erlang is defined and manipulated by the *riak_object* module.
    In Javascript, a Riak object looks like this:

    ```
    {
     "bucket":BucketAsString,
     "key":KeyAsString,
     "vclock":VclockAsString,
     "values":[
               {
                "metadata":{
                            "X-Riak-VTag":VtagAsString,
                            "X-Riak-Last-Modified":LastModAsString,
                            "Links":[...List of link objects],
                            ...other metadata...
                           },
                "data":ObjectData
               },
               ...other metadata/data values (siblings)...
              ]
    }
    ```
  2. *KeyData* : key data that was submitted with the inputs to the query or phase.
  3. *Arg* : a static argument for the entire phase that was submitted with the query.

*A map phase should produce a list of results.* You will see errors if the output of your map function is not a list.  Return the empty list if your map function chooses not to produce output. If your map phase is followed by another map phase, the output of the function must be compatible with the input to a map phase - a list of bucket-key pairs or `bucket-key-keydata` triples.

#### Map function examples

These map functions return the value (data) of the object being mapped:

```erlang
fun(Value, _KeyData, _Arg) ->
    [riak_object:get_value(Value)]
end.
```

```javascript
function(value, keydata, arg){
  return [value.values[0].data];
}
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

```javascript
function(value, keydata, arg){
  if(value.key.length > arg)
    return [[value.bucket, value.key]] ;
  else
    return [];
}
```




### Reduce phase functions

*Reduce functions take two arguments.* Those arguments are:

1. *ValueList*: the list of values produced by the preceding phase in the MapReduce query.
2. *Arg* : a static argument for the entire phase that was submitted with the query.

*A reduce function should produce a list of values*, but it must also be true that the function is commutative, associative, and idempotent. That is, if the input list `[a,b,c,d]` is valid for a given F, then all of the following must produce the same result:


```erlang
  F([a,b,c,d])
  F([a,d] ++ F([c,b]))
  F([F([a]),F([c]),F([b]),F([d])])
```


#### Reduce function examples

These reduce functions assume the values in the input are numbers and sum them:

```erlang
fun(ValueList, _Arg) ->
  [lists:foldl(fun erlang:'+'/2, 0, List)]
end.
```

```javascript
function(valueList, arg){
  return [valueList.reduce(
   function(acc, value){
      return acc + value;
   }, 0)];
}
```

These reduce functions sort their inputs:

```erlang
fun(ValueList, _Arg) ->
  lists:sort(ValueList)
end.
```

```javascript
function(valueList, arg){
  return valueList.sort();
}
```

## Debugging Javascript Map Reduce Phases

There are currently two facilities for debugging map reduce phases. If there was an exception in the Javascript VM you can view the error in the `log/sasl-error.log` file. In addition to viewing exceptions you can write to a specific log file from your map or reduce phases using the ejsLog function.

```javascript
ejsLog('/tmp/map_reduce.log', JSON.stringify(value))
```

Note that when used from a map phase the ejsLog function will create a file on each node on which the map phase runs. The output of a reduce phase will be located on the node you queried with your map reduce function.

## Configuration Tuning for Javascript

If you load larger JSON objects in your buckets there is a possibility you might encounter an error like the following:

```javascript
 {"lineno":465,"message":"InternalError: script stack space quota is exhausted","source":"unknown"}
```


You can increase the amount of memory allocated to the Javascript VM stack by editing your app.config. The following will increase the stack size from 8MB to 32MB:

```erlang
{js_thread_stack, 8}
```

becomes

```erlang
{js_thread_stack, 32},
```

In addition to increasing the amount of memory allocated to the stack you can increase the heap size as well by increasing the `js_max_vm_mem` from the default of 8MB. If you are collecting a large amount of results in a reduce phase you may need to increase this setting.

## Configuration for Riak 1.0

Riak 1.0 is the first release including the new MapReduce subsystem known as Riak Pipe.  By default, new Riak clusters will use Riak Pipe to power their MapReduce queries.  Existing Riak clusters that are upgraded to Riak 1.0 will continue to use the legacy MapReduce system unless the following line is added to the riak_kv section of each node's app.config:

```erlang
%% Use Riak Pipe to power MapReduce queries
{mapred_system, pipe},
```

<div class="note">Warning: Do not enable Riak Pipe for MapReduce processing until all nodes in the cluster are running Riak 1.0.</div>

Other than speed and stability of the cluster, the choice of MapReduce subsystem (Riak Pipe or legacy) should be invisible to your client.  All queries should have the same syntax and return the same results on Riak 1.0 with Riak Pipe as they did on earlier versions with the legacy subsystem.  If you should find a case where this is not true, you may revert to using the legacy subsystem by either removing the afformentioned line in your app.config or by changing it to read like this:

```erlang
%% Use the legacy MapReduce system
{mapred_system, legacy},
```

## Configuration Tuning for Reduce Phases

If you are using Riak 1.0 and the Riak Pipe subsystem for MapReduce queries, you have additional options for tuning your reduce phases.

### Batch Size

By default, Riak will evaluate a reduce function every time its phase receives 20 new inputs.  If your reduce phases would run more efficiently with more or fewer new inputs, you may change this default by adding the following to the riak_kv section of your app.config:

```erlang
%% Run reduce functions after 100 new inputs are received
{mapred_reduce_phase_batch_size, 100},
```

You may also control this batching behavior on a per-query basis by using the static argument of the phase specification.  When specifying phases over HTTP, the JSON configuration for evaluating the function after 150 new inputs looks like this:

```javascript
{"reduce":
  {...language, etc. as usual...
   "arg":{"reduce_phase_batch_size":150}}}
```

In Erlang, you may either specify a similar mochijson2 structure for the phase argument, or use the simpler proplist form:

```erlang
{reduce, FunSpec, [{reduce_phase_batch_size, 150}], Keep}
```

Finally, if you want your reduce function to be evaluated only once, after all inputs are received, use this argument instead:

```javascript
{"reduce":
  {...language, etc. as usual...
   "arg":{"reduce_phase_only_1":true}}}
```

Similarly, in Erlang:

```erlang
{reduce, FunSpec, [reduce_phase_only_1], Keep}
```

<div class="note">Warning: A known bug in Riak 1.0.0 means that it is possible a reduce function may run more often than specified if handoff happens while the phase is accumulating inputs.  This bug was fixed in 1.0.1.</div>

### Pre-Reduce

If your reduce functions can benefit from parallel execution, it is possible to request that the outputs of a preceding map phase be reduced local to the partition that produced them, before being sent, as usual, to the final aggregate reduce.

Pre-reduce is disabled by default.  To enable it for all reduce phases by default, add the following to the riak_kv section of your app.config:

```erlang
%% Always pre-reduce between map and reduce phases
{mapred_always_prereduce, true}
```

Pre-reduce may also be enabled or disabled on a per-phase basis via the Erlang API for map phases implemented in Erlang.  To enable pre-reduce, for any map phase followed by a reduce phase, pass a proplist as its static phase argument and include the following flag:

```erlang
{map, FunSpec, [do_prereduce], Keep}
```

<div class="note">Warning: A known bug in Riak 1.0.0 prevents per-phase pre-reduce from being enabled over HTTP.  This bug also prevents per-phase pre-reduce from being enabled for Javascript phases.  Use the global app.config flag for these cases. This bug was fixed in 1.0.1.</div>
