---
title: Loading Data and Running MapReduce
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: "[[Basic HTTP Operations|Basic Riak API Operations]]"
up:   "[[The Riak Fast Track]]"
next: "[[Links and Link Walking]]"
versions: false
interest: [
"[[MapReduce]]",
"[[Erlang MapReduce|MapReduce#MapReduce via the Erlang API]]",
"<a href='http://blog.basho.com/2010/02/03/the-release-riak-0.8-and-javascript-mapreduce/'>A MapReduce Screencast</a>",
"<a href='https://github.com/basho/riak_kv/blob/master/priv/mapred_builtins.js'>Some JavaScript MapReduce Functions included with Riak</a>",
"<a href='http://research.google.com/archive/mapreduce.html'>Google's MapReduce Paper</a>"
]
---

Riak provides various ways to query your data beyond the basic key/value operations: [[Full-Text Search|Riak Search]], [[MapReduce]], [[Secondary Indexes]], and [[Link Walking|Links]].

This section will walk you through loading some sample data (that we've borrowed from Google) into Riak and then using JSON over HTTP interface with Curl to perform some MapReduce queries on that data.

## Sample Data

This Erlang script will load historical stock-price data for Google (ticker symbol "GOOG") into your existing Riak cluster so we can use it.  Paste the code below into a file called `load_data.erl` inside the `dev` directory (or download it below).

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = tl(re:split(Data, "\r?\n", [{return, binary},trim])),
    lists:foreach(fun(L) -> LS = re:split(L, ","), format_and_insert(LS) end, Lines).

format_and_insert(Line) ->
    JSON = io_lib:format("{\"Date\":\"~s\",\"Open\":~s,\"High\":~s,\"Low\":~s,\"Close\":~s,\"Volume\":~s,\"Adj. Close\":~s}", Line),
    Command = io_lib:format("curl -XPUT http://127.0.0.1:8091/riak/goog/~s -d '~s' -H 'content-type: application/json'", [hd(Line),JSON]),
    io:format("Inserting: ~s~n", [hd(Line)]),
    os:cmd(Command).
```

Make the script executable:


```bash
$ chmod +x load_data.erl
```

Download the CSV file of stock data linked below and place it in the "dev" directory where we've been working.

* [goog.csv](https://github.com/basho/basho_docs/raw/master/source/data/goog.csv) - Google historical stock data
* [load_stocks.rb](https://github.com/basho/basho_docs/raw/master/source/data/load_stocks.rb) - Alternative script in Ruby to load the data
* [load_data.erl](https://github.com/basho/basho_docs/raw/master/source/data/load_data.erl) - Erlang script to load data (as shown in snippet)

Now load the data into Riak.

```bash
$ ./load_data.erl goog.csv
```

So now we have some data in our Riak cluster. Let's put that aside for a minute and learn a bit about MapReduce, and how Riak uses it.

## MapReduce

MapReduce is a programming paradigm, popularized by Google. In Riak, MapReduce is the primary method for non-primary-key-based querying.

Riak enables you to run MapReduce jobs through both the Erlang API and the HTTP API. For this tutorial we are going to use the HTTP API.

### Why do we use MapReduce for Querying Riak?

Key-value stores like Riak generally have very little functionality beyond just storing and fetching objects. MapReduce adds the capability to perform more powerful queries over the data stored in Riak. It also fits nicely with the functional programming orientation of Riak's core code and the distributed nature of the data storage.

The main goal of MapReduce is to spread the processing of a query across many systems to take advantage of parallel processing power. This is generally done by dividing the query into several steps, dividing the dataset into several chunks, and then running those step/chunk pairs on separate physical hosts. Riak's MapReduce has an additional goal: increasing data-locality. When processing a large dataset, it's often much more efficient to take the computation to the data than it is to bring the data to the computation.

"Map" and "Reduce" are both phases in the query process. Map functions take one piece of data as input, and produce zero or more results as output. If you're familiar with "mapping over a list" in functional programming style, you're already familiar with "map" steps in a map/reduce query.

## HTTP Query Syntax

Before we run some MapReduce queries of our own on the sample data, we should review a bit about how to write the queries and how they are executed.

MapReduce queries are issued over HTTP via a *POST* to the "/mapred" resource.  The body should be "application/json" of the form:

```javascript
{"inputs":[...inputs...],"query":[...query...]}
```

Map/Reduce queries have a default timeout of 60000 milliseconds (60 seconds). The default timeout can be overridden by supplying a different value, in milliseconds, in the JSON document:

```javascript
{"inputs":[...inputs...],"query":[...query...],"timeout": 90000}
```

### Inputs

The list of input objects is given as a list of 2-element lists of the form [Bucket,Key] or 3-element lists of the form [Bucket,Key,KeyData].

You may also pass just the name of a bucket ({"inputs":"mybucket",...}), which is equivalent to passing all of the keys in that bucket as inputs (i.e. "a map/reduce across the whole bucket").  You should be aware that this triggers the somewhat expensive "list keys" operation, so you should use it sparingly.

### Query

The query is given as a list of phases, each phase being of the form {PhaseType:{...spec...}}.  Valid PhaseType values are "map", "reduce", and "link".

Every phase spec may include a "keep" field, which must have a boolean value: "true" means that the results of this phase should be included in the final result of the map/reduce, "false" means the results of this phase should be used only by the next phase. Omitting the "keep" field accepts its default value, which is "false" for all phases except the final phase (Riak assumes that you were most interested in the results of the last phase of your map/reduce query).

#### Map

Map phases must be told where to find the code for the function to execute, and what language that function is in.

Function source can be specified directly in the query by using the "source" spec field.  Function source can also be loaded from a pre-stored Riak object by providing "bucket" and "key" fields in the spec.

For example:

```javascript
{"map":{"language":"javascript","source":"function(v) { return [v]; }","keep":true}}
```

would run the Javascript function given in the spec, and include the results in the final output of the m/r query.

```javascript
{"map":{"language":"javascript","bucket":"myjs","key":"mymap","keep":false}}
```

would run the Javascript function declared in the content of the Riak object under "mymap" in the "myjs" bucket, and the results of the function would not be included in the final output of the m/r query.

```javascript
{"map":{"language":"erlang","module":"riak_kv_mapreduce","function":"map_object_value"}}
```

would run the Erlang function "riak_kv_mapreduce:map_object_value/3".

Map phases may also be passed static arguments by using the "arg" spec field.

#### Reduce

Reduce phases look exactly like map phases, but are labeled "reduce".

<div class="info">For more information on map and reduce functions please refer to the [[MapReduce|MapReduce#Phasefunctions]] section of the docs which includes a description of the arguments passed to these functions.</div>

#### Link

Link phases accept "bucket" and "tag" fields that specify which links match the link query.  The string "_" (underscore) in each field means "match all", while any other string means "match exactly this string".  If either field is left out, it is considered to be set to "_" (match all).

For example:

```javascript
{"link":{"bucket":"foo","keep":false}}
```

would follow all links pointing to objects in the "foo" bucket, regardless of their tag.

## MapReduce Screencast

With the syntax and query design fresh in your mind, take a few minutes to watch this screencast and check out Riak's MapReduce in action.

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/11328947"></div>

<p><a href="http://vimeo.com/11328947">JavaScript MapReduce in Riak</a> from <a href="http://vimeo.com/bashotech">Basho Technologies</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

Here are some of the jobs we submitted in the screencast:

<dl>
<dt>[[simple-map.json|https://github.com/basho/basho_docs/raw/master/source/data/simple-map.json]]</dt>
<dd>A simple map-only job that returns the entire data set.</dd>
<dt>[[map-high.json|https://github.com/basho/basho_docs/raw/master/source/data/map-high.json]]</dt>
<dd>A map-reduce job that returns the maximum high sell value in the first week of January.</dd>
<dt>[[map-highs-by-month.json|https://github.com/basho/basho_docs/raw/master/source/data/map-highs-by-month.json]]</dt>
<dd>A more complicated map-reduce job that collects the max high by month.</dd>
<dt>[[first-week.json|https://github.com/basho/basho_docs/raw/master/source/data/first-week.json]]</dt>
<dd>A simple map-only job that returns the values for the first week of January 2010.</dd>
</dl>

## Sample Functions

So you've seen us run some MapReduce jobs. Now it's time to try your hand at it.

Based on the sample data we loaded in the last section, here are some functions that should work for you. Take a few minutes to run them and, if you're feeling daring, modify them based on what you know about MapReduce in Riak to see if you can manipulate the results.

<div class="info"><div class="title">Submitting [[MapReduce]] queries from the shell</div>To run a query from the shell, here's the curl command to use:

<div class="code"><pre>curl -XPOST http://127.0.0.1:8091/mapred -H "Content-Type: application/json" -d @-</pre></div>

After pressing return, paste your job in, for example the one shown below in the section "Complete Job", press return again, and then `Ctrl-D` to submit it. This way of running MapReduce queries is not specific to this tutorial, but it comes in very handy to just run quick fire-and-forget queries from the command line in general. With a client library, most of the dirty work of assembling the JSON that's sent to Riak will be done for you.</div>

### Map: find the days where the high was over $600.00

*Phase Function*

```javascript
function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  if(data.High && data.High > 600.00)
    return [value.key];
  else
    return [];
}
```

*Complete Job*

```json
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg) { var data = Riak.mapValuesJson(value)[0]; if(data.High && parseFloat(data.High) > 600.00) return [value.key]; else return [];}",
                  "keep":true}}]
}
```

[sample-highs-over-600.json](https://github.com/basho/basho_docs/raw/master/source/data/sample-highs-over-600.json)

### Map: find the days where the close is lower than open

*Phase Function*

```javascript
function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  if(data.Close < data.Open)
    return [value.key];
  else
    return [];
}
```

*Complete Job*

```json
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg) { var data = Riak.mapValuesJson(value)[0]; if(data.Close < data.Open) return [value.key]; else return [];}",
                  "keep":true}}]
}
```

[sample-close-lt-open.json](https://github.com/basho/basho_docs/raw/master/source/data/sample-close-lt-open.json)

### Map and Reduce: find the maximum daily variance in price by month

*Phase functions*


```javascript
/* Map function to compute the daily variance and key it by the month */
function(value, keyData, arg){
  var data = Riak.mapValuesJson(value)[0];
  var month = value.key.split('-').slice(0,2).join('-');
  var obj = {};
  obj[month] = data.High - data.Low;
  return [ obj ];
}

/* Reduce function to find the maximum variance per month */
function(values, arg){
  return [ values.reduce(function(acc, item){
             for(var month in item){
                 if(acc[month]) { acc[month] = (acc[month] < item[month]) ? item[month] : acc[month]; }
                 else { acc[month] = item[month]; }
             }
             return acc;
            })
         ];
}
```

*Complete Job*

```json
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg){ var data = Riak.mapValuesJson(value)[0]; var month = value.key.split('-').slice(0,2).join('-'); var obj = {}; obj[month] = data.High - data.Low; return [ obj ];}"}},
         {"reduce":{"language":"javascript",
                    "source":"function(values, arg){ return [ values.reduce(function(acc, item){ for(var month in item){ if(acc[month]) { acc[month] = (acc[month] < item[month]) ? item[month] : acc[month]; } else { acc[month] = item[month]; } } return acc;  }) ];}",
                    "keep":true}}
         ]
}
```

<!-- TODO: replace with a gist link -->

[sample-max-variance-by-month.json](https://github.com/basho/basho_docs/raw/master/source/data/sample-max-variance-by-month.json)

## A MapReduce Challenge

Here is a scenario involving the data you already have loaded up. If you have a moment, try to solve it using what you've just learned about MapReduce:


<div class="note"><div class="title">MapReduce Challenge</div>Find the largest day for each month in terms of dollars traded, and subsequently the largest overall day.

*Hint*: You will need at least one each of map and reduce phases.</div>
