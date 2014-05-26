---
title: Using MapReduce
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: beginner
keywords: [developers, mapreduce]
moved: {
  '1.4.0-': '/tutorials/querying/MapReduce'
}
---

## Introduction

MapReduce (M/R) is a technique for dividing work across a distributed
system. This takes advantage of the parallel processing power of
distributed systems and also reduces network bandwidth as the algorithm
is passed around to where the data lives, rather than a potentially huge
dataset transferred to a client algorithm.

Developers can use MapReduce for things like filtering documents by
tags, counting words in documents, and extracting links to related data.
In Riak, MapReduce is one method for non-key-based querying. MapReduce
jobs can be submitted through the [[HTTP API]] or the
[[Protocol Buffers API|PBC API]].

<div class="note">
<div class="title">MapReduce <em>not</em> for production use</div>
Riak MapReduce operations are very expensive computationally. Running an
expensive MapReduce job in production could have a significant
performance impact on your cluster. Because of this, we recommend
MapReduce operations only for batch processing purposes, not for real-
time querying.
</div>

## Features

* Map phases execute in parallel with data locality
* Reduce phases execute in parallel on the node where the job was submitted
* MapReduce queries written in Erlang

## When to Use MapReduce

* When you know the set of objects over which you want to MapReduce (i.e. the locations of the objects, as specified by [[bucket type|Using Bucket Types]], bucket, and key)
* When you want to return actual objects or pieces of objects and not just the keys. [[Search|Using Search]] and [[Secondary Indexes|Using Secondary Indexes]] are other means of returning objects based on non-key-based queries, but they only return lists of keys and not whole objects.
* When you need the utmost flexibility in querying your data. MapReduce gives you full access to your object and lets you pick it apart any way you want.

## When Not to Use MapReduce

* When you want to query data over an entire bucket. MapReduce uses a list of keys, which can place a lot of demand on the cluster.
* When you want latency to be as predictable as possible.

## How it Works

The MapReduce framework helps developers divide a query into steps,
divide the dataset into chunks, and then run those step/chunk pairs in
separate physical hosts.

There are two steps in a MapReduce query:

* **Map** --- The data collection phase, which breaks up large chunks of work into smaller ones and then takes action on each chunk. Map phases consist of a function and a list of objects on which the map operation will operate.
* **Reduce** --- The data collation or processing phase, which combines the results from the map step into a single output. The reduce phase is optional.

Riak MapReduce queries have two components:

* A list of inputs
* A list of phases

The elements of the input list are objects locations as specified by
[[bucket type|Using Bucket Types]], bucket, and key. The elements of the
phases list are chunks of information related to a map, a reduce, or a
link function.

A MapReduce query begins when a client makes the request to Riak. The
node that the client contacts to make the request becomes the
**coordinating node** responsible for the MapReduce job. As described
above, each job consists of a list of phases, where each phase is either
a map or a reduce phase. The coordinating node uses the list of phases
to route the object keys and the function that will operate
over the objects stored in those keys and instruct the proper vnode
to run that function over the right objects.

After running the map function, the results are sent back to the
coordinating node. This node then concatenates the list and passes that
information over to a reduce phase on the same coordinating node, 
assuming that the next phase in the list is a reduce phase.

The diagram below provides an illustration of how a coordinating vnode
orchestrates a MapReduce job.

![MapReduce Diagram](/images/MapReduce-diagram.png)

## Examples

In this example, we will create four objects with the text "caremad"
repeated a varying number of times and store those objects in the bucket
`training` (which does not bear a [[bucket type|Using Bucket Types]]).
An Erlang MapReduce function will be used to count the occurrences of
the word "caremad."

### Data object input commands

For the sake of simplicity, we'll use Riak's [curl](http://curl.haxx.se/)
in conjunction with Riak's [[HTTP API]] to store the objects:

```curl
curl -XPUT http://localhost:8098/buckets/training/keys/foo \
  -H 'Content-Type: text/plain' \
  -d 'caremad data goes here'

curl -XPUT http://localhost:8098/buckets/training/keys/bar \
  -H 'Content-Type: text/plain' \
  -d 'caremad caremad caremad caremad'

curl -XPUT http://localhost:8098/buckets/training/keys/baz \
  -H 'Content-Type: text/plain' \
  -d 'nothing to see here'

curl -XPUT http://localhost:8098/buckets/training/keys/bam \
  -H 'Content-Type: text/plain' \
  -d 'caremad caremad caremad'
```

### MapReduce script and deployment

First, the map function, which specifies that we want to get the key
for each object in the bucket `training` that contains the text
`caremad`:

```erlang
%% Need a function here
```

Here's the MapReduce job as JSON, stored in a `mapreduce.json` file:

```json
{
  "inputs":"training",
  "query": [
    {
      "map": {
        "language": "erlang",
        "source": "function(riakObject) {
          // function here
        }"
      }
    }
  ]
}
```

To run the query through the [[HTTP API]], we can `POST` that query
to the `/mapred` endpoint, specifying `application/json` as the content
type:

```curl
curl -XPOST http://localhost:8098/mapred \
  -H 'Content-Type: application/json' \
  -d @mapreduce.json
```

### Output

The output is the key for each object, followed by the count of the word 
"caremad" for that object. The output should look like this:

```
[["foo",1],["baz",0],["bar",4],["bam",3]]
```

### Recap

We ran an Erlang MapReduce function against the `training` bucket, which
takes each key/value object in the bucket and searches the text for the
word "pizza".

## Advanced MapReduce Queries

For more detailed information on MapReduce queries in Riak, we recommend
checking out our [[Advanced MapReduce]] guide.
