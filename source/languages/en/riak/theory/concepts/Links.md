---
title: Links
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
---

Links are metadata that establish one-way relationships between objects
in Riak. They can be used to loosely model graph like relationships
between objects in Riak.

The Link Header
---------------

The way to read and modify links via the [[HTTP API]] is the HTTP Link
header. This header emulates the purpose of &lt;link&gt; tags in HTML,
that is, establishing relationships to other HTTP resources. The format
that Riak uses is like so:

```bash
Link: </riak/bucket/key>; riaktag="tag"
```

Inside the angle-brackets (&lt;,&gt;) is a relative URL to another object in
Riak. The "tag" portion in double-quotes is any string identifier that
has a meaning relevant to your application.

Objects can have multiple links by separating them with commas. For
example, if an object was a participant in a doubly-linked list of
objects, it might look like this:

```bash
Link: </riak/list/1>; riaktag="previous", </riak/list/3>; riaktag="next"
```

<div class="info">There is no artificial limit to the number of links an object can
have. But, as adding links to an object does increase that object’s
size, the same guidelines that apply to your data should also apply to
your links: strike a balance between size and usability.</div>

Links in the Erlang API
-----------------------

Links in the Erlang API are stored as tuples in the object metadata of
this form:

```bash
{{<<"bucket">>,<<"key">>},<<"tag">>}
```

To access the links, use "riak\_object:get\_metadata/1" to retrieve the
metadata "dict", and then retrieve the `<<"Links">>` key from that dict.
Example:

```bash
1> {ok, Object} = Client:get(<<"list">>,<<"2">>,1).
2> Meta = riak_object:get_metadata(Object).
3> Links = dict:fetch(<<"Links">>, Meta).
[{{<<"list">>,<<"1">>},<<"previous">>},{{<<"list">>,<<"3">>},<<"next">>}]
```

To store links back in the object, update the dict, update the object
metadata, and put the object:

```bash
4> NewMeta = dict:store(<<"Links">>, [{{<<"list">>,<<"0">>},<<"first">>}|Links], Meta).
5> NewObject = riak_object:update_metadata(Object, NewMeta).
6> Client:put(NewObject,2).
```

Link-walking
------------

Link-walking (traversal) is a special case of
[[MapReduce|MapReduce]] querying, and can be accessed
through the [[HTTP Link Walking]]. Link-walks start at a single input
object and follow links on that object to find other objects that match
the submitted specifications. More than one traversal may be specified
in a single request, with any number of the intermediate results
returned. The final traversal in a link-walking request always returns
results.

-   [[Link-walking by
    Example|http://basho.com/link-walking-by-example/]]
    on the Basho Blog
