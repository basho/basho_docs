---
title_supertext: "Conflict Resolution:"
title: "C Sharp"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "C Sharp"
    identifier: "usage_conflict_resolution_csharp"
    weight: 103
    parent: "usage_conflict_resolution"
toc: true
aliases:
  - /riak/2.0.5/dev/using/conflict-resolution/csharp
  - /riak/kv/2.0.5/dev/using/conflict-resolution/csharp
---

For reasons explained in the [Introduction to conflict resolution]({{<baseurl>}}riak/kv/2.0.5/developing/usage/conflict-resolution), we strongly recommend adopting a conflict resolution strategy that requires applications to resolve siblings according to use-case-specific
criteria. Here, we'll provide a brief guide to conflict resolution using the
official [Riak .NET client][riak_dotnet_client].

## How the .NET Client Handles Conflict Resolution

In the Riak .NET client, every Riak object has a `siblings` property that
provides access to a list of that object's sibling values. If there are no
siblings, that property will return an empty list.

Here's an example of an object with siblings:

```csharp
var id = new RiakObjectId("siblings_allowed", "nickolodeon", "best_character");

var renObj = new RiakObject(id, "Ren", RiakConstants.ContentTypes.TextPlain);
var stimpyObj = new RiakObject(id, "Stimpy", RiakConstants.ContentTypes.TextPlain);

var renResult = client.Put(renObj);
var stimpyResult = client.Put(stimpyObj);

var getResult = client.Get(id);
RiakObject obj = getResult.Value;
Debug.WriteLine(format: "Sibling count: {0}", args: obj.Siblings.Count);
foreach (var sibling in obj.Siblings)
{
    Debug.WriteLine(
        format: "    VTag: {0}",
        args: sibling.VTag);
}
```

So what happens if the count of `obj.Siblings` is greater than 0, as in the case
above?

In order to resolve siblings, you need to either fetch, update and store a
canonical value, or choose a sibling from the `Siblings` list and store that as
the canonical value.

## Basic Conflict Resolution Example

In this example, you will ignore the contents of the `Siblings` list and will
fetch, update and store the definitive value.

```csharp
var id = new RiakObjectId("siblings_allowed", "nickolodeon", "best_character");

var renObj = new RiakObject(id, "Ren", RiakConstants.ContentTypes.TextPlain);
var stimpyObj = new RiakObject(id, "Stimpy", RiakConstants.ContentTypes.TextPlain);

var renResult = client.Put(renObj);
var stimpyResult = client.Put(stimpyObj);

var getResult = client.Get(id);
RiakObject obj = getResult.Value;
Debug.Assert(obj.Siblings.Count == 2);

// Now, modify the object's value
obj.SetObject<string>("Stimpy", RiakConstants.ContentTypes.TextPlain);

// Then, store the object which has vector clock attached
var putRslt = client.Put(obj);
CheckResult(putRslt);

obj = putRslt.Value;
// Voila, no more siblings!
Debug.Assert(obj.Siblings.Count == 0);
```

### Choosing a value from `Siblings`

This example shows a basic sibling resolution strategy in which the first
sibling is chosen as the canonical value.

```csharp
var id = new RiakObjectId("siblings_allowed", "nickolodeon", "best_character");

var renObj = new RiakObject(id, "Ren", RiakConstants.ContentTypes.TextPlain);
var stimpyObj = new RiakObject(id, "Stimpy", RiakConstants.ContentTypes.TextPlain);

var renResult = client.Put(renObj);
var stimpyResult = client.Put(stimpyObj);

var getResult = client.Get(id);
RiakObject obj = getResult.Value;
Debug.Assert(obj.Siblings.Count == 2);

// Pick the first sibling
RiakObject chosenSibling = getResult.Value.Siblings.First();

// Then, store the chosen object
var putRslt = client.Put(chosenSibling);
CheckResult(putRslt);

RiakObject updatedObject = putRslt.Value;
// Voila, no more siblings!
Debug.Assert(updatedObject.Siblings.Count == 0);
```


[riak_dotnet_client]: https://github.com/basho/riak-dotnet-client
