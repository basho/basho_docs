---
title_supertext: "Concept"
title: "Data Types"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Data Types"
    identifier: "learn_concepts_data_types"
    weight: 104
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.9.1/theory/concepts/crdts
  - /riak/kv/2.9.1/theory/concepts/crdts
---

[crdts pdf]: http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf
[data types converg]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/crdts/#convergence
[crdts reading list]: http://christophermeiklejohn.com/crdt/2014/07/22/readings-in-crdts.html
[data types impl]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/crdts/#implementation
[concept causal context dvv]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/causal-context/#dotted-version-vectors
[concept causal context sib]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/causal-context/#siblings
[concept causal context vc]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/causal-context/#vector-clocks
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/eventual-consistency
[concept strong consistency]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/strong-consistency
[dev data types]: {{<baseurl>}}riak/kv/2.9.1/developing/data-types
[riak_dt]: https://github.com/basho/riak_dt
[dev data types context]: {{<baseurl>}}riak/kv/2.9.1/developing/data-types/#data-types-and-context
[glossary node]: {{<baseurl>}}riak/kv/2.9.1/learn/glossary/#node
[glossary vnode]: {{<baseurl>}}riak/kv/2.9.1/learn/glossary/#vnode
[usage conflict resolution]: {{<baseurl>}}riak/kv/2.9.1/developing/usage/conflict-resolution

Riak Data Types are convergent replicated data types (CRDTs), inspired by the work of [Marc Shapiro, Nuno Preguiça, Carlos Baquero, and Marek Zawirski][crdts pdf]. Riak KV supports the following eventually-convergent data types, described in later sections:

- Counters
- Flags
- HyperLogLogs
- Maps
- Registers
- Sets

The difference between Riak Data Types and typical key/value data stored in Riak KV is that Riak Data Types are operations-based from the standpoint of Riak KV clients.

Instead of the usual create, read, update, and delete (CRUD) operations
performed on key/value pairs, data types enable you to perform
operations such as removing a register from a map, telling a counter to
increment itself by 5, or enabling a flag that was previously disabled.

It's important to note that Riak Data Types are operations-based from the standpoint of connecting clients. Like CRDTs, the [convergence logic][data types converg] is state-based behind the scenes. 

Riak Data Types enable applications to use CRDTs through a simple interface, without being exposed to the complex state-based logic underneath. More on Data Types and state can be found in the section on [implementation][data types impl] below.

For more articles on CRDTs, check out this [reading list][crdts reading list].


## Counters

Counters are a bucket-level Riak data type that can be used by themselves, associated with a bucket/key pair, or used within a map. A counter’s value can only be a positive integer, negative integer, or zero.

Counters are useful when a count is needed, for example:

- Counting the number of people following someone on Twitter
- Counting the amount of likes on a Facebook post
- Counting the points scored by a player in a game 

If you require unique, ordered IDs counters should not be used because uniqueness cannot be guaranteed.

### Operations

Counters are subject to two operations: increment and decrement.


## Flags

Flags are similar to Boolean values, but instead of `true` or
`false` flags are the value `enable` or `disable`. Flags can only be stored within maps; they cannot be stored in a bucket/key on their own.

Some examples of using flags:

- Showing if a tweet has been retweeted
- Showing if a user has signed up for a specific pricing plan

### Operations

Flags support only two operations: `enable` and `disable`. Flags can be
added to or removed from a map, but those operations are performed on
the map and not on the flag directly.


## HyperLogLogs

HyperLogLogs (HLLs) are a data type used to count unique elements within a data set or stream.

For example, hyperloglogs can be used for:

- Counting the number of unique visitors to your website
- Counting the number of unique searches users performed

### Operations

HyperLogLogs support two operations: adding elements and retrieving the count.


## Maps

Maps are the most versatile of the Riak data types because all other data types can be embedded within them, _including maps themselves_. This enables the creation of complex, custom data types from a few basic building blocks.

Maps are best suited for complex, multi-faceted data. The following
JSON-inspired pseudocode shows how a tweet might be structured as a map:

```
Map tweet {
    Counter: numberOfRetweets,
    Register: username,
    Register: tweetContent,
    Flag: favorited?,
    Map: userInfo
}
```

### Operations

You can perform two types of operations on maps:

1. Operations performed directly on the map itself, which includes
   adding fields to and removing fields from the map (e.g. adding a flag
   or removing a counter).
2. Operations performed on the Data Types nested in the map, e.g.
   incrementing a counter in the map or setting a flag to `enable`.
   Those operations behave just like the operations specific to that
   Data Type.


## Registers

Registers are essentially named binaries (like strings). Any binary
value can act as the value of a register. Like flags, registers cannot
be used on their own and must be embedded in maps.

Some examples of using registers:

- Storing the name `Cassius` in the register `first_name` in a map called `user14325_info`
- Storing the title of a blog post in a map called `2010-03-01_blog_post`

### Operations

Registers can only have the binaries stored within them changed. They can be added to and removed from maps, but those operations take place on the map in which the register is nested, and not on the register itself.


## Sets

Sets are collections of unique binary values, such as strings. All of
the values in a set are unique. For example, if you attempt to add the
element `shovel` to a set that already contains `shovel`, the operation
will be ignored by Riak KV. Sets can be used either on their own or
embedded in a map.

Some examples of using sets:

- Storing the UUIDs of a user's friends in a social network application
- Storing items in an e-commerce shopping cart

### Operations

Sets are subject to four basic operations: add an element, remove an
element, add multiple elements, or remove multiple elements.


## Advantages and Disadvantages of Data Types

[Conflict resolution][usage conflict resolution] in Riak KV can be difficult because it involves reasoning about concurrency, [eventual consistency][concept eventual consistency], [siblings][concept causal context sib], and other issues that many other databases don't require you to consider.

One of the core purposes behind data types is to relieve developers
using Riak KV of the burden of producing data convergence at the
application level by absorbing a great deal of that complexity into Riak KV
itself. Riak KV manages this complexity by building eventual consistency
into the data types themselves instead of requiring clients to do so.

You can still build applications with Riak KV that treat it as a highly
available key/value store, and you will always have this choice. What
Riak Data Types provide is additional flexibility and a broader choice
palette.

The trade-off that data types necessarily present is that they don't
allow you to produce your own convergence logic. If your use case
demands that you be able to create your own deterministic merge
functions, then Riak Data Types might not be a good fit.


## Implementation

Conflicts between replicas are inevitable in a distributed system like
Riak KV.

For example, if a map is stored in the key `my_map`, it is always
possible that the value of `my_map` will be different in nodes A and B.

Without using data types, that conflict must be resolved using
timestamps, [vector clocks][concept causal context vc], [dotted version vectors][concept causal context dvv], or some other means. With data types, conflicts are resolved by Riak KV itself, using a subsystem called [`riak_dt`][riak_dt].


## Convergence

The benefit of data types is that Riak KV knows how to resolve value
conflicts by applying data type-specific rules.

Riak KV does this by remembering the history of a value and broadcasting that
history along with the current value in the form of a [context object][dev data types context] that is similar to a [vector clock][concept causal context vc] or [dotted version vectors][concept causal context dvv]. Riak KV uses the history of each data type to make deterministic judgments about which value should be deemed correct.

### Example

Imagine a set stored in the key `fruits`. On one [node][glossary node] the set `fruits` has two elements, `apple` and `orange`. While on another node the set has only one element, `apple`.

What happens when the two nodes communicate and note the divergence?

In this case Riak KV would declare the set with two elements the winner.
At that point, the node with the incorrect set would be told: "The set
`fruits` should have elements `apple` and `orange`."

In general, convergence involves the following stages:

1. Check for divergence. If the data types have the same value, Riak KV
   does nothing. But if divergence is noted...
2. Riak KV applies data type-specific merge rules, like in the `fruits`
   set example above, which will result in a "correct" value.
3. After the merge logic is applied and the correct value is determined,
   the relevant [vnodes][glossary vnode] are notified and act to
   correct the divergence.

## Convergence Rules

Convergence means that data type conflicts are weighted in a certain direction. Riak's Data Types have their own internal weights that dictate what happens in case of conflict:

Data Type | Convergence rule
:--------|:------------
Flags | `enable` wins over `disable`
Registers | The most chronologically recent value wins, based on timestamps
Counters | Implemented as a PN-Counter ([paper][crdts pdf]), so all increments and decrements by all actors are eventually applied.  Every actor wins.
Sets | If an element is concurrently added and removed, the add will win
Maps | If a field is concurrently added or updated and removed, the add/update will win

In a production Riak KV cluster being hit by lots and lots of concurrent
writes, value conflicts are inevitable. Riak Data Types are not perfect, particularly because they do not guarantee [strong consistency][concept strong consistency] and you cannot specify the rules yourself. But the
rules that dictate the convergence logic behind the Riak Data Types
were carefully chosen to minimize the potential downsides associated
with value conflicts.
