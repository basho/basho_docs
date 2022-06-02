---
title: "Data Types"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Data Types"
    identifier: "learn_concepts_data_types"
    weight: 104
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.1.4/theory/concepts/crdts
  - /riak/kv/2.1.4/theory/concepts/crdts
---


[concept causal context dvv]: {{<baseurl>}}riak/kv/2.1.4/learn/concepts/causal-context/#dotted-version-vectors
[concept causal context sib]: {{<baseurl>}}riak/kv/2.1.4/learn/concepts/causal-context/#siblings
[concept causal context vc]: {{<baseurl>}}riak/kv/2.1.4/learn/concepts/causal-context/#vector-clocks
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.1.4/learn/concepts/eventual-consistency
[concept strong consistency]: {{<baseurl>}}riak/kv/2.1.4/learn/concepts/strong-consistency
[dev data types]: {{<baseurl>}}riak/kv/2.1.4/developing/data-types
[glossary node]: {{<baseurl>}}riak/kv/2.1.4/learn/glossary/#node
[glossary vnode]: {{<baseurl>}}riak/kv/2.1.4/learn/glossary/#vnode
[usage conflict resolution]: {{<baseurl>}}riak/kv/2.1.4/developing/usage/conflict-resolution


A pure key/value store is completely agnostic toward the data stored
within it. Any key can be associated with values of any conceivable
type, from short strings to large JSON objects to video files. Riak
began as a pure key/value store, but over time it has become more and
more aware of the data stored in it through features like [secondary
indexes]({{<baseurl>}}riak/kv/2.1.4/developing/usage/secondary-indexes/) and [Search]({{<baseurl>}}riak/kv/2.1.4/developing/usage/search/).

In version 2.0, Riak continued this evolution by introducing a series of
eventually convergent **Data Types**. Riak Data Types are convergent
replicated data types, also known as CRDTs, inspired above all by the
work of Shapiro, Preguiça, Baquero, and Zawirski
([paper](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf)). We
would also recommend [this reading
list](http://christophermeiklejohn.com/crdt/2014/07/22/readings-in-crdts.html).

## CRDTs vs. Other Riak Data

The central difference between Riak Data Types and typical key/value
data stored in Riak is that Riak Data Types are **operations based**
from the standpoint of Riak clients. Instead of the usual
CRUD---**C**reate, **R**ead, **U**pdate, and **D**elete---operations
performed on key/value pairs, Data Types enable you to perform
operations, such as removing a register from a map, telling a counter to
increment itself by 5, or enabling a flag that was previously disabled
(more on each of these types below).

It's important to note, however, that Riak Data Types are operations
based _only from the standpoint of connecting clients_. Like the CRDTs
on which they are based, the convergence logic is _state based_ behind
the scenes. In other words, Riak Data Types enable applications to use
CRDTs through a simple interface, without being exposed to the complex
state-based logic underneath. More on Data Types and state can be found
in the section on [implementation][dev data types] below.

## Advantages and Disadvantages of Data Types

[Conflict resolution][usage conflict resolution] in Riak can be difficult because it involves
reasoning about concurrency, [eventual consistency][concept eventual consistency], [siblings][concept causal context sib], and other issues that many other databases don't
require you to take into account.

One of the core purposes behind Data Types is to relieve developers
using Riak of the burden of producing data convergence at the
application level by absorbing a great deal of that complexity into Riak
itself. Riak manages this complexity by building eventual consistency
_into the Data Types themselves_ instead of requiring clients to do so.

You can still build applications with Riak that treat it as a highly
available key/value store, and you will always have this choice. What
Riak Data Types provide is additional flexibility and a broader choice
palette.

The trade-off that Data Types necessarily present is that they don't
allow you to produce your own convergence logic. If your use case
demands that you be able to create your own deterministic merge
functions, then Riak Data Types might not be a good fit.

## Riak's Five Data Types

There is a vast and ever-growing number of CRDTs. Riak currently
implements five of them: **flags**, **registers**, **counters**,
**sets**, and **maps**. Each will be described in turn in the sections
below.

### Flags

Flags behave much like Boolean values, except that instead of `true` or
`false` flags bear the value `enable` or `disable`. Flags cannot be used
on their own, i.e. a flag cannot be stored in a bucket/key by itself.
Instead, flags can only be stored within maps.

#### Operations

Flags support only two operations: `enable` and `disable`. Flags can be
added to or removed from a map, but those operations are performed on
the map and not on the flag directly.

#### Examples

* Whether a tweet has been retweeted
* Whether a user has signed up for a specific pricing plan

### Registers

Registers are essentially named binaries (like strings). Any binary
value can act as the value of a register. Like flags, registers cannot
be used on their own and must be embedded in Riak maps.

#### Operations

Registers are subject to only operation. They can only have the binaries
stored within them changed. They can be added to and removed from maps,
but those operations take place on the map in which the register is
nested, and not on the register itself.

#### Examples

* Storing the name `Cassius` in the register `first_name` in a map called `user14325_info`
* Storing the title of a blog post in a map called `2010-03-01_blog_post`

### Counters

Counters are the one Riak Data Type that existed prior to version 2.0
(introduced in version 1.4.0). Their value can only be a positive or
negative integer or zero. They are useful when a fairly accurate
estimate of a quantity is needed, and not reliable if you require
unique, ordered IDs (such as UUIDs), because uniqueness cannot be
guaranteed.

#### Operations

Counters are subject to two operations, increment and decrement,
whether they are used on their own or in a map.

#### Examples

* The number of people following someone on Twitter
* The number of "likes" on a Facebook post
* The number of points scored by a player in a role-playing game

### Sets

Sets are collections of unique binary values, such as strings. All of
the values in a set are unique. For example, if you attempt to add the
element `shovel` to a set that already contains `shovel`, the operation
will be ignored by Riak. Sets can be used either on their own or
embedded in a map.

#### Operations

They are subject to four basic operations: add an element, remove an
element, add multiple elements, or remove multiple elements.

#### Examples

* The UUIDs of a user's friends in a social network application
* The items in an e-commerce shopping cart

### Maps

Maps are the richest of the Riak Data Types because within the
**fields** of a map you can nest _any_ of the five Data Types, including
maps themselves (you can even embed maps within maps, and maps within
those maps, and so on).

#### Operations

You can perform two types of operations on maps:

1. Operations performed directly on the map itself, which includes
   adding fields to and removing fields from the map (e.g. adding a flag
   or removing a counter).
2. Operations performed on the Data Types nested in the map, e.g.
   incrementing a counter in the map or setting a flag to `enable`.
   Those operations behave just like the operations specific to that
   Data Type.

#### Examples

Maps are best suited to complex, multi-faceted data. The following
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

## Riak Data Types Under the Hood

Conflicts between replicas are inevitable in a distributed system like
Riak. If a map is stored in the key `my_map`, for example, it is always
possible that the value of `my_map` will be different in nodes A and B.
Without using Data Types, that conflict must be resolved using
timestamps, [vector clocks][concept causal context vc], [dotted version vectors][concept causal context dvv], or some other
means. With Data Types, conflicts are resolved by Riak itself, using a
subsystem called [`riak_dt`](https://github.com/basho/riak_dt).

### Data Type Convergence

The beauty of Data Types is that Riak "knows" how to resolve value
conflicts by applying Data Type-specific rules. In general, Riak does
this by remembering the **history** of a value and broadcasting that
history along with the current value in the form of a [context
object]({{<baseurl>}}riak/kv/2.1.4/developing/data-types/#Data-Types-and-Context) that is similar to a
[vector clock][concept causal context vc] or `[dotted version vectors][concept causal context dvv]. Riak uses the history of each Data Type to make deterministic
judgments about which value should be deemed correct.

#### Example

Imagine a set stored in the key `fruits`. On one [node][glossary node], the set `fruits` has two elements, `apple` and
`orange`, while on another node the set has only one element, `apple`.
What happens when the two nodes communicate and note the divergence?

In this case Riak would declare the set with two elements the winner.
At that point, the node with the incorrect set would be told: "The set
`fruits` should have elements `apple` and `orange`."

In general, convergence involves the following stages:

1. Check for divergence. If the Data Types have the same value, Riak
   does nothing. But if divergence is noted...
2. Riak applies Data Type-specific merge rules, like in the `fruits`
   set example above, which will result in a "correct" value.
3. After the merge logic is applied and the correct value is determined,
   the relevant [vnodes][glossary vnode] are notified and act to
   correct the divergence.

### Convergence Rules

Thus far, we have not yet specified which rules actually govern
convergence, with the exception of the set example above. Convergence
essentially means that Data Type conflicts are _weighted_ in a certain
direction. All five of Riak's Data Types have their own internal
weights that dictate what happens in case of conflict.

Data Type | Convergence rule
:--------|:------------
Flags | `enable` wins over `disable`
Registers | The most chronologically recent value wins, based on timestamps
Counters | Implemented as a PN-Counter ([paper](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf)), so all increments and decrements by all actors are eventually applied.  Every actor wins.
Sets | If an element is concurrently added and removed, the add will win
Maps | If a field is concurrently added or updated and removed, the add/update will win

In a production Riak cluster being hit by lots and lots of concurrent
writes, value conflicts are inevitable, and Riak Data Types are not
perfect, particularly in that they do _not_ guarantee [strong
consistency][concept strong consistency] and in that you cannot specify the rules yourself. But the
rules that dictate the convergence logic behind the five Riak Data Types
were carefully chosen to minimize the potential downsides associated
with value conflicts.
