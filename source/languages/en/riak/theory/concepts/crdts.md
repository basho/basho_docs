---
title: Datatypes
project: riak
version: 2.0.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
---

A pure key/value store is completely agnostic toward the data stored within it. Any key can be associated with values of any conceivable type, from short strings to large JSON objects to video files. Riak began as a pure key/value store, but over time it became less agnostic toward the data stored in it through features like [[secondary indexes]], [[search capabilities|Riak Search]], and [[counters]].

In version 2.0, Riak continued this evolution by introducing a series of eventually convergent **datatypes** inspired by academic research on convergent replicated datatypes (CRDTs), most notably the work of Shapiro, Preguiça, Baquero, and Zawirski ([paper](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf)).

The difference between Riak datatypes and other data stored in Riak is that datatypes are **operations based**. Instead of the usual reads, writes, and deletes performed on key/value pairs, you instead perform operations like removing a register from a map, or telling a counter to increment itself by 5, or enabling a flag that was previously disabled (more on each of these types below).

One of the core purposes behind datatypes is to relieve developers using Riak of the burden of producing data convergence at the application level by absorbing some of that complexity into Riak itself. You can still---and you will always be able to---build applications with Riak that treat Riak as a highly available key/value store. That is not going away. What _is_ being provided is additional flexibility---more choices.

The trade-off that datatypes present is that using them takes away your ability to customize how convergence takes place. If your use case demands that you create your own deterministic merge functions, then Riak datatypes might not be a good fit.

## Riak's Datatypes

There are five Riak datatypes in total: **flags**, **registers**, **counters**, **sets**, and **maps**. Each will be described in turn in the sections below.

### Flags

Flags behave much like Boolean values, with two possible values: `enable` and `disable`. Flags cannot be used on their own, i.e. a flag cannot be stored in a bucket/key pairing by itself. Instead, flags can only be stored within maps. In general, conflicts between flags will resolve to `enable`.

#### Examples

* Whether a tweet has been retweeted
* Whether a user has signed up for a specific pricing plan

### Registers

Registers are essentially named binaries (like strings). Any binary value can act as the value of a register. Like flags, registers cannot be used on their own and must be embedded in Riak maps. In general, conflicts between registers will resolve to 

#### Examples

* Storing the name `Cassius` in the register `first_name`
* Storing the title of a blog post

### Counters

Counters are the one Riak datatype that existed prior to version 2.0. Their value is always a positive or negative integer. They are subject to two basic operations: incremement and decrement. They are useful when a fairly accurate estimate of a quantity is needed, and not reliable if you need something like unique, ordered IDs (such as UUIDs), because uniqueness cannot be guaranteed.

#### Examples

* The number of people following someone on Twitter
* The number of "likes" on a Facebook post

### Sets

Sets are basic collections of binary values (like strings). They are subject to four basic operations: add an element, remove an element, add multiple elements, or remove multiple elements. Sets can be used either on their own or embedded in a map. In general, conflicts between sets will resolve to the set with more members.

#### Examples

* The names of friends in a social network
* Items in an Amazon shopping cart (this example comes from the )

### Maps

Maps are the richest of the Riak datatypes because within them you can embed _any_ of the five datatypes, including maps themselves (you can even embed maps within maps, and maps within those maps, and so on). Operations performed directly on maps involve adding and removing datatypes, e.g. adding a register or removing a counter. The operations performed on datatypes within maps are specific to that datatype, e.g. adding an element to a set within a map, disabling a flag within a map, etc.

The following JSON-inspired pseudocode shows how a map might be structured:

```
Map tweet {
    Counter numberOfRetweets,
    Register username,
    Register tweetContent,
    Flag favorited?,
    Map userInfo
}
```

## Riak Datatypes Under the Hood

Riak datatypes are implemented as a subsystem of Riak called [`riak_dt`](https://github.com/basho/riak_dt), which runs alongside [`riak_kv`](https://github.com/basho/riak_kv).

Conflicts between replicas are inevitable in a distributed system like Riak. If a map is stored in the key `my_map`, for example, it is always possible that the value of `my_map` will be different in nodes A and B. Without using datatypes, that conflict must be resolved using timestamps, vclocks, dotted version vectors, or some other means. With datatypes, conflicts are resolved by Riak itself.

The beauty of datatypes is that Riak "knows" how to resolve these conflicts by applying datatype-specific rules. In general, Riak does this by remembering the history of a value and broadcasting that history _as part of the value_. Riak uses this history to make deterministic judgments about which value is more "true" than the other.

To give an example, think of a set stored in the key `my_set`. On one node, the set has two elements (A and B), and in another node the set has three elements (A, B, and C). In this case, Riak would choose the set with more elements, and then tell the former set: "This is the correct value. You need to have elements A, B, and C." Once this operation is complete, the set will have elements A, B, and C on both nodes.

Riak datatypes are eventually consistent because values _do_ eventually converge to a "correct" value after a series of these judgments are made. The following general rules apply to conflicts for specific datatypes:

Datatype | General rule
:--------|:------------
Flag | `enable` wins over `disable`
Register | Timestamp. Whichever value is deemed the most recent wins.
Counter | The average
Set | 
