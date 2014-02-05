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

One of the core purposes behind datatypes is to relieve developers of the burden of producing data convergence at the application level and absorbing some of that complexity into Riak itself. The trade-off is that datatypes take away your ability to customize how convergence takes place. Riak's implementation of datatypes will make a great deal of sense for many use cases, while others will need to keep hold of responsibility for data convergence.

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

#### Examples

* Complex user data in a social network application

## Riak Datatypes Under the Hood

Applications need deterministic means of resolving conflicts; one way is to resolve those conflicts on the application side, via siblings (which is the default behavior now that `allow_mult` is set to `true` by default)

CRDTs must be idempotent, commutative, and associative

Classic example: Amazon shopping cart; which cart is deemed "true?"

Of all the possible "C"s, "convergent" is the most important; Riak's CRDTs merge automatically at write and read time, on the server and _not_ in the application.

Giving power back to the developer to both use Riak and have HA, and also **note care**; no need to code deterministic merge functions; if you _need_ deterministic merge functions of your own, then do not use Riak datatypes

Setting `allow_mult` to `true` means that you have important choices to make; but no matter what you choose, you need to have a deterministic way of resolving conflicts, depending on what your use case demands, e.g. pick the highest timestamp, union of all the values in a list, etc.

You can still---and you will always be able to---build applications with Riak that treat Riak as a highly available key/value store. That is not going away. What _is_ being provided is additional flexibility---more choices.

In versions of Riak prior to 2.0, Riak was essentially agnostic toward data types (with the exception of counters, introduced in version 1.4)

## Maps

Enable you to compose data types into richer combinations; a map is a collection of **fields** (think of JSON); each is a `{name, DataType}` pair; if two fields with the same name but different types are added to the map, it is assumed that you wish to keep both => they're treated as two different fields; **field operations** add and remove fields (i.e. alter the _schema_ of the map); **field _update_ operations** perform operations on the fields themselves; batched operations are possible; context should be sent with batch operations that contain a Field Remove or Set Remove, no matter how deeply nested they are inside of the map; no need to update a field, as fields are created dynamically when they are needed; when running field update operations, those fields behave like their data type (counters like counters, flags like flags, etc.)

The following pseudocode shows 

```
Map tweet {
    Counter tweetID
    Register username
    Register tweetContent
    Flag retweeted
}
```



### Note on Atomic/Blocking Operations

Sets and maps are neither atomic nor blocking, unlike their Redis counterparts (sets and hashes, respectively); Riak CRDTs are _never_ atomic or blocking, by necessity

## How Riak Implements Datatypes

Operations performed by finite state machines that do work and send messages to other replicas (and also receive messages from other replicas)
Riak DT
`-behaviour(riak_dt).`
State based (due to lack of reliable broadcast channel)
`new/0` --- empty
`value/1` --- the resolved value
`update/3` --- mutate
`merge/2` --- converge two CRDTs
`equal/2` --- compare internal value of two CRDTs

Example: tweet

Collections of things; it's expected that you store binaries; members of a team, tweets, friends in a social network, etc; operations like `add` or `remove`; removing is trickier than adding; you _should_ fetch the set and its context and then send the context with the remove operation (so that the set is locked while removal is happening); all operations are executed atomically at the coordinating replica; if any operation fails, then none of the operations are applied

observe-remove => you can only remove if the passed-in state/context says you can; the application is

### Note on Bucket Types

You can set the `datatype` of buckets to `counter`, `set`, or `map`, but not to `register` or `flag`. The reasoning behind this is that storing a bunch of `registers` without context doesn't make a whole lot of sense, and neither does storing a whole bunch of `flags` without context; a `counter` indeed lacks context, but by necessity
