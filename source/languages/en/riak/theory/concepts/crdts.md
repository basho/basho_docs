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

Flags behave much like Boolean values, with two possible values: `enable` and `disable`. Flags cannot be used on their own, i.e. a flag cannot be stored in a bucket/key pairing. Instead, flags can only be stored within maps.

A flag has two sets: tokens and tombstones; "enable wins"; disable => copy all tokens into tombstones

### Registers

Registers are essentially named binaries. An example would be storing the name `Cassius` in the register `first_name`. Any binary value can act as the value of a register. Like flags, registers cannot be used on their own and must be embedded in Riak maps.

## Riak Datatypes Under the Hood

Applications need deterministic means of resolving conflicts; one way is to resolve those conflicts on the application side, via siblings (which is the default behavior now that `allow_mult` is set to `true` by default)

CRDTs must be idempotent, commutative, and associative

Classic example: Amazon shopping cart; which cart is deemed "true?"

Of all the possible "C"s, "convergent" is the most important; Riak's CRDTs merge automatically at write and read time, on the server and _not_ in the application.

Giving power back to the developer to both use Riak and have HA, and also **note care**; no need to code deterministic merge functions; if you _need_ deterministic merge functions of your own, then do not use Riak datatypes

Setting `allow_mult` to `true` means that you have important choices to make; but no matter what you choose, you need to have a deterministic way of resolving conflicts, depending on what your use case demands, e.g. pick the highest timestamp, union of all the values in a list, etc.

You can still---and you will always be able to---build applications with Riak that treat Riak as a highly available key/value store. That is not going away. What _is_ being provided is additional flexibility---more choices.

In versions of Riak prior to 2.0, Riak was essentially agnostic toward data types (with the exception of counters, introduced in version 1.4)

## Sets

Collections of things; it's expected that you store binaries; members of a team, tweets, friends in a social network, etc; operations like `add` or `remove`; removing is trickier than adding; you _should_ fetch the set and its context and then send the context with the remove operation (so that the set is locked while removal is happening); all operations are executed atomically at the coordinating replica; if any operation fails, then none of the operations are applied

observe-remove => you can only remove if the passed-in state/context says you can; the application is

## Maps

Enable you to compose data types into richer combinations; a map is a collection of **fields** (think of JSON); each is a `{name, DataType}` pair; if two fields with the same name but different types are added to the map, it is assumed that you wish to keep both => they're treated as two different fields; **field operations** add and remove fields (i.e. alter the _schema_ of the map); **field _update_ operations** perform operations on the fields themselves; batched operations are possible; context should be sent with batch operations that contain a Field Remove or Set Remove, no matter how deeply nested they are inside of the map; no need to update a field, as fields are created dynamically when they are needed; when running field update operations, those fields behave like their data type (counters like counters, flags like flags, etc.)

## Registers

Binary value, like a string, e.g. an email address or a first name. Client has to know how to send vclocks.

## Counters

Already contained in Riak 1.4; incrementing and decrementing by a specified value; no vclocks, no siblings; always only _one_ value; no writes are lost

Number of Twitter followers, number of Facebook likes, number of page visits

**Note**: Counters are _not_ for creating unique, ordered IDs like UUIDs; they should be taken as providing a rough estimate; think of `HINCRBY` in Redis

### Note on Atomic/Blocking Operations

Sets and maps are neither atomic nor blocking, unlike their Redis counterparts (sets and hashes, respectively); Riak CRDTs are _never_ atomic or blocking, by necessity

### Note on Bucket Types

You can set the `datatype` of buckets to `counter`, `set`, or `map`, but not to `register` or `flag`. The reasoning behind this is that storing a bunch of `registers` without context doesn't make a whole lot of sense, and neither does storing a whole bunch of `flags` without context; a `counter` indeed lacks context, but by necessity

Enables developers to delegate certain responsibilities back to Riak

Shopping cart example => previously, devs would have to resolve conflicts on the application side; CRDTs enable them to allow Riak to handle conflicts in type-specific ways

Monotonic => change is in a single direction

**Convergent** (state-based) => one replica updates, then forwards entire state; downstream merges

A -> A'; A' sends state to all current A's

**Commutative** (operation-based) => only mutations-/ops-based; needs a reliable broadcast channel

Trade-offs:
* Garbage problem => when do you get rid of things?
* Only let primaries take writes (not fallbacks)
* pw = dw = 1
* Dead actors

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

```
Map tweet {
    Counter tweetID
    Register username
    Register tweetContent
    Flag retweeted
}
```

Which HTTP data types correspond to each CRDT?

## Questions
