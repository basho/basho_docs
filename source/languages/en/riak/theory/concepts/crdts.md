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

Conflicts between replicas are inevitable in a distributed system like Riak. If, for example, a map is stored in the key `my_map`, it is always possible

The beauty of Riak datatypes is that Riak "knows" how to resolve conflicts between replicas, and it applies datatype-specific rules to resolve those conflicts. If, for example, a map is stored in the key `my_map`, 


The rules applied in a particular situation depend on which datatype is being resolved, but no matter what, it means that you don't have to.

The rules that are applied vary based on what datatype needs converging.

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

observe-remove => you can only remove if the passed-in state/context says you can; the application is

