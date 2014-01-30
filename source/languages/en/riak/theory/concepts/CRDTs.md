---
title: Datatypes
project: riak
version: 2.0.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
---

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

## Maps

Enable you to compose data types into richer combinations; a map is a collection of **fields** (think of JSON); each is a `{name, DataType}` pair; if two fields with the same name but different types are added to the map, it is assumed that you wish to keep both => they're treated as two different fields; **field operations** add and remove fields (i.e. alter the _schema_ of the map); **field _update_ operations** perform operations on the fields themselves; batched operations are possible; context should be sent with batch operations that contain a Field Remove or Set Remove, no matter how deeply nested they are inside of the map; no need to update a field, as fields are created dynamically when they are needed; when running field update operations, those fields behave like their data type (counters like counters, flags like flags, etc.)

## Registers

Binary value, like a string, e.g. an email address or a first name

## Flags

Boolean

## Counters

Already contained in Riak 1.4; incrementing and decrementing by a specified value; no vclocks, no siblings; always only _one_ value; no writes are lost

**Note**: Counters are _not_ for creating unique, ordered IDs like UUIDs; they should be taken as providing a rough estimate; think of `HINCRBY` in Redis

### Note on Atomic/Blocking Operations

Sets and maps are neither atomic nor blocking, unlike their Redis counterparts (sets and hashes, respectively); Riak CRDTs are _never_ atomic or blocking, by necessity

### Note on Bucket Types

You can set the `datatype` of buckets to `counter`, `set`, or `map`, but not to `register` or `flag`. The reasoning behind this is that storing a bunch of `registers` without context doesn't make a whole lot of sense, and neither does storing a whole bunch of `flags` without context; a `counter` indeed lacks context, but by necessity

Enables developers to delegate certain responsibilities back to Riak

Shopping cart example => previously, devs would have to resolve conflicts on the application side; CRDTs enable them to allow Riak to handle conflicts in type-specific ways

Monotonic => change is in a single direction

Setting up a bucket to store a specific data type, e.g. maps:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}''
```

**Convergent** (state-based) => one replica updates, then forwards entire state; downstream merges

A -> A'; A' sends state to all current A's

**Commutative** (operation-based) => only mutations-/ops-based; needs a reliable broadcast channel

### CRDT operations

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