---
title: Garbage Collection
project: riakcs
version: 1.4.5+
document: tutorial
toc: true
index: true
audience: advanced
keywords: [garbage, gc, deletion, cleanup]
---

In Riak CS, any named object bears multiple **versions** that are
stored in the system at any given time. Each version of the object is
accessible via an object manifest that includes a
[UUID](http://en.wikipedia.org/wiki/Universally_unique_identifier) for
that version.

At the system level, Riak CS attempts to have only one _active_ manifest
for a named object at any given time, although multiple active manifests
can coexist in some cases. It is important to note, however, that **only
one active object manifest is available to users accessing Riak CS at
any given time**.

Garbage collection of object versions involves a variety of actions that
can be divided into two essential phases:

1. Synchronous actions that occur in the foreground while the user is waiting for notification of successful command completion
2. Asynchronous actions that occur in the background and are not directly tied to user actions

These two phases are described in more detail in the following sections.

## Synchronous GC Actions

Riak CS users can undertake two actions to initiate garbage collection
of an object version:

1. Overwriting the object with a new version
2. Deleting the object

When an object version is overwritten, a new object manifest is written
with the state set to `active`. This new version is then made available
to Riak CS users. When an object is explicitly deleted, however, this
means that no active versions remain and thus that the object is no
longer externally available to users.

Behind the scenes, overwriting or deleting an object also means that a
set of eligible manifest versions is determined, while the state of each
eligible manifest is changed to `pending_delete` and the
`delete_marked_time` field is set to a time value representing the
current time.

The method for compiling the list of eligible manifests is dependent
on the operation, i.e. whether the object is being overwritten or
deleted.

If the object is being overwritten, the previously `active` manifest
version is selected along with any manifest versions that are in the
`writing` state. An object is in a `writing` state if the
`last_block_written_time` field represents a time value greater than
`leeway_seconds` ago (or the `write_start_time` in cases where the
`last_block_written_time` is undefined).

If a manifest version remains in the `writing` state for greater than
`leeway_seconds` seconds, Riak CS assumes that that manifest version
represents a failed upload attempt. In that case, Riak CS deems it
acceptable to reap any object blocks that may have been written.
Manifest versions in the `writing` state whose `last_block_written_time`
has not exceeded the `leeway_seconds` threshold are _not_ deemed
eligible because they could represent an object version that is still in
the process of writings its blocks.

Object deletes are more straightforward. Since no object is externally
available to the user after a delete operation, any manifest versions
in the `active` or `writing` state are eligible to be cleaned up. In
this case, there is no concern about reaping the object version that is
currently being written to become the next `active` version.

Once the states of the eligible manifests have been updated to
`pending_delete` the manifest information for any `pending_delete`
manifest versions are collected into a CRDT set and the set is written
as a value to the `riak-cs-gc` bucket keyed by a time value
representing the current epoch time plus the leeway interval (*i.e.*
the `leeway_seconds` configuration option). If that write is
successful then the state for each manifest in the set is updated to
`scheduled_delete`. This indicates that the blocks of the object have
been scheduled for deletion by the garbage collection daemon and
avoids other manifest resolution processes for the object from
scheduling unnecessary deletions.

Once the manifest enters the `scheduled_delete` state it remains as a
tombstone for a minimum of `leeway_seconds`.

After these actions have been attempted, the synchronous portion of the
garbage collection process is concluded and a response is returned to the
user who issued the request.

## Garbage Collection Daemon

The asynchronous portion of the garbage collection process is
orchestrated by the garbage collection daemon that wakes up at
specific intervals and checks the `riak-cs-gc` bucket for any
scheduled entries that are eligible for reaping. The daemon enters a
*working* state and begins to delete the object blocks associated with
the eligible keys and continues until all keys have been
processed. The duration of this *working* state varies depending on
the number of keys involved and the size of the objects they
represent. The daemon checks for messages after processing each object
so that the work interval may be manually interrupted if needed.

Deletion eligibility is determined using the key values in the
`riak-cs-gc` bucket. The keys in the `riak-cs-gc` bucket are
representations of epoch time values with random suffixes
appended. The purpose of the random suffix is to avoid hot keys when
the system is dealing with high volumes of deletes or overwrites. If
the current time according to the daemon is later than the time
represented by a key plus the leeway interval then the blocks for any
object manifests stored at that key are eligible for deletion and the
daemon attempts to delete them.

The daemon gathers the eligible keys for deletion by performing a
secondary index range query on the `$key` index with a lower bound of
time *0* and an upper bound of the current time. This allows the
daemon to collect all the keys that are eligible for deletion and have
some way of accounting for clock skew.

The daemon may also be configured to use more efficient paginated
index queries to gather the deletion-eligible keys by setting the
`paginated_index` configuration option to `true`. In this case the gc
daemon requests up to `gc_batch_size` keys from the GC bucket and
deletes the manifests associated with those keys before requesting the
next set of keys. The value for `gc_batch_size` may be configured and
the default value is 1000.

The daemon also has two options to add concurrency to the GC
process. Concurrency can be added in order to use multiple GC worker
processes to operate on different groups of keys from the GC bucket or
it may be added to have multiple workers process the deletion of data
blocks associated with a particular manifest. The latter is discussed
more in the *Object Block Reaping* section below.

Once the object blocks represented by a key in the `riak-cs-gc` bucket
have all been deleted, the key is deleted from the `riak-cs-gc` bucket.

## Controlling the GC Daemon

The garbage collection daemon may be queried and manipulated using the
`riak-cs-gc` script. The script is installed to the `sbin` directory
along with the primary `riak-cs` script. The available commands that can
be used with the `riak-cs-gc` script are listed below. Running the script
with no command provided displays a list of the available commands.

* **batch** - Manually start garbage collection for a batch of
    eligible objects
* **status** - Get the current status of the garbage collection
    daemon. The output is dependent on the current state of the
    daemon.
* **pause** - Pause the current batch of object garbage collection. It
    has no effect if there is no active batch.
* **resume** - Resume a paused garbage collection batch. It has no
    effect if there is no previously paused batch.

## Manifest Updates

Manifest versions are retrieved and updated by the
`riak_cs_manifest_fsm` module with very few exceptions. This module
encapsulates the logic needed to retrieve the manifests, resolve any
conflicts due to siblings, and write updated manifest versions back to
Riak.

## Object Block Reaping

The actual deletion of the blocks of an object is managed by the
`riak_cs_delete_fsm` module. It starts up a number of delete workers
(based on the configured delete concurrency) and passes off object
block information to those workers who in turn carry out the actual
delete operation for that block. The delete workers are instances of
the `riak_cs_block_server` module.

Once a worker deletes a block it notifies the delete fsm and waits for
notification about another block to delete.  Once all blocks of an
object are deleted then the delete fsm starts an instance of the
manifest fsm to handle deleting the manifest version from the object
manifest data structure and if there are no remaining manifest
versions to delete the entire object manifest data structure. The goal
of this final step is to avoid the cost of scanning through empty
manifest keys that could linger indefinitely.

## Trade-offs

1. An **slow** reader may have blocks GC'd as it is reading an
object if the read exceeds the leeway interval.
1. There is some reliance on system clocks and this could lead to object
blocks being deleted earlier or later than their intended eligibility window
dictates due to clock skew.
1. A network partition (or machine failure) lasting longer than
`leeway_seconds` could cause a manifest to "come back to life"
and appear active, it would then continually serve requests whose
blocks could not be found.

## Configuration

Riak CS's garbage collection implementation gives the deployer several
knobs to adjust for fine-tuning system performace. More information
can be found in our documentation on [[configuring Riak CS|Configuring Riak CS#Garbage-Collection-Settings]].
