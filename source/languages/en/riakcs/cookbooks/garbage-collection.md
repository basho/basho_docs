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

This document describes some of the implementation details behind Riak
CS's garbage collection process. For information on configuring this
system, please see our documentation on [[configuring Riak CS]].

## Versions and Manifests

In Riak CS, any named object bears multiple **versions** that are stored
in the system at any given time. These versions are not exposed to end
users and are used only for internal purposes. Each version of the
object is accessible via an object **manifest** that includes a
[UUID](http://en.wikipedia.org/wiki/Universally_unique_identifier) for
that version.

At the system level, Riak CS attempts to have only one _active_ manifest
for a named object at any given time, although multiple active manifests
can coexist in some cases. In spite of this, **only one active object
manifest is available to users accessing Riak CS at any given time**,
which means that Riak CS users are never exposed to multiple manifests.

Garbage collection (GC) of object versions involves a variety of actions
that can be divided into two essential phases:

1. Synchronous actions that occur in the foreground while the user is
   waiting for notification of successful command completion
2. Asynchronous actions that occur in the background and are not
   directly tied to user actions

These two phases are described in more detail in the following sections.

<div class="note">
<div class="title">Note on manifest pruning</div>
A Riak CS object's manifest is updated any time a write, i.e. a `PUT` or
`DELETE` request, is issued, which means that manifest sizes can grow
significantly over time. This can lead to latency problems. Riak CS's GC
subsystem will prune these manifests. If you're experiencing
manifest-related issues, we would recommend using GC.
</div>

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
`gc.leeway_period` ago (or the `write_start_time` in cases where the
`last_block_written_time` is undefined).

If a manifest version remains in the `writing` state for greater than
`gc.leeway_period`, Riak CS assumes that that manifest version
represents a failed upload attempt. In that case, Riak CS deems it
acceptable to reap any object blocks that may have been written.
Manifest versions in the `writing` state whose `last_block_written_time`
has not exceeded the `gc.leeway_period` threshold are _not_ deemed
eligible because they could represent an object version that is still in
the process of writings its blocks.

Object deletes are more straightforward. Since no object is externally
available to the user after a delete operation, any manifest versions
in the `active` or `writing` state are eligible to be cleaned up. In
this case, there is no concern about reaping the object version that is
currently being written to become the next `active` version.

{{#1.5.0-}}

Once the states of the eligible manifests have been updated to
`pending_delete`, the manifest information for any `pending_delete`
manifest versions are collected into a CRDT set and the set is written
as a value to the `riak-cs-gc` bucket keyed by a time value
representing the current epoch time plus the leeway interval, i.e.
the `gc.leeway_period` configuration option. If that write is
successful then the state for each manifest in the set is updated to
`scheduled_delete`. This indicates that the blocks of the object have
been scheduled for deletion by the garbage collection daemon and
avoids other manifest resolution processes for the object from
scheduling unnecessary deletions.
{{/1.5.0-}}
{{#1.5.0+}}

Once the states of the eligible manifests have been updated to
`pending_delete` the manifest information for any `pending_delete`
manifest versions are collected into a CRDT set and the set is written
as a value to the `riak-cs-gc` bucket keyed by a time value
representing the current epoch time. If that write is
successful then the state for each manifest in the set is updated to
`scheduled_delete`. This indicates that the blocks of the object have
been scheduled for deletion by the garbage collection daemon and
avoids other manifest resolution processes for the object from
scheduling unnecessary deletions.

The use of the current epoch time as the basis for the keys in the
`riak-cs-gc` bucket is a change from previous versions of Riak
CS. Previously the current epoch time the value of `gc.leeway_period`
was used. This change means that the `gc.leeway_period` interval is
enforced by the garbage collection daemon process and not during the
synchronous portion of the garbage collection process. The benefit of
this is that the `gc.leeway_period` interval may be changed for objects
that have already been deleted or overwritten and allows system
operators to potentially reap objects sooner than originally specified
`gc.leeway_period` interval if it is necessary.
{{/1.5.0+}}

Once the manifest enters the `scheduled_delete` state it remains as a
tombstone for a minimum of `gc.leeway_period`.

After these actions have been attempted, the synchronous portion of the
garbage collection process is concluded and a response is returned to
the user who issued the request.

## Garbage Collection Daemon

{{#1.5.0-}}

The asynchronous portion of the garbage collection process is
orchestrated by the garbage collection daemon that wakes up at
specific intervals and checks the `riak-cs-gc` bucket for any
scheduled entries that are eligible for reaping. The daemon enters a
*working* state and begins to delete the object blocks associated with
the eligible keys and continues until all keys have been processed.
The duration of this working state varies depending on the the number of
keys involved and the size of the objects they represent. The daemon
checks for messages after processing each object so that the work
interval may be manually interrupted if needed.

Deletion eligibility is determined using the key values in the
`riak-cs-gc` bucket. The keys in the `riak-cs-gc` bucket are
representations of epoch time values with random suffixes
appended. The purpose of the random suffix is to avoid hot keys when
the system is dealing with high volumes of deletes or overwrites. If
the current time according to the daemon is later than the time
represented by a key plus the leeway interval then the blocks for any
object manifests stored in that key are eligible for deletion and the
daemon attempts to delete them.

The daemon gathers the eligible keys for deletion by performing a
secondary index range query on the `$key` index with a lower bound of
time 0 and an upper bound of the current time. This allows the
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
process: concurrency can be added in order to use multiple GC worker
processes to operate on different groups of keys from the GC bucket or
it may be added to have multiple workers process the deletion of data
blocks associated with a particular manifest. The latter is discussed
more in the [[Object Block Reaping|Garbage
Collection#Object-Block-Reaping]] section below.

Once all of the object blocks represented by a key in the `riak-cs-gc`
bucket have been deleted, the key is deleted from the `riak-cs-gc`
bucket.
{{/1.5.0-}}
{{#1.5.0+}}

The asynchronous portion of the garbage collection process is
orchestrated by the garbage collection daemon that wakes up at specific
intervals and checks the `riak-cs-gc` bucket for any scheduled entries
that are eligible for reaping.

The daemon gathers the eligible keys for deletion by performing a
secondary index range query on the `$key` index with a lower bound of
time *0* and an upper bound of the current time. This allows the
daemon to collect all the keys that are eligible for deletion and have
some way of accounting for clock skew.

The daemon may also be configured to use more efficient paginated
index queries to gather the deletion-eligible keys by setting the
`gc_paginated_indexes` configuration option to `true`. In this case the gc
daemon requests up to `gc_batch_size` keys from the GC bucket and
deletes the manifests associated with those keys before requesting the
next set of keys.

The initial query performed by the garbage collection daemon may
return a subset of the eligible records if `gc_paginated_indexes` is
`true` or all eligible records otherwise.

The daemon starts up a worker process to carry out the actual reaping
of the records and passes it the batch of keys from the query of the
`riak-cs-gc` bucket. The value for each key received by the worker
process is a set containing one or more object manifests that must be
reaped.  The worker process removes the objects represented by each
object manifest in the set and then notifies the garbage collection
daemon that it has completed the task and is available for more work.

Meanwhile, the daemon repeats the process of querying the `riak-cs-gc`
bucket for more eligible records to delete and feeding the resulting
keys to worker processes until either the maximum number of worker
processes is reached (`gc.max_workers`) or there are no remaining
records eligible for removal.

Deletion eligibility is determined using the key values in the
`riak-cs-gc` bucket. The keys in the `riak-cs-gc` bucket are
representations of epoch time values with random suffixes
appended. The purpose of the random suffix is to avoid hot keys when
the system is dealing with high volumes of deletes or overwrites. If
the current time according to the daemon minus the leeway interval is
later than the time represented by a key then the blocks for any
object manifests stored at that key are eligible for deletion and the
daemon passes them off to a worker process that attempts to delete
them.

There are two levels of concurrency within the garbage collection
process. The first is the use of worker processes by the garbage
collection daemon to allow different groups of eligible records from
the garbage collection bucket to be processed independently.  The
second is that multiple workers processes can be employed in the
deletion of data blocks associated with a single object. The latter is
discussed more in the *Object Block Reaping* section below.

Once all of the objects represented by manifests stored for a
particular key in the `riak-cs-gc` bucket have been deleted, the key
is deleted from the `riak-cs-gc` bucket.
{{/1.5.0+}}

### One Daemon per Cluster

We recommend using only _one_ active garbage collection daemon in any
Riak CS cluster. If multiple daemons are currently being used, you can
disable the others by setting the `gc.interval` parameter to `infinity`
on those nodes. More information on how to do that can be found in the
[[CS configuration doc|Configuring Riak
CS#Garbage-Collection-Settings]].

## Controlling the GC Daemon

The garbage collection daemon may be queried and manipulated using the
`riak-cs-gc` script. The script is installed to the `bin` or `sbin`
directory (depending on OS) along with the primary `riak-cs` script.
The available commands that can be used with the `riak-cs-gc` script are
listed below. Running the script with no command provided displays a
list of the available commands.

Command | Description
:-------|:-----------
`batch` | Manually start garbage collection for a batch of eligible objects.{{#1.5.0+}} This command takes an optional argument to indicate a leeway time other than the currently configured `gc.leeway_period` time for the batch.{{/1.5.0+}}
`status` | Get the current status of the garbage collection daemon. The output is dependent on the current state of the daemon.
`pause` | Pause the current batch of object garbage collection. It has no effect if there is no active batch.
`resume` | Resume a paused garbage collection batch. It has no effect if there is no previously paused batch.
`set-interval` | Set or update the garbage collection interval. This setting uses a unit of seconds.
`set-leeway` | Set or update the garbage collection leeway time. This setting indicates how many seconds must elapse after an object is deleted or overwritten before the garbage collection system may reap the object. This setting uses a unit of seconds.{{1.5.0+}}

For more information, see our documentation on [[Riak CS command-line
tools]].

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

1. A **slow** reader may have blocks GC'd as it is reading an object if
   the read exceeds the leeway interval.
2. There is some reliance on system clocks and this could lead to object
   blocks being deleted earlier or later than their intended eligibility
   window dictates due to clock skew.
3. A network partition (or machine failure) lasting longer than
   `gc.leeway_period` could cause a manifest to "come back to life" and
   appear active, it would then continually serve requests whose blocks
   could not be found.

## Configuration

Riak CS's garbage collection implementation gives the deployer several
knobs to adjust for fine-tuning system performace. More information
can be found in our documentation on [[configuring Riak CS|Configuring
Riak CS#Garbage-Collection-Settings]].

## More Information

If you'd like more in-depth material on garbage collection in Riak CS,
we recommend consulting the [Riak CS
wiki](https://github.com/basho/riak_cs/wiki/Object-Chunking-and-Garbage-Collection)
