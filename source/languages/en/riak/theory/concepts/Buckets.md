---
title: Buckets
project: riak
version: 0.10.0+
document: appendix
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Buckets'
}
---

Buckets are used to define a virtual keyspace for storing Riak objects.
They enable you to define non-default configurations over that keyspace
concerning [[replication properties]] and [[other parameters|Buckets#configuration]].

In certain respects, buckets can be compared to tables in relational
databases or folders in filesystems, respectively. From the standpoint
of performance, buckets with default configurations are essentially
"free," while non-default configurations, defined [[using bucket types]],
will be gossiped around [[the ring|Clusters#the-ring]] using Riak's
[[cluster metadata]] subsystem.

## Configuration

Bucket configurations are defined [[using bucket types]], which enables
you to create and modify sets of configurations and apply them to as
many buckets as you wish. With bucket types, you can configure the
following bucket-level parameters, overriding the default values if you
wish.

#### allow_mult

Determines whether sibling values can be created. See [[siblings|Vector Clocks#Siblings]]. The default can be `true` or `false` depending on the context. See the documentation on [[`allow_mult` in Riak 2.0|Using Bucket Types#bucket-types-and-the-allow_mult-setting]] for more information.

#### n_val

Specifies the number of copies of each object to be stored in the cluster. See the documentation on [[replication properties]]. Default: `3`.

#### last_write_wins

Indicates if an object's timestamp will be used to decide the canonical write in the case of a conflict. See the documentation on [[vector clocks]] and on [[conflict resolution]] for more information. Default: `false`.


#### r, pr, w, dw, pw, rw, notfound_ok, basic_quorum

See the documentation on [[replication properties]] for more information on all of these properties.

#### precommit

A list of Erlang functions to be executed before writing an object. See our documentation on [[pre-commit hooks|Using Commit Hooks#pre-commit-hooks]] for more information. Default: no pre-commit hooks.

#### postcommit

`postcommit` | A list of Erlang functions to be executed after writing an object. See our documentation on [[post-commit hooks|Using Commit Hooks#post-commit-hooks]] for more information. Default: no post-commit hooks.

#### old_vclock, young_vclock, small_vclock, big_vclock

These settings enabling you to manage [[vector clock pruning|Conflict Resolution#vector-clock-pruning]].

#### backend

If you are using the [[Multi]] backend, this property enables you to determine which of Riak's available backends---[[Bitcask]], [[LevelDB]], or [[Memory]]---will be used in buckets of this type.

#### datatype

If you are using [[Riak Data Types|Using Data Types]], this setting determines [[which data type|Using Data Types#setting-up-buckets-to-use-riak-data-types]] will be used in buckets of this bucket type. Possible values: `counter`, `set`, or `map`.

#### dvv_enabled

Whether [[dotted version vectors]] will be used instead of traditional [[vector clocks]] for [[conflict resolution]]. Default: `false`.

#### chash_keyfun, linkfun

These settings involve features that have been deprecated. You will not need to adjust these values.
