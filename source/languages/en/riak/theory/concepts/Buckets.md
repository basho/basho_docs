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

Parameter | Description | Data type | Default 
:---------|:------------|:----------|:-------
`allow_mult` | Determines whether sibling values can be created. See [[siblings|Vector Clocks#Siblings]]. | Boolean | See the documentation on [[`allow_mult` in Riak 2.0|Using Bucket Types#bucket-types-and-the-allow_mult-setting]]
`n_val` | Specifies the number of copies of each object to be stored in the cluster. See the documentation on [[replication properties]]. | integer | `3`
`last_write_wins` | Indicates if an object's vector clocks will be used to decide the canonical write based on time of write in the case of a conflict. See the documentation on [[vector clocks]] and on [[conflict resolution]]. | Boolean | `false`
`r`, `pr`, `w`, `dw`, `pw`, `rw` | These values determine the number of responses for Riak required before an operation is considered successful. See the documentation on [[replication properties]] for more information. | integer or `all`, `quorum`, or `one`
`precommit` | A list of Erlang functions to be executed before writing an object. See our documentation on [[pre-commit hooks|Using Commit Hooks#pre-commit-hooks]] for more information. | list of strings | empty list
`postcommit` | A list of Erlang functions to be executed after writing an object. See our documentation on [[post-commit hooks|Using Commit Hooks#post-commit-hooks]] for more information. | list of strings | empty list

