---
title: "Managing Active Anti-Entropy"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Managing Active Anti-Entropy"
    identifier: "cluster_operations_aae"
    weight: 111
    parent: "managing_cluster_operations"
toc: true
version_history:
  in: "2.9.1+"
aliases:
  - /riak/kv/2.9.8/ops/advanced/aae/
  - /riak/2.9.8/ops/advanced/aae/
---
[config search#throttledelay]: {{<baseurl>}}riak/kv/2.9.8/configuring/search/#search-anti-entropy-throttle-tier-delay
[config search#throttle]: {{<baseurl>}}riak/kv/2.9.8/configuring/search/#search-anti-entropy-throttle

Riak's [active anti-entropy](../../../learn/concepts/active-anti-entropy/) \(AAE) subsystem is a set of background processes that repair object inconsistencies stemming from missing or divergent object values across nodes. Riak operators can turn AAE on and off and configure and monitor its functioning.

In Riak versions 2.9.1 and later, [TicTac AAE]({{<baseurl>}}riak/kv/2.9.8/using/cluster-operations/tictac-active-anti-entropy/) is included with releases as an option to be used in addition to or instead of traditional AAE in Riak.

## Enabling Active Anti-Entropy

Whether AAE is currently enabled in a node is determined by the value of
the `anti_entropy` parameter in the node's [configuration files](../../../configuring/reference/).

In Riak versions 2.0 and later, AAE is turned on by default.

```riakconf
anti_entropy = active
```

```appconfig
{riak_kv, [

  {anti_entropy, {on, []}},

    %% More riak_kv settings...
]}
```

For monitoring purposes, you can also activate AAE debugging, which
provides verbose debugging message output:

```riakconf
anti_entropy = active-debug
```

```appconfig
{riak_kv, [

    %% With debugging
    {anti_entropy, {on, [debug]}},

    %% More riak_kv settings...
]}
```

Remember that you will need to [restart the node](../../admin/riak-cli/#restart) for any configuration-related changes to take effect.

## Disabling Active Anti-Entropy

Alternatively, AAE can be switched off if you would like to repair
object inconsistencies using [read repair](../../../learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy) alone:

```riakconf
anti_entropy = passive
```

```appconfig
{riak_kv, [

    %% AAE turned off
    {anti_entropy, {off, []}},

    %% More riak_kv settings...
]}
```

If you would like to reclaim the disk space used by AAE operations, you
must manually delete the directory in which AAE-related data is stored
in each node.

```bash
rm -Rf <path_to_riak_node>/data/anti_entropy/*
```

The default directory for AAE data is `./data/anti_entropy`, as in the
example above, but this can be changed. See the section below titled
**Data Directory**.

Remember that you will need to [restart the node](../../admin/riak-cli/#restart) for any configuration-related changes to take effect.

The directory deletion method above can also be used to force a
rebuilding of hash trees.

## Monitoring AAE

Riak's command-line interface includes a command that provides insight
into AAE-related processes and performance:

```bash
riak-admin aae-status
```

When you run this command in a node, the output will look like this
(shortened for the sake of brevity):

```
================================== Exchanges ==================================
Index                                              Last (ago)    All (ago)
-------------------------------------------------------------------------------
0                                                  19.0 min      20.3 min
22835963083295358096932575511191922182123945984    18.0 min      20.3 min
45671926166590716193865151022383844364247891968    17.3 min      19.8 min
68507889249886074290797726533575766546371837952    16.5 min      18.3 min
91343852333181432387730302044767688728495783936    15.8 min      17.3 min
...

================================ Entropy Trees ================================
Index                                              Built (ago)
-------------------------------------------------------------------------------
0                                                  5.7 d
22835963083295358096932575511191922182123945984    5.6 d
45671926166590716193865151022383844364247891968    5.5 d
68507889249886074290797726533575766546371837952    4.3 d
91343852333181432387730302044767688728495783936    4.8 d

================================ Keys Repaired ================================
Index                                                Last      Mean      Max
-------------------------------------------------------------------------------
0                                                     0         0         0
22835963083295358096932575511191922182123945984       0         0         0
45671926166590716193865151022383844364247891968       0         0         0
68507889249886074290797726533575766546371837952       0         0         0
91343852333181432387730302044767688728495783936       0         0         0

```

Each of these three tables contains information for each
[vnode](../../../learn/concepts/vnodes) in your cluster in these three categories:

Category | Measures | Description
:--------|:---------|:-----------
**Exchanges** | `Last` | When the most recent exchange between a data partition and one of its replicas was performed
  | `All` | How long it has been since a partition exchanged with all of its replicas
**Entropy Trees** | `Built` | When the hash trees for a given partition were created
**Keys Repaired** | `Last` | The number of keys repaired during all key exchanges since the last node restart
  | `Mean` | The mean number of keys repaired during all key exchanges since the last node restart
  | `Max` | The maximum number of keys repaired during all key exchanges since the last node restart

All AAE status information obtainable using the `riak-admin aae-status`
command is stored in-memory and is reset when a node is restarted with
the exception of hash tree build information, which is persisted on disk
(because hash trees themselves are persisted on disk).

## Configuring AAE

Riak's [configuration files](../../../configuring/reference/) enable you not just to turn AAE on and
off but also to fine-tune your cluster's use of AAE, e.g. how
much memory AAE processes should consume, how frequently specific
processes should be run, etc.

### Data Directory

By default, data related to AAE operations is stored in the
`./data/anti_entropy` directory in each Riak node. This can be changed
by setting the `anti_entropy.data_dir` parameter to a different value.

### Throttling

AAE has a built-in throttling mechanism that can insert delays between
AAE repair operations when [vnode](../../../learn/concepts/vnodes) mailboxes reach the length
specified by the [`search.anti_entropy.throttle.$tier.delay`][config search#throttledelay] parameter (more on
that in the section below). Throttling can be switched on and off using
the [`search.anti_entropy.throttle`][config search#throttle] parameter. The default is `on`.

#### Throttling Tiers

If you activate AAE throttling, you can use *tiered throttling* to
establish a series of vnode mailbox-size thresholds past which a
user-specified time delay should be observed. This enables you to
establish, for example, that a delay of 10 milliseconds should be
observed if the mailbox of any vnode reaches 50 messages.

The general form for setting tiered throttling is as follows:

```riakconf
search.anti_entropy.throttle.$tier.delay
search.anti_entropy.throttle.$tier.solrq_queue_length
```

In the above example, `$tier` should be replaced with the desired
name for that tier (e.g. `tier1`, `large_mailbox_tier`, etc). If you
choose to set throttling tiers, you will need to set the mailbox size
for one of the tiers to 0. Both the `.solrq_queue_length` and `.delay`
parameters must be set for each tier.

Below is an example configuration for three tiers, with mailbox sizes of
0, 50, and 100 and time delays of 5, 10, and 15 milliseconds,
respectively:

```riakconf
search.anti_entropy.throttle.tier1.solrq_queue_length = 0
search.anti_entropy.throttle.tier1.delay = 5ms
search.anti_entropy.throttle.tier2.solrq_queue_length = 50
search.anti_entropy.throttle.tier2.delay = 10ms
search.anti_entropy.throttle.tier3.solrq_queue_length = 100
search.anti_entropy.throttle.tier3.delay = 15ms
```

### Bloom Filters

Bloom filters are mechanisms used to prevent reads that are destined to
fail because no object exists in the location that they're querying.
Using bloom filters can improve reaction time for some queries, but
entail a small general performance cost. You can switch bloom filters
on and off using the `anti_entropy.bloomfilter` parameter.

### Trigger Interval

The `anti_entropy.trigger_interval` setting determines how often Riak's
AAE subsystem looks for work to do, e.g. building or expiring hash
trees, triggering information exchanges between nodes, etc. The default
is every 15 seconds (`15s`). Raising this value may save resources, but
at a slightly higher risk of data corruption.

### Hash Trees

As a fallback measure in addition to the normal operation of AAE on-disk
hash trees, Riak periodically clears and regenerates all hash trees
stored on disk to ensure that hash trees correspond to the key/value
data stored in Riak. This enables Riak to detect silent data corruption
resulting from disk failure or faulty hardware. The
`anti_entropy.tree.expiry` setting enables you to determine how often
that takes place. The default is once a week (`1w`). You can set up this
process to run once a day (`1d`), twice a day (`12h`), once a month
(`4w`), and so on.

In addition to specifying how often Riak expires hash trees after they
are built, you can also specify how quickly and how many hash trees are
built. You can set the frequency using the
`anti_entropy.tree.build_limit.per_timespan` parameter, for which the
default is every hour (`1h`); the number of hash tree builds is
specified by `anti_entropy.tree.build_limit.number`, for which the
default is 1.

### Write Buffer Size

While you are free to choose the backend for data storage in Riak,
background AAE processes use [LevelDB](../../../setup/planning/backend/leveldb). You can adjust the size of the
write buffer used by LevelDB for hash tree generation using the
`anti_entropy.write_buffer_size` parameter. The default is `4MB`.

### Open Files and Concurrency Limits

The `anti_entropy.concurrency_limit` parameter determines how many AAE
cross-node information exchanges or hash tree builds can happen
concurrently. The default is `2`.

The `anti_entropy.max_open_files` parameter sets an open-files limit for
AAE-related background tasks, analogous to [open files limit](../../performance/open-files-limit) settings used in operating systems. The default is `20`.

## AAE and Riak Search

Riak's AAE subsystem works to repair object inconsistencies both with
for normal key/value objects as well as data related to [Riak Search](../../../developing/usage/search). In particular, AAE acts on indexes stored in
[Solr](http://lucene.apache.org/solr/), the search platform that drives
Riak Search. Implementation details for AAE and Search can be found in
the [Search Details](../../reference/search/#active-anti-entropy-aae)
documentation.

You can check on the status of Search-related AAE using the following
command:

```bash
riak-admin search aae-status
```

The output from that command can be interpreted just like the output
discussed in the section on [monitoring](#monitoring-aae) above.




