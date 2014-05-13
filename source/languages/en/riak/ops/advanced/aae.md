---
title: Managing Active Anti-Entropy
project: riak
version: 1.4.8+
document: guide
audience: advanced
keywords: [operators, aae, active anti-entropy]
---

Riak's [[active anti-entropy]] (AAE) subsystem is a background process
that repairs object conflicts stemming from missing or divergent object
values across nodes. Riak operators can turn AAE on and off and
configure and monitor its functioning.

## Enabling AAE

Whether AAE is currently enabled in a node is determined by the value of
the `anti_entropy` parameter in the node's [[configuration files]]. In
Riak versions 2.0 and later, AAE is turned on by default.

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

Remember that you will need to [[restart the node|riak-admin Command Line#restart]]
for any configuration-related changes to take effect.

## Disabling AAE

Alternatively, AAE can be switched off if you would like to repair
object conflicts using [[read repair|Active Anti-Entropy#read-repair]]
alone:

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

Remember that you will need to [[restart the node|riak-admin Command Line#restart]]
for any configuration-related changes to take effect.

## Configuring AAE

Riak's [[configuration files]] enable you not just to turn AAE on and
off but also to fine-tune your cluster's use of AAE, e.g. how
much memory AAE processes should consume, how frequently specific
processes should be run, etc.

### Data Directory

By default, data related to AAE operations is stored in the `./data/anti_entropy`
directory in each Riak node. This can be changed by setting the `anti_entropy.data_dir`
parameter to a different value.

### Throttling

AAE has a built-in throttling mechanism that can insert delays between
AAE repair operations when [[vnode|Riak Glossary#vnode]] mailboxes reach
the length specified by the `anti_entropy.throttle.$tier.delay`
parameter (more on that in the section below). Throttling can be
switched on and off using the `anti_entropy.throttle` parameter. The 
default is `on`. 

#### Throttling Tiers

If you active AAE throttling, you can use **tiered throttling** to
establish a series of vnode mailbox-size thresholds past which a
user-specified time delay should be observed. This enables you to
establish, for example, that a delay of 10 milliseconds should be
observed if the mailbox of any vnode reaches 50 messages.

The general form for setting throttling tiers is as follows:

```riakconf
anti_entropy.throttle.$tier.mailbox_size
anti_entropy.throttle.$tier.delay
```

In the above example, `$tier` should be replaced with the desired
name for that tier, e.g. `tier1`, `large_mailbox_tier`, etc. If you
choose to set throttling tiers, you will need to set the mailbox size
for one of the tiers to 0. Both the `.mailbox_size` and `.delay`
parameters must be set for each tier.

Below is an example configuration for three tiers, with mailbox sizes of
0, 50, and 100 and time delays of 5, 10, and 15 milliseconds,
respectively:

```riakconf
anti_entropy.throttle.tier1.mailbox_size = 0
anti_entropy.throttle.tier1.delay = 5ms
anti_entropy.throttle.tier1.mailbox_size = 50
anti_entropy.throttle.tier1.delay = 10ms
anti_entropy.throttle.tier1.mailbox_size = 100
anti_entropy.throttle.tier1.delay = 15ms
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
at the risk of 

### Hash Trees

As a fallback measure in addition to the normal operation of AAE on-disk
hash trees, Riak periodically clears and regenerates all hash trees
stored on disk to ensure that hash trees correspond to the key/value
data stored in Riak. This enables Riak to detect silent data corruption
resulting from disk failure or faulty hardware. The `anti_entropy.tree.expiry`
setting enables you to determine how often that takes place. The default
is once a week (`1w`). You can set up this process to run once a day
(`1d`), twice a day (`12h`), once a month (`4w`), and so on.

In addition to specifying how often Riak expires hash trees after they
are built, you can also specify how quickly and how many hash trees are
built. You can set the frequency using the `anti_entropy.tree.build_limit.per_timespan`
parameter, for which the default is every hour (`1h`); the number of
hash trees

### Write Buffer Size

While you are free to choose the backend for data storage in Riak, 
background AAE processes use [[LevelDB]]. You can adjust the size of the
write buffer used by LevelDB for hash tree generation using the 
`anti_entropy.write_buffer_size` parameter. The default is `4MB`.

### Open Files and Concurrency Limits

The `anti_entropy.concurrency_limit` parameter determines how many AAE
cross-node information exchanges or hash tree builds can happen
concurrently. The default is `2`.

The `anti_entropy.max_open_files` parameter sets an open-files limit for
AAE-related background tasks, analogous to [[open files limit]] settings
used in operating systems. The default is `20`.
