---
title: Managing Active Anti-Entropy
project: riak
version: 1.4.8+
document: guide
audience: advanced
keywords: [operators, aae, active anti-entropy]
---

Riak's [[active anti-entropy]] (AAE) subsystem is a background process that repairs object conflicts stemming from missing or divergent object values across nodes. Riak operators can turn AAE on and off and configure and monitor its functioning.

## Enabling and Disabling AAE

Whether AAE is currently enabled in a node is determined by the value of the `anti_entropy` parameter in the node's [[configuration files]]. In Riak versions 2.0 and later, AAE is turned on by default.

```riakconf
anti_entropy = active
```

```appconfig
{riak_kv, [

	{anti_entropy, {on, []}},

    %% More riak_kv settings...
]}
```

You can also activate AAE debugging, which provides verbose debugging message output:

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

If you would like to repair object conflicts using [[read repair|Active Anti-Entropy#read-repair]] alone, the AAE can switched off:

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

Remember that you will need to then restart the node fo the changes to take effect.

## Configuring AAE

Riak's [[configuration files]] enable you not just to turn AAE on and off but also to finetune your cluster's use of AAE.

### Throttling

AAE has a built-in throttling mechanism that can insert delays between AAE repair operations when [[vnode|Riak Glossary#vnode]] mailboxes reach a specified length.

Throttling Tiers

### Bloom Filters

Bloom filters are mechanisms used to shortcut

### Trigger Interval

The `anti_entropy.trigger_interval` setting determines how often Riak's AAE subsystem looks for work to do, e.g. building or expiring hash trees, triggering information exchanges between nodes, etc. The default is every 15 seconds (`15s`). Raising this value may save resources, but at the risk of 

### Hash Tree Expiry

As fallback measure in addition to the normal operation of AAE on-disk hash trees, Riak periodically clears and regenerates all hash trees stored on disk to ensure that hash trees correspond to the key/value data stored in Riak. This enables Riak to detect silent data corruption resulting from disk failure or faulty hardware. The `anti_entropy.tree.expiry` setting enables you to determine how often that takes place. The default is once a week (`1w`). You can set up this process to run once a day (`1d`), twice a day (`12h`), once a month (`4w`), and so on.

### Concurrency Limits

The `anti_entropy.concurrency_limit` parameter determines how many AAE cross-node information exchanges or hash tree builds can happen concurrently. The default is `2`.

## Monitoring AAE



