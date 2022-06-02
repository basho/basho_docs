---
title: "Incompatibility between Dotted Version Vectors and Last-Write Wins"
description: ""
menu:
  community:
    name: "Incompatiblility with DVV and Last-Write Wins"
    identifier: "incompdvvlww"
    weight: 300
    parent: "productadvisories"
toc: true
---


[ZIP file]: https://github.com/basho/basho_docs/raw/riak/2.1.1/extras/binaries/dvv_lww_buckets.beam.zip


Info | Value
:----|:-----
Date issued | July 6, 2015
Product | Riak
Affected versions | 2.0.0+, 2.1.0+

## Overview

Using dotted version vectors and `last_write_wins` causes object updates to behave unexpectedly.

## Description

In Riak 2.0.0 through Riak 2.1.1, enabling dotted version vectors and `last_write_wins` at the same time has been found to cause unexpected behavior on object updates (PUT or POST with causal context), including both passive and active anti-entropy. 

## Affected Users

This issue will affect you if:

* You are using the default bucket type, and have explicitly set both `dvv_enabled` and `last_write_wins` to 'true'. The default value for `dvv_enabled` is 'false' for the default bucket type.

OR

* You are using a custom bucket type, and have explicitly set `last_write_wins` to 'true'. The default value for `dvv_enabled` is 'true' on custom bucket types.

AND 

* You are doing _updates_ to keys. That is GET-mutate-PUT passing the vclock back to Riak. PUTs without a causal context are safe.

## Impact

The issue manifests itself as failure to read a key or keys that are affected. The issue may only show in logs. The issue may be short-lived if the affected keys are overwritten.
 
## Mitigation Strategy

In all bucket types where `last_write_wins` is being used, this issue can be avoided by setting `dvv_enabled` to 'false'. Setting `dvv_enabled=false` will enable AAE or read repair to fix those affected keys. In order to make this process as easy as possible, we have provided a pre-compiled Erlang module called dvv_lww_buckets.beam that can be used to scan for and fix affected buckets. 

1. Fetch the patch [ZIP
file].
1. Uncompress the patch ZIP file.
1. Place the beam file in your basho-patches directory (»riak_install_dir«/lib/basho-patches).
1. If the beam file is not already owned by the user that Riak is running
under, change the ownership to the user Riak is running under.
1. Execute the following commands:

```riak
riak attach
l(dvv_lww_buckets).
dvv_lww_buckets:fix_buckets().
```

>Note that this only needs to be done on one node per Riak cluster, as the beam file provides a utility to fix buckets across the cluster.


If you see any output of the form: `!! Fixing <<"foo">>: - resetting dvv_enabled=false`, then you have one or more affected buckets. The `fix_buckets` function will automatically fix the bucket properties for both untyped buckets with properties different from default properties, and also any bucket types that have these settings. If you have set these properties for your default bucket in an advanced.config setting, it will warn you that your default bucket is configured incorrectly, so you will have to edit your advanced.config and make sure that you set `dvv_enabled` to 'false'.
 
>**WARNING:** 
>
>**DO NOT set `last_write_wins=false` as AAE or Read Repair may remove all conflicting values for affected keys.**

When AAE is enabled, the next AAE run will repair any keys with the issue. Also, once the bucket properties are fixed, issuing a GET (for example, your application reading the key) will result in both a successful read and will repair that particular object.

If AAE is disabled in your cluster, infrequently accessed data affected by this issue will be in a damaged state until the next request. More information about AAE, and the risks associated with disabling it, is available at [{{< baseurl >}}riak/kv/2.1.1/learn/concepts/active-anti-entropy/]({{< baseurl >}}riak/kv/2.1.1/learn/concepts/active-anti-entropy/).

In a future release we will ensure that dotted version vectors and `last_write_wins` cannot be enabled simultaneously.

