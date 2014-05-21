---
title: Upgrading to 2.0
project: riak
version: 2.0.0+
document: guide
audience: intermediate
keywords: [2.0, developers]
---

Upgrading vs. fresh install

## Siblings in Riak 2.0

## Bucket Types

## When Downgrading is No Longer an Option

## Upgrading Your Configuration System

Riak 2.0 offers a replacement configuration system, based on the
[Cuttlefish](https://github.com/basho/cuttlefish) project, that both
simplifies configuration syntax and utilizes one file, `riak.conf`,
instead of two (`app.config` and `vm.args`). Full documentation of the
new system can be in the [[configuration files]] document.

If you're upgrading to Riak 2.0 from an earlier version, you have two
configuration options:

1. Manually port your configuration from the older system into the new system.
2. Keep your configuration files from the older system, which are still recognized in Riak 2.0.

If you choose the first option, make sure to consult the [[configuration files]]
documentation, as many configuration parameters have changed names,
some no longer exist, and others have been added.

If you choose the second option, Riak will automatically determine that
the older configuration system is being used. You should be aware,
however, that some settings must be set in an `advanced.config` file.
For a listing of those parameters, see our documentation on [[advanced configuration|Configuration Files#advanced-configuration]].


## Disk Usage Expectations

## Upgrading Search