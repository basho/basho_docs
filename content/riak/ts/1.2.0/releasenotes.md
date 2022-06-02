---
title: "Riak TS Release Notes"
description: "Riak TS 1.1.0 Release Notes"
menu:
  riak_ts-1.2.0:
    name: "Release Notes"
    identifier: "release_notes"
    weight: 101
    parent: "introduction"
project: "riak_ts"
project_version: "1.2.0"
toc: true
aliases:
    - /riakts/1.2.0/releasenotes/
---


# Riak TS 1.2.0 Release Notes

Released February 23, 2016.

Riak TS 1.2.0 introduces riak_shell, a shell that allows you to run SQL within Riak TS. This release also expands aggregate and arithmetic functionality to allow them to be used together, enabling deeper analysis of time series data. 


## New Features

* riak_shell is a configurable, extendable shell for Riak that allows you to run SQL commands and logging in a single shell within Riak TS. You can find more information about riak_shell [here]({{< baseurl >}}riak/ts/1.2.0/using/riakshell/).


## Changes

* Aggregate and arithmetic functions can now be used together in a single value expression. [[PR #1327](https://github.com/basho/riak_kv/pull/1327), [PR #90](https://github.com/basho/riak_ql/pull/90), and [PR #95](https://github.com/basho/riak_ql/pull/95)]
* In Riak TS 1.1, `STDDEV()` was actually an implementation of Population Standard Deviation. Since it is standard in database systems to use `STDDEV()` for implementations of Sample Standard Deviation and `STDDEV_POP()` or similar for Population Standard Deviation, `STDDEV()` has been renamed `STDDEV_POP()`. Additionally, `STDDEV_SAMP()` has been added for Sample Standard Deviation, and `STDDEV()` is now treated as Sample Standard Deviation as well. [[PR #98](https://github.com/basho/riak_ql/pull/98)]


## Bugfixes

* [[PR #95](https://github.com/basho/riak_ql/pull/95)] Semicolons are now valid statement terminators.
* [[PR #1338](https://github.com/basho/riak_kv/pull/1338)] If you divide by zero using `SELECT`, you will now receive an error. 


## Compatibility
Riak TS is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 7 (development only)
* OSX 10.8+ (development only)


## Known Issues

* For security reasons, you should change the owner of the /etc/init.d/riak file to the root user after installation has completed. See our [product advisory]({{<baseurl>}}community/productadvisories/codeinjectioninitfiles/) for more information and further instruction.
* Negation of an aggregate function returns an error. You can use negation by structuring any aggregate you'd like to negate as follows: `-1*COUNT(...)`.
* Rolling upgrades are not supported.
* AAE must be turned off.
* Riak Search is not supported.
* Multi-Datacenter Replication is not supported.