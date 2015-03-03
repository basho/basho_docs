---
title: SSL 3.0 Vulnerability and POODLE Attack
project: riak
version: 1.0.0+
versions: false
document: reference
---

Info | Value
:----|:-----
Date issued | January 27, 2015
Product | Riak and Riak CS
Affected Riak versions | 1.2.x, 1.3.x, 1.4.x, 2.0.0-2.0.2
Affected Riak CS versions | All versions up to 1.5.2

## Overview

SSL version 3 has been revealed as insecure via an attack on
[POODLE](https://www.us-cert.gov/ncas/alerts/TA14-290A). The Erlang VM
on which Riak relies supports this old version.

## Description

This fix is very narrow in scope. It instructs Erlang's SSL library to
forbid SSL version 3 traffic. Versions of Riak prior to 1.2 are also
susceptible in the limited scenarios described here, but the patch
supplied is not applicable.

## Affected Users

Users that do any of the following will be will be affected:

* expose Riak CS to untrusted networks via HTTPS
* expose Riak's optional HTTPS interface to untrusted networks
* expose Riak Control to untrusted networks

If you do not expose Riak or Riak CS to untrusted networks, we do not
recommend applying this patch, as it may lead to upgrade problems in the
future. If you are a Riak CS user, please also assess your Riak
installation against the criteria above and apply the patch if
indicated.

## Riak 2.0 Users

If you have installed Riak 2.0.5, you will not need to apply the patch,
as that version includes the fix. If you are using Riak 2.0.0 to 2.0.2,
please upgrade to 2.0.5.

## Riak CS and Riak 1.2-1.4 Users

To install the patch, perform the following on each node in your
cluster:

1. Fetch the patch [ZIP
file](https://github.com/basho/basho_docs/raw/master/source/data/poodle-1.x.zip)
1. Stop the node
1. Uncompress the patch ZIP file
1. Copy the `ssl_record.beam` file from the unzipped file to the
`basho-patches` directory. Below is a list of operating systems and the
appropriate directory for that system:
  * Debian and Ubuntu --- `/usr/lib/riak/lib/basho-patches`
  * CentOS and RHEL --- `/usr/lib64/riak/lib/basho-patches`

    For Riak CS, the relevant directories are
    `/usr/lib/riak-cs/lib/basho-patches` and
    `/usr/lib64/riak-cs/lib/basho-patches`, respectively.
1. Confirm the MD5 hash of the `ssl_record.beam` file
(**541b4a78044808b70b871a0897013b82**)
1. Start the node

To verify that the patch has been installed properly, run the `[[riak
attach|riak Command Line#attach]]` command (or `[[riak-cs attach|Riak CS
Command-line Tools#riak-cs]]`). Once you have entered the Erlang shell,
run `m(ssl_record)`. In the resulting output, the `compiled: Date:` text
should read `January 15 2015`. You can exit the shell using **Ctrl-C**.

## Backout Plan

To uninstall this patch, perform the following on each node in your
cluster:

* Stop the node
* Delete the `ssl_record.beam` file from the `basho-patches` directory
* Start the node

## Moving Forward

This patch is included in Riak 2.0.5 and all releases thereafter.
