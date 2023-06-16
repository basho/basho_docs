---
title: "Riak KV 3.0.13 Release Notes"
description: ""
project: "riak_kv"
project_version: 3.0.13
menu:
  riak_kv-3.0.13:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.13/community/release-notes
  - /riak/kv/3.0.13/intro-v20
  - /riak/3.0.13/intro-v20
  - /riak/kv/3.0.13/introduction
---

Released Feb 04, 2023.

## Overview

This release is focused on improving the reliability of handoffs. The speed of handoffs is critical to the recovery times of nodes following failure, and also to the time necessary to expand or contract the cluster. Controlling the speed can be managed by increasing concurrency (using riak admin transfer-limit <limit>), but this can often lead to handoff unreliability due to timeouts.

The main handoff process has been refactored to remove some deprecated messages, simplify the naming of configuration items, improve the logging of handoffs and increase the frequency of handoff sync messages. More frequent sync messages should make the flow of handoffs from the sender more responsive to pressure at the receiver.

On joining a node to a cluster, there will now be an attempt to exchange metadata with the joining node before the join is staged. This should reduce the probability of events failing immediately after join, as bucket types had not yet been replicated to the joining node.

A new configuration option has been added to change Riak to only commit read repair on primary (not fallback) vnodes. In short-term failures, enabling this option will reduce the time taken for hinted handoff to complete following recovery of a failed node - as now the handoff will contain only objects changed for that partition during the outage. With the default setting of disabled; fallback nodes will also contain each object fetched during the outage, in-line with the behaviour in previous releases.

A fix has been implemented in leveled to reduce failures and inefficiencies when re-building the ledger key store from the object Journal, in situations where there has been a high volume of object churn.

Some helper functions have been added to `riak_client`, to simplify some operational tasks. These functions can be called from riak remote_console, e.g. `riak_client:repair_node().` - which replaces the series of commands previously required to run partition repair across all partitions on a node. `riak_client:tictacaae_suspend_node().` may be used to suspend Tictacaae AAE exchanges on a node following a failure, so that they can be re-enabled using riak_client:tictacaae_resume_node(). once handoffs have been completed.

There are still outstanding issues related to handoffs.

The release also includes a significant change to the HTTP API. In previous releases PUT, POST and DELETE requests would all GET the object prior to starting the PUT process. This is in contrast to the Protocol Buffers API which would only GET the object in case where conditions were passed in the put (e.g. if_none_match or if_not_modified). These two APIs now have the same non-functional behaviour, the HTTP API will no longer request a GET before the PUT if the request does not contain a condition (e.g. using If-None-Match, If-Match, If-Modified-Since as well as a new bespoke condition X-Riak-If-Not-Modified).

A vector clock being passed on a PUT using the X-Riak-If-Not-Modified header, will return a 409:Conflict should the passed vector clock not match the clock found prior to updating the object. This will work as the PB API if_not_modified option. This is still an eventually consistent condition, parallel updates may still lead to siblings when {allow_mult, true}.
## Previous Release Notes

Please see the KV 3.0.12 release notes [here]({{<baseurl>}}riak/kv/3.0.12/release-notes/).

