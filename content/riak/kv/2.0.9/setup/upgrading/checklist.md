---
title: "Production Checklist"
description: ""
project: "riak_kv"
project_version: "2.0.9"
menu:
  riak_kv-2.0.9:
    name: "Production Checklist"
    identifier: "upgrading_checklist"
    weight: 100
    parent: "upgrading"
toc: true
aliases:
  - /riak/2.0.9/ops/upgrading/production-checklist/
  - /riak/kv/2.0.9/ops/upgrading/production-checklist/
---

[perf open files]: {{<baseurl>}}riak/kv/2.0.9/using/performance/open-files-limit
[perf index]: {{<baseurl>}}riak/kv/2.0.9/using/performance
[ntp]: http://www.ntp.org/
[security basics]: {{<baseurl>}}riak/kv/2.0.9/using/security/basics
[cluster ops load balance]: {{<baseurl>}}riak/kv/2.0.9/configuring/load-balancing-proxy
[config reference]: {{<baseurl>}}riak/kv/2.0.9/configuring/reference
[config backend]: {{<baseurl>}}riak/kv/2.0.9/configuring/backend
[usage search]: {{<baseurl>}}riak/kv/2.0.9/developing/usage/search
[usage conflict resolution]: {{<baseurl>}}riak/kv/2.0.9/developing/usage/conflict-resolution
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.0.9/learn/concepts/eventual-consistency
[apps replication properties]: {{<baseurl>}}riak/kv/2.0.9/developing/app-guide/replication-properties
[concept strong consistency]: {{<baseurl>}}riak/kv/2.0.9/using/reference/strong-consistency
[cluster ops bucket types]: {{<baseurl>}}riak/kv/2.0.9/using/cluster-operations/bucket-types
[use admin commands]: {{<baseurl>}}riak/kv/2.0.9/using/admin/commands
[use admin riak control]: {{<baseurl>}}riak/kv/2.0.9/using/admin/riak-control
[cluster ops inspect node]: {{<baseurl>}}riak/kv/2.0.9/using/cluster-operations/inspecting-node
[troubleshoot http]: {{<baseurl>}}riak/kv/2.0.9/using/troubleshooting/http-204
[use admin riak-admin]: {{<baseurl>}}riak/kv/2.0.9/using/admin/riak-admin
[SANs]: http://en.wikipedia.org/wiki/Storage_area_network

Deploying Riak KV to a realtime production environment from a development or testing environment can be a complex process. While the specifics of that process will always depend on your environment and practices, there are some basics for you to consider and a few questions that you will want to ask while making this transition.

We've compiled these considerations and questions into separate categories for you to look over.

## System

* Are all systems in your cluster as close to identical as possible in
  terms of both hardware and software?
* Have you set appropriate [open files limits][perf open files] on all
  of your systems?
* Have you applied the [Riak KV performance improvement recommendations][perf index]?

## Network

* Are all systems using the same [NTP servers][ntp] to
  synchronize clocks?
* Are you sure that your NTP clients' configuration is monotonic (i.e.
  that your clocks will not roll back)?
* Is DNS correctly configured for all systems' production deployments?
* Are connections correctly routed between all Riak nodes?
* Are connections correctly set up in your load balancer?
* Are your [firewalls][security basics] correctly configured?
* Check that network latency and throughput are as expected for all of the
  following (we suggest using [iperf][ntp] to verify):
  - between nodes in the cluster
  - between the load balancer and all nodes in the cluster
  - between application servers and the load balancer
* Do all Riak nodes appear in the load balancer's rotation?
* Is the load balancer configured to balance connections with roundrobin
  or a similarly random [distribution scheme][cluster ops load balance]?

## Riak KV

* Check [configuration files][config reference]:
  - Does each machine have the correct name and IP settings in
    `riak.conf` (or in `app.config` if you're using the older
    configuration files)?
  - Are all [configurable settings][config reference] identical
    across the cluster?
  - Have all of the settings in your configuration file(s) that were
    changed for debugging purposes been reverted back to production
    settings?
  - If you're using [multiple data backends][config backend], are all of your
    bucket types configured to use the correct backend?
  - If you are using Riak Security, have you checked off all items in
    the [security checklist][security basics] and turned on security?
  - If you're using [multiple data backends][config backend], do all machines'
    config files agree on their configuration?
  - Do all nodes agree on the value of the [`allow_mult`][config basic] setting?
  - Do you have a [sibling resolution][usage conflict resolution] strategy in
    place if `allow_mult` is set to `true`?
  - Have you carefully weighed the [consistency trade-offs][concept eventual consistency] that must be made if `allow_mult` is set to `false`?
  - Are all of your [apps replication properties][apps replication properties] configured correctly and uniformly across the cluster?
  - If you are using [Riak Search][usage search], is it enabled on all
    nodes? If you are not, has it been disabled on all nodes?
  - If you are using [strong consistency][concept strong consistency] for some or all of your
    data:
      * Does your cluster consist of at least three nodes? If it does
        not, you will not be able to use this feature, and you are
        advised against enabling it.
      * If your cluster does consist of at least three nodes, has the
        strong consistency subsystem been [enabled][config strong consistency] on all nodes?
      * Is the [`target_n_val`][config reference] that is set on each node higher than any `n_val` that you intend to use for strongly consistent bucket types (or any bucket types for that matter)? The default is 4, which will likely need to be raised if you are using strong consistency.
  - Have all [bucket types][cluster ops bucket types] that you intend to use
    been created and successfully activated?
  - If you are using [`riak_control`][use admin riak control], is it enabled on the node(s) from which you intend to use it?
* Check data mount points:
  - Is `/var/lib/riak` mounted?
  - Can you grow that disk later when it starts filling up?
  - Do all nodes have their own storage systems (i.e. no
    [SANs]), or do you have a plan in place for switching to that configuration later?
* Are all Riak KV nodes up?
  - Run `riak ping` on all nodes. You should get `pong` as a response.
  - Run `riak-admin wait-for-service riak_kv <node_name>@<IP>` on each
    node. You should get `riak_kv is up` as a response.

    The `<node_name>@<IP>` string should come from your [configuration
    file(s)][configure reference].
* Do all nodes agree on the ring state?
  - Run `riak-admin ringready`. You should get `TRUE ALL nodes agree on
    the ring [list_of_nodes]`.
  - Run `riak-admin member-status`. All nodes should be valid (i.e.
    listed as `Valid: 1`), and all nodes should appear in the list
  - Run `riak-admin ring-status`. The ring should be ready (`Ring Ready:
    true`), there should be no unreachable nodes (`All nodes are up and
    reachable`), and there should be no pending changes to the ring
    (`No pending changes`).
  - Run `riak-admin transfers`. There should be no active transfers (`No
    transfers active`).

## Operations

* Does your monitoring system ensure that [NTP][ntp] is
  running?
* Are you collecting [time series data][cluster ops inspect node] on
  the whole cluster?
  - System metrics
    + CPU load
    + Memory used
    + Network throughput
    + Disk space used/available
    + Disk input/output operations per second (IOPS)
  - Riak metrics (from the [`/stats`][troubleshoot http] HTTP endpoint or
    using [`riak-admin`][use admin riak-admin])
    + Latencies: `GET` and `PUT` (mean/median/95th/99th/100th)
    + Vnode stats: `GET`s, `PUT`s, `GET` totals, `PUT` totals
    + Node stats: `GET`s, `PUT`s, `GET` totals, `PUT` totals
    + Finite state machine (FSM) stats:
      * `GET`/`PUT` FSM `objsize` (99th and 100th percentile)
      * `GET`/`PUT` FSM `times` (mean/median/95th/99th/100th)
    + Protocol buffer connection stats
      * `pbc_connects`
      * `pbc_active`
      * `pbc_connects_total`
* Are the following being graphed (at least the key metrics)?
  - Basic system status
  - Median and 95th and 99th percentile latencies (as these tend to be
    leading indicators of trouble)

## Application and Load

* Have you benchmarked your cluster with simulated load to confirm that
  your configuration will meet your performance needs?
* Are the [develop client libraries] in use in your application up to date?
* Do the client libraries that you're using support the version of Riak KV
  that you're deploying?

## Confirming Configuration with Riaknostic

Recent versions of Riak KV ship with Riaknostic, a diagnostic utility that
can be invoked by running `riak-admin diag <check>`, where `check` is
one of the following:

* `disk`
* `dumps`
* `memory_use`
* `nodes_connected`
* `ring_membership`
* `ring_preflists`
* `ring_size`
* `search`
* `sysctl`

Running `riak-admin diag` with no additional arguments will run all
checks and report the findings. This is a good way of verifying that
you've gotten at least some of the configurations mentioned above
correct, that all nodes in your cluster are up, and that nothing is
grossly misconfigured. Any warnings produced by `riak-admin diag` should
be addressed before going to production.

## Troubleshooting and Support

* Does your team, including developing and operations, know how to open
  support requests with Basho?
* Is your team familiar with Basho Support's Service-Level Agreement
  (SLA) levels?
  - Normal and Low are for issues not immediately impacting production
    systems
  - High is for problems that impact production or soon-to-be-production
    systems, but where stability is not currently compromised
  - Urgent is for problems causing production outages or for those
    issues that are likely to turn into production outages very soon.
    On-call engineers respond to urgent requests within 30 minutes,
    24 / 7.
* Does your team know how to gather `riak-debug` results from the whole
  cluster when opening tickets? If not, that process goes something like
  this:
  - SSH into each machine, run `riak-debug`, and grab the resultant
    `.tar.gz` file
  - Attach all debug tarballs from the whole cluster each time you open
    a new High- or Urgent-priority ticket

## The Final Step: Taking it to Production

Once you've been running in production for a month or so, look back at
the metrics gathered above. Based on the numbers you're seeing so far,
configure alerting thresholds on your latencies, disk consumption, and
memory. These are the places most likely to give you advance warning of
trouble.

When you go to increase capacity down the line, having historic metrics
will give you very clear indicators of having resolved scaling problems,
as well as metrics for understanding what to upgrade and when.
