---
title: Statistics and Monitoring
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
moved: {
  '1.4.0-': '/cookbooks/Statistics-and-Monitoring',
  '1.4.0-2.0.0': '/ops/running/monitoring/stats-and-monitoring'
}
---

Riak provides data related to current operating status, which includes
statistics in the form of counters and histograms. These statistics
are made available through the HTTP API via the `[[/stats|HTTP
Status]]` endpoint, or through the `[[riak-admin|riak-admin Command
Line]]` interface, in particular the `stat` and `status` commands.

This page presents the most commonly monitored and gathered
statistics, as well as numerous solutions for monitoring and gathering
statistics that our customers and community report using successfully
in Riak cluster environments. You can learn more about the specific
Riak statistics provided in the [[Inspecting a Node]] and [[HTTP
Status]] documentation.

## System Metrics To Graph

Graphing general system metrics of Riak nodes will help with
diagnostics and early warnings of potential problems, as well as help
guide provisioning and scaling decisions.

* CPU (user/system/wait/idle)
* Processor Load
* Available Memory
* Available disk space
* Used file descriptors
* Swap Usage
* IOWait
* Read operations
* Write operations
* Network throughput
* Network errors

We also recommend tracking your system's virtual and
writebacks. Things like massive flushes of dirty pages or steadily
climbing writeback volumes can indicate poor virtual memory tuning.
More information can be found [here][sysctl_vm_txt] and in our
documentation on [[system tuning|System Performance
Tuning#Storage-and-File-System-Tuning]].

## Riak Metrics to Graph
Riak metrics fall into several general categories:

1. Throughput metrics
2. Latency metrics
3. Erlang resource usage metrics
4. General Riak load/health metrics

If graphing all of the [[available Riak metrics|Inspecting a Node]] is
not practical, you should pick a minimum relevant subset from these
categories. Some of the most helpful metrics are discussed below.

### Throughput Metrics

Graphing the throughput stats relevant to your use case is often
helpful for capacity planning and usage trend analysis. In addition,
it helps you establish an expected baseline -- that way, you can
investigate unexpected spikes or dips in the throughput.  The
following stats are recorded for operations that happened *during the
last minute*.

Metric | Relevance | Operations (for the last minute)
:--------|:--------|:--------------------------------
```node_gets``` | K/V | Reads coordinated by this node
```node_puts``` | K/V | Writes coordinated by this node
```vnode_counter_update``` | Data Types | Update [Counters][data_types_counters] operations coordinated by local vnodes
```vnode_set_update``` | Data Types | Update [Sets][data_types_sets] operations coordinated by local vnodes
```vnode_map_update``` | Data Types | Update [Maps][data_types_maps] operations coordinated by local vnodes
```search_query_throughput_one``` | Search | Search queries on the node
```search_index_throughtput_one``` | Search | Documents indexed by Search
```consistent_gets``` | Strong Consistency | Consistent reads on this node
```consistent_puts``` | Strong Consistency | Consistent writes on this node
```vnode_index_reads``` | Secondary Indexes | Number of local replicas participating in secondary index reads

Note that there are no separate stats for updates to Flags or
Registers, as these are included in ```vnode_map_update```.

### Latency Metrics

As with the throughput metrics, keeping an eye on average (and max)
latency times will help detect usage patterns, and provide advanced
warnings for potential problems.

<div class="note">
<div class="title">Note on FSM Time Stats</div>
FSM Time Stats represent the amount of time in microseconds required
to traverse the GET or PUT Finite State Machine code, offering a
picture of general node health. From your application's perspective,
FSM Time effectively represents experienced latency. Mean, Median, and
95th-, 99th-, and 100th-percentile (Max) counters are displayed. These
are one-minute stats.
</div>

Metric | Also | Relevance | Latency (in microseconds)
:------|:-----|:----------|:-------------------------
```node_get_fsm_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | K/V | Time between reception of client read request and subsequent response to client
```node_put_fsm_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | K/V | Time between reception of client write request and subsequent response to client
```object_counter_merge_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100```  | Data Types | Time it takes to perform an Update Counter operation
```object_set_merge_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100```  | Data Types | Time it takes to perform an Update Set operation
```object_map_merge_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100```  | Data Types | Time it takes to perform an Update Map operation
```search_query_latency_median``` | ```_min```, ```_95```, ```_99```, ```_999```, ```_max``` | Search | Search query latency
```search_index_latency_median``` | ```_min```, ```_95```, ```_99```, ```_999```, ```_max``` | Search | Time it takes Search to index a new document
```consistent_get_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | Strong Consistency | Strongly consistent read latency
```consistent_put_time_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | Strong Consistency | Strongly consistent write latency

### Erlang Resource Usage Metrics These are system metrics from the
perspective of the Erlang VM, measuring resources allocated and used
by Erlang.

Metric | Notes
:------|:-------------------------
```sys_process_count``` | Number of processes currently running in the Erlang VM
```memory_processes``` | Total amount of memory allocated for Erlang processes (in bytes)
```memory_processes_used``` | Total amount of memory used by Erlang processes (in bytes)

### General Riak Load/Health Metrics

These various stats give a picture of the general level of activity or
load on the Riak node at any given moment.

Metric | Also | Notes
:------|:-----|:------------------
```node_get_fsm_siblings_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | Number of siblings encountered during all GET operations by this node within the last minute. Watch for abnormally high sibling counts, especially max ones.
```node_get_fsm_objsize_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | Object size encountered by this node within the last minute. Abnormally large objects (especially paired with high sibling counts) can indicate sibling explosion.
```riak_search_vnodeq_mean``` | ```_median```, ```_95```, ```_99```, ```_100``` | Number of unprocessed messages in the vnode message queues of the Riak Search subsystem on this node in the last minute. The queues give you an idea of how backed up Solr is getting.
```search_index_fail_one``` | | Number of "Failed to index document" errors Search encountered for the last minute
```pbc_active``` | | Number of currently active protocol buffer connections
```pbc_connect``` | | Number of new protocol buffer connections established during the last minute
```read_repairs``` | | Number of read repair operations this node has coordinated in the last minute (determine baseline, watch for abnormal spikes)
```list_fsm_active``` | | Number of List Keys FSMs currently active (should be 0)
```node_get_fsm_rejected``` | | Number of GET FSMs actively being rejected by Sidejob's overload protection
```node_put_fsm_rejected``` | | Number of PUT FSMs actively being rejected by Sidejob's overload protection

## Command-line Interface

The `[[riak-admin|riak-admin Command Line]]` tool provides two
interfaces for retrieving statistics and other information: `status`
and `stat`.

### status

Running the `riak-admin status` command will return all of the
currently available information from a running node.

```bash
riak-admin status
```

This will return a list of over 300 key/value pairs, like this:

```
1-minute stats for 'dev1@127.0.0.1'
-------------------------------------------
connected_nodes : ['dev2@127.0.0.1','dev3@127.0.0.1']
consistent_get_objsize_100 : 0
consistent_get_objsize_195 : 0
... etc ...
```

A comprehensive list of available stats can be found in the
[[Inspecting a Node|Inspecting a Node#riak-admin-status]] document.

### stat

The `riak-admin stat` command is related to the `riak-admin status`
command but provides a more fine-grained interface for interacting with
stats and information. Full documentation of this command can be found
in the [[Inspecting a Node|Inspecting a Node#riak-admin-stat]] document.

## Statistics and Monitoring Tools

There are many open source, self-hosted, and service-based solutions for
aggregating and analyzing statistics and log data for the purposes of
monitoring, alerting, and trend analysis on a Riak cluster. Some
solutions provide Riak-specific modules or plugins as noted.

The following are solutions which customers and community members have
reported success with when used for monitoring the operational status of
their Riak clusters. Community and open source projects are presented
along with commercial and hosted services.

<div class="note">
<div class="title">Note on Riak 2.x Statistics Support</div>
Many of the below third-party tools are either general-purpose tools with no
dedicated Riak support, or have plugins that were designed and/or built for a
specific use case in mind. As such, many of the below only aggregate the
statistics and messages that were output by Riak 1.4.x.</br>
Look for banners calling out the tools that do support the full set of Riak 2.x
statistics!
</div>

### Self-Hosted Monitoring Tools

#### Riaknostic

[Riaknostic](http://riaknostic.basho.com) is a growing suite of
diagnostic checks that can be run against your Riak node to discover
common problems and recommend how to resolve them. These checks are
derived from the experience of the Basho Client Services Team as well as
numerous public discussions on the mailing list, IRC room, and other
online media.

Riaknostic integrates into the `riak-admin` command via a `diag`
subcommand, and is a great first step in the process of diagnosing and
troubleshooting issues on Riak nodes.

#### Riak Control

[[Riak Control]] is Basho's REST-driven user-interface for managing Riak
clusters. It is designed to give you quick insight into the health of
your cluster and allow for easy management of nodes.

While Riak Control does not currently offer specific monitoring and
statistics aggregation or analysis functionality, it does offer features
which provide immediate insight into overall cluster health, node
status, and handoff operations.

#### collectd

[collectd](http://collectd.org) gathers statistics about the system it
is running on and stores them. The statistics are then typically graphed
to find current performance bottlenecks, predict system load, and
analyze trends.

#### Ganglia

[Ganglia](http://ganglia.info) is a monitoring system specifically
designed for large, high-performance groups of computers, such as
clusters and grids. Customers and community members using Riak have
reported success in using Ganglia to monitor Riak clusters.

A [Riak Ganglia module][riak_ganglia] for collecting statistics from
the Riak HTTP `[[/stats|HTTP Status]]` endpoint is also available.

#### Nagios

<div class="note">
<strong>Tested and Verified Support for Riak 2.x Stats.</strong>
</div>

[Nagios](http://www.nagios.org) is a monitoring and alerting solution
that can provide information on the status of Riak cluster nodes, in
addition to various types of alerting when particular events occur.
Nagios also offers logging and reporting of events and can be used for
identifying trends and capacity planning.

A collection of [reusable Riak-specific scripts][riak_nagios] are
available to the community for use with Nagios.

#### OpenTSDB

[OpenTSDB](http://opentsdb.net) is a distributed, scalable Time Series Database
(TSDB) used to store, index, and serve metrics from various sources. It can
collect data at a large scale and graph these metrics on the fly.

A [Riak collector for OpenTSDB][tcollector_riak_plugin] is available as part of
the [tcollector framework][tcollector].

#### Riemann

[Riemann](http://aphyr.github.com/riemann/) uses a powerful stream
processing language to aggregate events from client agents running on
Riak nodes, and can help track trends or report on events as they occur.
Statistics can be gathered from your nodes and forwarded to a solution
such as Graphite for producing related graphs.

A [Riemann Tools](https://github.com/aphyr/riemann.git) project
consisting of small programs for sending data to Riemann provides a
module specifically designed to read Riak statistics.

#### Zabbix

<div class="note">
<strong>Tested and Verified Support for Riak 2.x Stats.</strong>
</div>

[Zabbix](http://www.zabbix.com) is an open-source performance monitoring,
alerting, and graphing solution that can provide information on the state of
Riak cluster nodes.

A [Zabbix plugin for Riak][riak_zabbix] is available to get you started
monitoring Riak using Zabbix.


### Hosted Service Monitoring Tools

The following are some commercial tools which Basho customers have
reported successfully using for statistics gathering and monitoring
within their Riak clusters.

#### Circonus
[Circonus](http://circonus.com) provides organization-wide monitoring,
trend analysis, alerting, notifications, and dashboards. It can been
used to provide trend analysis and help with troubleshooting and
capacity planning in a Riak cluster environment.

#### New Relic

<div class="note">
<strong>Tested and Verified Support for Riak 2.x Stats.</strong>
</div>

[New Relic](http://newrelic.com) is a data analytics and visualization platform
that can provide information on the current and past states of Riak nodes and
visualizations of machine generated data such as log files.

A [Riak New Relic Agent][riak_new_relic] for collecting statistics from the Riak
HTTP `[[/stats|HTTP Status]]` endpoint is also available.

#### Splunk

[Splunk](http://www.splunk.com) is available as downloadable software or
as a service, and provides tools for visualization of machine generated
data such as log files. It can be connected to Riak's HTTP statistics
`[[/stats|HTTP Status]]` endpoint.

Splunk can be used to aggregate all Riak cluster node operational log
files, including operating system and Riak-specific logs and Riak
statistics data. These data are then available for real time graphing,
search, and other visualization ideal for troubleshooting complex issues
and spotting trends.

## Summary

Riak exposes numerous forms of vital statistic information which can be
aggregated, monitored, analyzed, graphed, and reported on in a variety
of ways using numerous open source and commercial solutions.

If you use a solution not listed here with Riak and would like to
include it (or would otherwise like to update the information on this
page), feel free to fork the docs, add it in the appropriate section,
and send a pull request to the [Riak
Docs](https://github.com/basho/basho_docs).

## References

* [[Inspecting a Node]]
* [Riaknostic](http://riaknostic.basho.com)
* [[Riak Control]]
* [collectd](http://collectd.org)
* [Ganglia](http://ganglia.info)
* [Nagios](http://www.nagios.org)
* [OpenTSDB](http://opentsdb.net)
* [tcollector framework][tcollector]
* [Riemann](http://aphyr.github.com/riemann/)
* [Riemann Github](https://github.com/aphyr/riemann)
* [Zabbix](http://www.zabbix.com)
* [Circonus](http://circonus.com)
* [New Relic](http://newrelic.com)
* [Splunk](http://www.splunk.com)
* [Riak Docs on Github](https://github.com/basho/basho_docs)


[sysctl_vm_txt]: https://www.kernel.org/doc/Documentation/sysctl/vm.txt
[data_types_counters]: http://docs.basho.com/riak/latest/dev/using/data-types/#Counters
[data_types_sets]: http://docs.basho.com/riak/latest/dev/using/data-types/#Sets
[data_types_maps]: http://docs.basho.com/riak/latest/dev/using/data-types/#Maps
[riak_nagios]: https://github.com/basho/riak_nagios
[tcollector]: https://github.com/stumbleupon/tcollector
[tcollector_riak_plugin]: https://github.com/stumbleupon/tcollector/blob/master/collectors/0/riak.py
[riak_zabbix]: https://github.com/basho/riak-zabbix
[riak_new_relic]: https://github.com/basho/riak_newrelic
[riak_ganglia]: https://github.com/jnewland/gmond_python_modules/tree/master/riak/
