---
title: Statistics and Monitoring
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
moved: {
    '1.4.0-': '/cookbooks/Statistics-and-Monitoring'
}
---

## Statistics from Riak

Riak provides data related to current operating status, which includes statistics in the form of counters and histograms. These statistics are made available through the HTTP API via the `[[/stats|HTTP Status]]` endpoint, or through the `[[riak-admin status|Inspecting a Node#riak-admin-status]]` command.

This page presents the most commonly monitored and gathered statistics, as well as numerous solutions for monitoring and gathering statistics that our customers and community report using successfully in Riak cluster environments. You can learn more about the specific Riak statistics provided in the [[Inspecting a Node]] and [[HTTP Status]] documentation.

### Counters
Riak provides counters for GETs, PUTs, read repairs, and other common operations.  By default, these counters count either the number of operations occurring within the last minute, or for the runtime duration of the node.

#### Gets and Puts
GET/PUT counters are provided for both nodes and vnodes. These counters are commonly graphed over time for trend analysis, capacity planning, and so forth.

At a minimum, the following stats should be graphed: `node_gets`, `node_gets_total`, `node_puts`, `node_puts_total`, `vnode_gets`, `vnode_gets_total`, `vnode_puts_total`.

#### Read Repairs
Read repair counters are commonly graphed and monitored for abnormally high totals, which can be indicative of an issue.

#### Coordinated Redirection
Counters representing the number of coordinated node redirection operations are provided in total since node startup.

### Statistics
Riak provides statistics for a range of operations.  By default, Riak provides the mean, median, 95th percentile, 99th percentile, and 100th percentile over a 60 second window.

#### Finite State Machine Time
Riak exposes Finite State Machine (FSM) time counters (`node_get_fsm_time_*` & `node_put_fsm_time_*`) that measure the amount of time in microseconds required to traverse the GET or PUT FSM code, offering a picture of general node health.

#### GET FSM Object Size
GET FSM Object Size (`node_get_fsm_objsize_*`) measures the size of objects flowing through this node's GET finite state machine (GET_FSM). The size of an object is obtained by summing the length of the object's bucket name, key, serialized vector clock, value, and the serialized metadata of each sibling.

#### GET FSM Siblings
GET FSM Sibling (`node_get_fsm_siblings_*`) provides a histogram (with 60 second window) of the number of siblings encountered by this node on the occasion of a GET request.

### Additional Riak Metrics to Graph
`memory_processes_used`, `sys_process_count`, `pbc_connect`, `pbc_active`.


## Systems Metrics To Graph

Metric                 |
---------------------- |
Available Disk Space   |
IOWait                 |
Read Operations        |
Write Operations       |
Network Throughput     |
Load Average           |


## Statistics and Monitoring Tools
There are many open source, self-hosted, and service-based solutions for aggregating and analyzing statistics and log data for the purposes of monitoring, alerting, and trend analysis on a Riak cluster. Some solutions provide Riak-specific modules or plugins as noted.

The following are solutions which customers and community members have reported success with when used for monitoring the operational status of their Riak clusters. Community and open source projects are presented along with commercial and hosted services.

### Community and Open Source Tools

#### Riaknostic
[Riaknostic](http://riaknostic.basho.com) is a growing suite of diagnostic checks that can be run against your Riak node to discover common problems and recommend how to resolve them. These checks are derived from the experience of the Basho Client Services Team as well as numerous public discussions on the mailing list, IRC room, and other online media.

Riaknostic integrates into the `riak-admin` command via a `diag` subcommand, and is a great first step in the process of diagnosing and troubleshooting issues on Riak nodes.

#### Riak Control
[[Riak Control]] is Basho's REST-driven user-interface for managing Riak clusters. It is designed to give you quick insight into the health of your cluster and allow for easy management of nodes.

While Riak Control does not currently offer specific monitoring and statistics aggregation or analysis functionality, it does offer features which provide immediate insight into overall cluster health, node status, and handoff operations.

#### collectd
[collectd](http://collectd.org) gathers statistics about the system it is running on and stores them. The statistics are then typically graphed to find current performance bottlenecks, predict system load, and analyze trends.

#### Ganglia
[Ganglia](http://ganglia.info) is a monitoring system specifically designed for large, high-performance groups of computers, such as clusters and grids. Customers and community members using Riak have reported success in using Ganglia to monitor Riak clusters.

A [Riak Ganglia module](https://github.com/jnewland/gmond_python_modules/tree/master/riak/) for collecting statistics from the Riak HTTP `[[/stats|HTTP Status]]` endpoint is also available.

#### Nagios
[Nagios](http://www.nagios.org) is a monitoring and alerting solution that can provide information on the status of Riak cluster nodes, in addition to various types of alerting when particular events occur. Nagios also offers logging and reporting of events and can be used for identifying trends and capacity planning.

A collection of [reusable Riak-specific scripts](https://github.com/basho/riak_nagios) are available to the community for use with Nagios.

#### Riemann
[Riemann](http://aphyr.github.com/riemann/) uses a powerful stream processing language to aggregate events from client agents running on Riak nodes, and can help track trends or report on events as they occur. Statistics can be gathered from your nodes and forwarded to a solution such as Graphite for producing related graphs.

A [Riemann Tools](https://github.com/aphyr/riemann.git) project consisting of small programs for sending data to Riemann provides a module specifically designed to read Riak statistics.

#### OpenTSDB
[OpenTSDB](http://opentsdb.net) is a distributed, scalable Time Series Database (TSDB) used to store, index, and serve metrics from various sources. It can collect data at a large scale and graph these metrics on the fly.

A [Riak collector for OpenTSDB](https://github.com/stumbleupon/tcollector/blob/master/collectors/0/riak.py) is available as part of the [tcollector framework](https://github.com/stumbleupon/tcollector).

### Commercial and Hosted Service Tools
The following are some commercial tools which Basho customers have reported successfully using for statistics gathering and monitoring within their Riak clusters.

#### Circonus
[Circonus](http://circonus.com) provides organization-wide monitoring, trend analysis, alerting, notifications, and dashboards. It can been used to provide trend analysis and help with troubleshooting and capacity planning in a Riak cluster environment.

<!--
Need more information on this one...
#### Scout
[Scout](https://scoutapp.com)
-->

#### Splunk
[Splunk](http://www.splunk.com) is available as downloadable software or as a service, and provides tools for visualization of machine generated data such as log files. It can be connected to Riak's HTTP statistics `[[/stats|HTTP Status]]` endpoint.

Splunk can be used to aggregate all Riak cluster node operational log files, including operating system and Riak-specific logs and Riak statistics data. These data are then available for real time graphing, search, and other visualization ideal for troubleshooting complex issues and spotting trends.

## Summary
Riak exposes numerous forms of vital statistic information which can be aggregated, monitored, analyzed, graphed, and reported on in a variety of ways using numerous open source and commercial solutions.

If you use a solution not listed here with Riak and would like to include it (or would otherwise like to update the information on this page), feel free to fork the docs, add it in the appropriate section, and send a pull request to the [Riak Docs](https://github.com/basho/basho_docs).

## References

* [[Inspecting a Node]]
* [Riaknostic](http://riaknostic.basho.com)
* [[Riak Control]]
* [collectd](http://collectd.org)
* [Ganglia](http://ganglia.info)
* [Nagios](http://www.nagios.org)
* [Riemann](http://aphyr.github.com/riemann/)
* [Riemann Github](https://github.com/aphyr/riemann)
* [OpenTSDB](http://opentsdb.net)
* [tcollector project](https://github.com/stumbleupon/tcollector)
* [tcollector Riak module](https://github.com/stumbleupon/tcollector/blob/master/collectors/0/riak.py)
* [Folsom Backed Stats Riak 1.2](http://basho.com/blog/technical/2012/07/02/folsom-backed-stats-riak-1-2/)
* [Circonus](http://circonus.com)
* [Splunk](http://www.splunk.com)
* [Riak Docs Github](https://github.com/basho/basho_docs)
