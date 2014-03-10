---
title: JMX Monitoring
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, config]
---

{{#2.0.0-}}
Riak exposes monitoring data via JMX. To enable JMX monitoring, edit the `[[app.config|Configuration Files#app-config]]` associated with your Riak installation and set the `enabled` property of the `riak_jmx` section to `true` as shown below. The TCP port on which the JMX provider listens is also configurable in this section (the default JMX port is `41110`).

```erlang
    {riak_jmx, [
        {enabled, true},
        {port, 41110}
      ]}
```
{{/2.0.0-}}
{{#2.0.0+}}
Riak exposes monitoring data via JMX. To enable JMX monitoring, edit the `[[riak.conf|Configuration Files]]` associated with your Riak installation and set the `jmx` property to `on`. The TCP port on which the JMX provider listens is configurable using the `jmx.port` parameter (the default is `41110`).
{{/2.0.0+}}

To view JMX data---assuming that you have the Sun JDK installed---launch JConsole as follows:

```bash
jconsole <hostname_to_monitor>:<jmx_port>
```

Once connected, click on the **MBeans** tab, expand the **com.basho.riak** tree view, and select **Attributes**. The attributes listed in the table below will be displayed.

Riak JMX has been tested with the Sun JRE 1.6.0_12 and 1.6.0_20. Some older/non-Sun JREs do not work (e.g. the default java-gcj JRE installed on Debian lenny). If you have problems with JMX or see the message below, please try upgrading to the Sun JRE:

```log
=INFO REPORT==== 9-Jun-2010::08:14:57 ===
JMX server monitor <pid> exited with code <non-zero>.
```

## Exported JMX Attributes

Attribute | Type | Description
:---------|:-----|:-----------
`CPUNProcs` | int | Number of running processes
`CpuAvg1` | int | 1-minute load average
`CpuAvg5` | int | 5-minute load average
`CpuAvg15` | int | 15-minute load average
`NodeGetFsmTime95` | float | 95th-percentile GET time (microseconds)
`NodeGetFsmTime99` | float | 99th percentile GET time (microseconds)
`NodeGetFsmTimeMax` | float | Maximum GET time (microseconds)
`NodeGetFsmTimeMean` | float | Mean GET time (microseconds)
`NodeGetFsmTimeMedian` | float | Median GET time (microseconds)
`NodeGets` | int | Number of GETs in past minute
`NodeGetsTotal` | int | Number of GETs since node start
`NodeName` | string | Node name
`NodePutFsmTime95` | float | 95th percentile PUT time (microseconds)
`NodePutFsmTime99` | float | 99th percentile PUT time (microseconds)
`NodePutFsmTimeMax` | float | Maximum PUT time (microseconds)
`NodePutFsmTimeMean` | float | Mean PUT time (microseconds)
`NodePutFsmTimeMedian` | float | Median PUT time (microseconds)
`NodePuts` | int | Number of PUTs in past minute
`NodePutsTotal` | int | Number of PUTs since node start
`PBCActive` | int | Number of active Protocol Buffers connections
`PBCConnects` | int | Number of Protocol Buffers connections in past minute
`PBCConnectsTotal` | int | Number of Protocol Buffers connections since node start
`RingCreationSize` | int | Number of partitions in Riak ring
`VnodeGets` | int | Number of vnode-level GETs in past minute
`VnodeGetsTotal` | int | Number of vnode-level GETs since node start
`VnodePuts` | int | Number of vnode-level PUTs in past minute
`VnodePutsTotal` | int | Number of vnode-level PUTs since node start

