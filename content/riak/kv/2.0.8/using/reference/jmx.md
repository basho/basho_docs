---
title: "JMX Monitoring"
description: ""
project: "riak_kv"
project_version: "2.0.8"
menu:
  riak_kv-2.0.8:
    name: "JMX Monitoring"
    identifier: "managing_ref_jmx"
    weight: 108
    parent: "managing_ref"
toc: true
commercial_offering: true
aliases:
    - /riak/2.0.8/ops/running/monitoring/jmx
    - /riak/kv/2.0.8/ops/running/monitoring/jmx
---

Riak exposes monitoring data via JMX.  To enable JMX monitoring, edit the [`app.config`]({{<baseurl>}}riak/kv/2.0.8/configuring/reference/#app-config) associated with your Riak installation and set the `enabled` property of the `riak_jmx` section to `true` as shown below.  The TCP port on which the JMX provider listens is also configurable in this section (the default JMX port is `41110`).

```erlang
    {riak_jmx, [
        {enabled, true},
        {port, 41110}
      ]}
```

To view JMX data---assuming that you have the Sun JDK installed---launch JConsole as follows:

```bash
$ jconsole <hostname_to_monitor>:<jmx_port>
```

Once connected, click on the **MBeans** tab, expand the **com.basho.riak** tree view, and select **Attributes**. The attributes listed in the table below will be displayed.

Riak JMX has been tested with the Sun JRE 1.6.0_12 and 1.6.0_20. Some older/non-Sun JREs do not work (e.g. the default java-gcj JRE installed on Debian lenny). If you have problems with JMX or see the message below, please try upgrading to the Sun JRE:

```log
   =INFO REPORT==== 9-Jun-2010::08:14:57 ===
   JMX server monitor <pid> exited with code <non-zero>.
```

## Exported JMX Attributes
<br>
<table>
    <tr>
        <th WIDTH="30%">Attribute</th>
        <th WIDTH="15%">Type</th>
        <th WIDTH="55%">Description</th>
    </tr>
    <tr>
        <td><tt>CPUNProcs</tt></td>
        <td>int</td>
        <td>Number of running processes</td>
    </tr>
    <tr>
        <td><tt>CpuAvg1</tt></td>
        <td>int</td>
        <td>1 minute load average</td>
    </tr>
    <tr>
        <td><tt>CpuAvg5</tt></td>
        <td>int</td>
        <td>5 minute load average</td>
    </tr>
    <tr>
        <td><tt>CpuAvg15</tt></td>
        <td>int</td>
        <td>15 minute load average</td>
    </tr>
    <tr>
        <td><tt>NodeGetFsmTime95</tt></td>
        <td>float</td>
        <td>95th percentile GET time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodeGetFsmTime99</tt></td>
        <td>float</td>
        <td>99th percentile GET time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodeGetFsmTimeMax</tt></td>
        <td>float</td>
        <td>Maximum GET time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodeGetFsmTimeMean</tt></td>
        <td>float</td>
        <td>Mean GET time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodeGetFsmTimeMedian</tt></td>
        <td>float</td>
        <td>Median GET time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodeGets</tt></td>
        <td>int</td>
        <td>Number of GETs in past minute</td>
    </tr>
    <tr>
        <td><tt>NodeGetsTotal</tt></td>
        <td>int</td>
        <td>Number of GETs since node start</td>
    </tr>
    <tr>
        <td><tt>NodeName</tt></td>
        <td>string</td>
        <td>Node name</td>
    </tr>
    <tr>
        <td><tt>NodePutFsmTime95</tt></td>
        <td>float</td>
        <td>95th percentile PUT time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodePutFsmTime99</tt></td>
        <td>float</td>
        <td>99th percentile PUT time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodePutFsmTimeMax</tt></td>
        <td>float</td>
        <td>Maximum PUT time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodePutFsmTimeMean</tt></td>
        <td>float</td>
        <td>Mean PUT time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodePutFsmTimeMedian</tt></td>
        <td>float</td>
        <td>Median PUT time (microseconds)</td>
    </tr>
    <tr>
        <td><tt>NodePuts</tt></td>
        <td>int</td>
        <td>Number of PUTs in past minute</td>
    </tr>
    <tr>
        <td><tt>NodePutsTotal</tt></td>
        <td>int</td>
        <td>Number of PUTs since node start</td>
    </tr>
    <tr>
        <td><tt>PBCActive</tt></td>
        <td>int</td>
        <td>Number of active Protocol Buffers connections</td>
    </tr>
    <tr>
        <td><tt>PBCConnects</tt></td>
        <td>int</td>
        <td>Number of Protocol Buffers connections in past minute</td>
    </tr>
    <tr>
        <td><tt>PBCConnectsTotal</tt></td>
        <td>int</td>
        <td>Number of Protocol Buffers connections since node start</td>
    </tr>
    <tr>
        <td><tt>RingCreationSize</tt></td>
        <td>int</td>
        <td>Number of partitions in Riak ring</td>
    </tr>
    <tr>
        <td><tt>VnodeGets</tt></td>
        <td>int</td>
        <td>Number of vnode-level GETs in past minute</td>
    </tr>
    <tr>
        <td><tt>VnodeGetsTotal</tt></td>
        <td>int</td>
        <td>Number of vnode-level GETs since node start</td>
    </tr>
    <tr>
        <td><tt>VnodePuts</tt></td>
        <td>int</td>
        <td>Number of vnode-level PUTs in past minute</td>
    </tr>
    <tr>
        <td><tt>VnodePutsTotal</tt></td>
        <td>int</td>
        <td>Number of vnode-level PUTs since node start</td>
    </tr>
</table>
