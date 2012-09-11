<div class="info"><div class="title">Riak Enterprise Only</div>This documentation applies only to Riak Enterprise, Basho's commercial extension to <a href="http://wiki.basho.com/Riak.html">Riak</a>. To learn more about the differences between Riak and Riak Enterprise, <a href="http://basho.com/products/riak-overview/">read here</a>.  To talk to us about using Riak Enterprise,  <a href="http://info.basho.com/Wiki_Contact.html" target="_blank">let us know</a>.</div>
  

  Riak exposes monitoring data via JMX.  To enable JMX monitoring, edit the
  [[app.config|Configuration-Files#app.config]] and set the ```enabled``` property of the ```riak_jmx```
  section to ```true```, as follows.  The TCP port that the JMX provider listens on
  is also configurable in this section (the default JMX port is ```41110```).

```erlang
    {riak_jmx, [
        {enabled, true},
        {port, 41110}
      ]}
```

   To view JMX data if you have the Sun JDK installed, launch JConsole
   as follows:

```bash
   % jconsole <hostname_to_monitor>:<jmx_port> 
```

   Once connected, click on the 'MBeans' tab, expand the 'com.basho.riak' 
   tree view, and select 'Attributes'.  The attributes listed below will
   be displayed.

   Riak JMX has been tested with the Sun JRE 1.6.0_12 and 1.6.0_20.  Some
   older/non-Sun JREs do not work (e.g. the default java-gcj JRE installed
   on debian lenny).  If you have problems with JMX or see the
   message below, please try upgrading to the Sun JRE:

```text
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
        <td>CPUNProcs </td>
        <td>int
		</td>
        <td>Number of running processes      
		</td>
    </tr>
    <tr>
        <td>CpuAvg1 </td>
        <td>int
		</td>
        <td>1 minute load average      
		</td>
    </tr>
    <tr>
        <td>CpuAvg5 </td>
        <td>int
		</td>
        <td>5 minute load average      
		</td>
    </tr>
    <tr>
        <td>CpuAvg15  </td>
        <td>int
		</td>
        <td>15 minute load average                                       
		</td>
    </tr>
    <tr>
        <td>NodeGetFsmTime95 </td>
        <td>float
		</td>
        <td>95th percentile GET time (microseconds)    
		</td>
    </tr>
    <tr>
        <td>NodeGetFsmTime99</td>
        <td>float 
		</td>
        <td>99th percentile GET time (microseconds)           
		</td>
    </tr>
    <tr>
        <td>NodeGetFsmTimeMax</td>
        <td>float
		</td>
        <td>Maximum GET time (microseconds)    
		</td>
    </tr>
    <tr>
        <td>NodeGetFsmTimeMean</td>
        <td>float
		</td>
        <td>Mean GET time (microseconds)    
		</td>
    </tr>
    <tr>
        <td>NodeGetFsmTimeMedian</td>
        <td>float 
		</td>
        <td>Median GET time (microseconds)    
		</td>
    </tr>
    <tr>
        <td>NodeGets</td>
        <td>int
		</td>
        <td>Number of GETs in past minute   
		</td>
    </tr>
    <tr>
        <td>NodeGetsTotal  </td>
        <td>int
		</td>
        <td>Number of GETs since node start    
		</td>
    </tr>
    <tr>
        <td>NodeName</td>
        <td>string
		</td>
        <td>Node name    
		</td>
    </tr>
    <tr>
        <td>NodePutFsmTime95 </td>
        <td>float 
		</td>
        <td>95th percentile PUT time (microseconds)    
		</td>
    </tr>
    <tr>
        <td>NodePutFsmTime99 ) </td>
        <td>float 
		</td>
        <td>99th percentile PUT time (microseconds)    
		</td>
    </tr>
    <tr>
        <td>NodePutFsmTimeMax</td>
        <td>float
		</td>
        <td>Maximum PUT time (microseconds)     
		</td>
    </tr>
    <tr>
        <td>NodePutFsmTimeMean</td>
        <td>float 
		</td>
        <td>Mean PUT time (microseconds)   
		</td>
    </tr>
    <tr>
        <td>NodePutFsmTimeMedian</td>
        <td>float
		</td>
        <td>Median PUT time (microseconds) 
		</td>
    </tr>
    <tr>
        <td>NodePuts</td>
        <td>int
		</td>
        <td>Number of PUTs in past minute   
		</td>
    </tr>
    <tr>
        <td>NodePutsTotal </td>
        <td>int
		</td>
        <td>Number of PUTs since node start      
		</td>
    </tr>
    <tr>
        <td>PBCActive   </td>
        <td>int
		</td>
        <td>Number of active Protocol Buffers connections 
		</td>
    </tr>
    <tr>
        <td>PBCConnects </td>
        <td>int
		</td>
        <td>Number of Protocol Buffers connections in past minute     
		</td>
    </tr>
    <tr>
        <td>PBCConnectsTotal</td>
        <td>int
		</td>
        <td>Number of Protocol Buffers connections since node start 
		</td>
    </tr>
    <tr>
        <td>RingCreationSize </td>
        <td>int
		</td>
        <td>Number of partitions in Riak ring   
		</td>
    </tr>
    <tr>
        <td>VnodeGets  </td>
        <td>int
		</td>
        <td>Number of vnode-level GETs in past minute     
		</td>
    </tr>
    <tr>
        <td>VnodeGetsTotal</td>
        <td>int
		</td>
        <td>Number of vnode-level GETs since node start      
		</td>
    </tr>
    <tr>
        <td>VnodePuts   </td>
        <td>int
		</td>
        <td>Number of vnode-level PUTs in past minute      
		</td>
    </tr>
    <tr>
        <td>VnodePutsTotal</td>
        <td>int
		</td>
        <td>Number of vnode-level PUTs since node start  
		</td>
    </tr>

</table>
