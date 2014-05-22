---
title: Configuration Files
project: riak
version: 2.0.0+
document: reference
toc: true
audience: intermediate
---

Riak has a `riak.conf` configuration file located in `/etc` if you are using a source install or in `/etc/riak` if you used a binary install.

The `riak.conf` file is used to set a wide variety of attributes for the node, from the storage backend that the node will use to store data to the location of SSL-related files to sibling resolution parameters and beyond.

At any time, you can get a snapshot of currently applied configurations through the command line. For a listing of *all* of the configs currently applied in the node:

```bash
riak config effective
```

This will output a long list of the following form:

```bash
anti_entropy = active
anti_entropy.bloomfilter = on
anti_entropy.concurrency_limit = 2
# and so on
```

For detailed information about a particular configuration variable, use the `config describe <variable>` command. This command will output a description of what the parameter configures, which datatype you should use to set the parameter (integer, string, enum, etc.), the default value of the parameter, the currently set value in the node, and the name of the parameter in `app.config` in older versions of Riak (if applicable).

For in-depth information about the `ring_size` variable, for example:

```bash
riak config describe ring_size
```

This will output the following:

```bash
Documentation for ring_size
Number of partitions in the cluster (only valid when first
creating the cluster). Must be a power of 2, minimum 8 and maximum
1024.

   Datatype     : [integer]
   Default Value: 64
   Set Value    : undefined
   app.config   : riak_core.ring_creation_size
```

Below is a table listing the configurable parameters in `riak.conf`.

## Storage Backend

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>storage_backend</tt></td>
<td>Specifies the storage engine used for Riak's key-value data and secondary indexes (if supported).</td>
<td><tt>bitcask</tt></td>
</tr>

</tbody>
</table>

## Directories

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>platform_log_dir</tt></td>
<td></td>
<td><tt>./log</tt></td>
</tr>

<tr>
<td><tt>platform_lib_dir</tt></td>
<td></td>
<td><tt>./lib</tt></td>
</tr>

<tr>
<td><tt>platform_etc_dir</tt></td>
<td></td>
<td><tt>./etc</tt></td>
</tr>

<tr>
<td><tt>platform_data_dir</tt></td>
<td></td>
<td><tt>./data</tt></td>
</tr>

<tr>
<td><tt>platform_bin_dir</tt></td>
<td></td>
<td><tt>./bin</tt></td>
</tr>

</tbody>
</table>

## Search

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>search</tt></td>
<td>To enable Search, set this to <tt>on</tt>.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>search.root_dir</tt></td>
<td>The root directory for Riak Search, under which index data and configuration is stored.</td>
<td><tt>./data/yz</tt></td>
</tr>

<tr>
<td><tt>search.anti_entropy.data_dir</tt></td>
<td>The directory in which Search's Active Anti-Entropy data files are stored</td>
<td><tt>./data/yz_anti_entropy</tt></td>
</tr>

<tr>
<td><tt>search.solr.jvm_options</tt></td>
<td>The options to pass to the Solr JVM. Non-standard options, i.e. <tt>-XX</tt>, may not be portable across JVM implementations. Example: <tt>XX:+UseCompressedStrings</tt></td>
<td><tt>-Xms1g -Xmx1g -XX:+UseStringCache -XX:+UseCompressedOops</tt></td>
</tr>

<tr>
<td><tt>search.solr.jmx_port</tt></td>
<td>The port to which Solr JMX binds. <strong>Note</strong>: Binds on every interface.</td>
<td><tt>8985</tt></td>
</tr>

<tr>
<td><tt>search.solr.port</tt></td>
<td>The port to which Solr binds. <strong>Note</strong>: Binds on every interface.</td>
<td><tt>8093</tt></td>
</tr>

<tr>
<td><tt>search.solr.start_timeout</tt></td>
<td>How long Riak will wait for Solr to start. The start sequence will be tried twice. If both attempts time out, the Riak node will be shut down. This may need to be increased as more data is indexed and Solr takes longer to start. Values lower than <tt>1s</tt> will be rounded up to the minimum <tt>1s</tt>.</td>
<td><tt>30s</tt></td>
</tr>

</tbody>
</table>

## SNMP

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>snmp.nodePutTime100Threshold</tt></td>
<td>Maximum PUT time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodePutTime99Threshold</tt></td>
<td>99th percentile PUT time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodePutTime95Threshold</tt></td>
<td>95th percentile PUT time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodePutTimeMedianThreshold</tt></td>
<td>Median PUT time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodePutTimeMeanThreshold</tt></td>
<td>Mean PUT time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodeGetTime100Threshold</tt></td>
<td>Maximum GET time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodeGetTime99Threshold</tt></td>
<td>99th percentile GET time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodeGetTime95Threshold</tt></td>
<td>95th percentile GET time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodeGetTimeMedianThreshold</tt></td>
<td>Median GET time</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.nodeGetTimeMeanThreshold</tt></td>
<td>The threshold for the SNMP gauge at which traps are sent. Set to <tt>off</tt> to disable traps for this gauge. When set to a positive integer in microseconds, the rising trap will be sent when the gauge crosses above the threshold, and the falling trap will be sent when the gauge crosses below the threshold. In the case of the <tt>nodeGetTimeMean</tt> gauge, the threshold is <tt>nodeGetTimeMeanThreshold</tt>, the rising trap is <tt>nodeGetTimeMeanAlarmRising</tt>, and the falling trap is <tt>nodeGetTimeMeanFalling</tt>. Other gauge thresholds follow this naming pattern.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.traps.replication</tt></td>
<td>Enable or disable traps for Multi-Datacenter Replication.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>snmp.refresh_frequency</tt></td>
<td>How often SNMP will refresh its counters out of Riak's internal stats.</td>
<td><tt>1m</tt></td>
</tr>

<tr>
<td><tt>snmp.database_dir</tt></td>
<td>The directory in which SNMP will store its internal database.</td>
<td><tt>./data/snmp/agent/db</tt></td>
</tr>

<tr>
<td><tt>snmp.force_reload</tt></td>
<td>Whether to force SNMP information to be repopulated on startup.</td>
<td><tt>on</tt></td>
</tr>

</tbody>
</table>

## JMX

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>jmx</tt></td>
<td>Turns on Java Management Extensions for Riak</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>jmx.refresh_rate</tt></td>
<td>How often to refresh stats</td>
<td><tt>30s</tt></td>
</tr>

<tr>
<td><tt>jmx.restart_check</tt></td>
<td>Time to wait between restarts of JMX. This is only for retrying JMX if the JMX server crashes.</td>
<td><tt>10m</tt></td>
</tr>

<tr>
<td><tt>jmx.port</tt></td>
<td>The port on which JMX will listen</td>
<td><tt>41110</tt></td>
</tr>

</tbody>
</table>

## LevelDB

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>leveldb.data_root</tt></td>
<td>The directory in which LevelDB will store its data.</td>
<td><tt>./data/leveldb</tt></td>
</tr>

<tr>
<td><tt>leveldb.total_mem_percent</tt></td>
<td>Defines the percentage (between 1 and 100) of total server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes as Riak activates/inactivates vnodes on this server to stay within this size.</td>
<td><tt>70</tt></td>
</tr>

<tr>
<td><tt>leveldb.compaction.trigger.tombstone_count</tt></td>
<td>Controls when a background compaction initiates solely due to the number of delete tombstones within an individual <tt>.sst</tt> table file.  Value of <tt>off</tt> disables the feature.</td>
<td><tt>1000</tt></td>
</tr>

<tr>
<td><tt>leveldb.fadvise_willneed</tt></td>
<td>Option to override LevelDB's use of <tt>fadvise(DONTNEED)</tt> with <tt>fadvise(WILLNEED)</tt> instead. <tt>WILLNEED</tt> can reduce disk activity on systems where physical memory exceeds the database size.</td>
<td><tt>false</tt></td>
</tr>

<tr>
<td><tt>leveldb.threads</tt></td>
<td>The number of worker threads performing LevelDB operations.</td>
<td><tt>71</tt></td>
</tr>

<tr>
<td><tt>leveldb.verify_compaction</tt></td>
<td>Enables or disables the verification of LevelDB data during compaction.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>leveldb.verify_checksums</tt></td>
<td>Enables or disables the verification of the data fetched from LevelDB against internal checksums.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>leveldb.block.size_steps</tt></td>
<td>Defines the number of incremental adjustments to attempt between the <tt>block.size</tt> value and the maximum <tt>block.size</tt> for an <tt>.sst</tt> table file. A value of zero disables the underlying dynamic <tt>block_size</tt> feature.</td>
<td><tt>16</tt></td>
</tr>

<tr>
<td><tt>leveldb.block.restart_interval</tt></td>
<td>Defines the key count threshold for a new key entry in the key index for a block. Most deployments should leave this parameter alone.</td>
<td><tt>16</tt></td>
</tr>

<tr>
<td><tt>leveldb.block.size</tt></td>
<td>Defines the size threshold for a block/chunk of data within one <tt>.sst</tt> table file. Each new block gets an index entry in the <tt>.sst</tt> table file's master index.</td>
<td><tt>4KB</tt></td>
</tr>

<tr>
<td><tt>leveldb.bloomfilter</tt></td>
<td>Each database <tt>.sst</tt> table file can include an optional "bloom filter" that is highly effective in shortcutting data queries that are destined to not find the requested key. The Bloom filter typically increases the size of an <tt>.sst</tt> table file by about 2%.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>leveldb.write_buffer_size_max</tt></td>
<td>See <tt>leveldb.write_buffer_size_min</tt> directly below.</td>
<td><tt>60MB</tt></td>
</tr>

<tr>
<td><tt>leveldb.write_buffer_size_min</tt></td>
<td>Each vnode first stores new key/value data in a memory-based write buffer. This write buffer is in parallel to the recovery log mentioned in the <tt>sync</tt> parameter. Riak creates each vnode with a randomly sized write buffer for performance reasons. The random size is somewhere between <tt>write_buffer_size_min</tt> and <tt>write_buffer_size_max</tt>.</td>
<td><tt>30MB</tt></td>
</tr>

<tr>
<td><tt>leveldb.limited_developer_mem</tt></td>
<td>This is a Riak-specific option that is used when a developer is testing a high number of vnodes and/or several VMs on a machine with limited physical memory. Do <em>not</em> use this option if making performance measurements.  This option overwrites values given to <tt>write_buffer_size_min</tt> and <tt>write_buffer_size_max</tt>.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>leveldb.sync_on_write</tt></td>
<td>Whether LevelDB will flush after every write. <strong>Note</strong>: If you are familiar with fsync, this is analagous to calling fsync after every write.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>leveldb.maximum_memory</tt></td>
<td>This parameter defines the number of bytes of server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes to stay within this size. The memory size can alternately be assigned as percentage of total server memory via <tt>leveldb.maximum_memory.percent</tt>.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>leveldb.maximum_memory.percent</tt></td>
<td>This parameter defines the percentage of total server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes to stay within this size. The memory size can alternately be assigned as a byte count via <tt>leveldb.maximum_memory</tt> instead.</td>
<td><tt>80</tt></td>
</tr>

</tbody>
</table>

## Bitcask

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>bitcask.data_root</tt></td>
<td>The directory under which Bitcask will store its data.</td>
<td><tt>./data/bitcask</tt></td>
</tr>

<tr>
<td><tt>bitcask.io_mode</tt></td>
<td>Configure how Bitcask writes data to disk. If set to <tt>erlang</tt>, writes are made via Erlang's built-in file API; if set to <tt>nif</tt>, writes are made via direct calls to the POSIX C API. The <tt>nif</tt> mode provides higher throughput for certain workloads, but has the potential to negatively impact the Erlang VM, leading to higher worst-case latencies and possible throughput collapse.</td>
<td><tt>erlang</tt></td>
</tr>

<tr>
<td><tt>bitcask.expiry.grace_time</tt></td>
<td>By default, Bitcask will trigger a merge whenever a data file contains an expired key. This may result in excessive merging under some usage patterns. To prevent this you can set the <tt>bitcask.expiry.grace_time</tt> option.  Bitcask will defer triggering a merge solely for key expiry by the configured number of seconds. Setting this to <tt>1h</tt> effectively limits each cask to merging for expiry once per hour.</td>
<td><tt>0</tt></td>
</tr>

<tr>
<td><tt>bitcask.hintfile_checksums</tt></td>
<td>Whether to allow the CRC to be present at the end of hintfiles. Setting this to <tt>allow_missing</tt> runs Bitcask in a backwards-compatible mode in which old hint files will still be accepted without CRC signatures.</td>
<td><tt>strict</tt></td>
</tr>

<tr>
<td><tt>bitcask.expiry</tt></td>
<td>By default, Bitcask keeps all of your data around. If your data has limited time value, or if for space reasons you need to purge data, you can set the <tt>expiry</tt> option. For example, if you need to purge data automatically after 1 day, set the value to <tt>1d</tt>. <tt>off</tt> disables automatic expiration</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>bitcask.fold.max_puts</tt></td>
<td>See the description for the <tt>bitcask.fold.max_age</tt> config directly below.</td>
<td><tt>0</tt></td>
</tr>

<tr>
<td><tt>bitcask.fold.max_age</tt></td>
<td>Fold keys thresholds will reuse the keydir if another fold was started less than <tt>fold.max_age</tt> ago and there were fewer than <tt>fold.max_puts</tt> updates. Otherwise, it will wait until all current fold keys complete and then start. Set either option to <tt>unlimited</tt> to disable.</td>
<td><tt>unlimited</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.thresholds.small_file</tt></td>
<td>Describes the minimum size a file must have to be excluded from the merge. Files smaller than the threshold will be included. Increasing the value will cause more files to be merged, whereas decreasing the value will cause fewer files to be merged.</td>
<td><tt>10MB</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.thresholds.dead_bytes</tt></td>
<td>Describes the minimum amount of data occupied by dead keys in a file to cause it to be included in the merge. Increasing the value will cause fewer files to be merged, whereas decreasing the value will cause more files to be merged.</td>
<td><tt>128MB</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.thresholds.fragmentation</tt></td>
<td>Describes which ratio of dead keys to total keys in a file will cause it to be included in the merge. The value of this setting is a percentage from 0 to 100. For example, if a data file contains 4 dead keys and 6 live keys, it will be included in the merge at the default ratio (which is 40). Increasing the value will cause fewer files to be merged, decreasing the value will cause more files to be merged.</td>
<td><tt>40</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.triggers.dead_bytes</tt></td>
<td>Describes how much data stored for dead keys in a single file will trigger merging. The value is in bytes. If a file meets or exceeds the trigger value for dead bytes, merge will be triggered. Increasing the value will cause merging to occur less often, whereas decreasing the value will cause merging to happen more often. When either of these constraints are met by any file in the directory, Bitcask will attempt to merge files.</td>
<td><tt>512MB</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.triggers.fragmentation</tt></td>
<td>Describes which ratio of dead keys to total keys in a file will trigger merging. The value of this setting is a percentage from 0 to 100. For example, if a data file contains 6 dead keys and 4 live keys, then merge will be triggered at the default setting. Increasing this value will cause merging to occur less often, whereas decreasing the value will cause merging to happen more often.</td>
<td><tt>60</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.window.end</tt></td>
<td>See the description of the <tt>bitcask.merge.policy</tt> config below.</td>
<td><tt>23</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.window.start</tt></td>
<td>See the description of the <tt>bitcask.merge.policy</tt> config below.</td>
<td><tt>0</tt></td>
</tr>

<tr>
<td><tt>bitcask.merge.policy</tt></td>
<td>Lets you specify when during the day merge operations are allowed to be triggered. Valid options are: <tt>always</tt>, meaning no restrictions; <tt>never</tt>, meaning that merging will never be attempted; and <tt>window</tt>, specifying the hours during which merging is permitted, where <tt>bitcask.merge.window.start</tt> and <tt>bitcask.merge.window.end</tt> are integers between 0 and 23. If merging has a significant impact on performance of your cluster, or your cluster has quiet periods in which little storage activity occurs, you may want to change this setting from the default.</td>
<td><tt>always</tt></td>
</tr>

<tr>
<td><tt>bitcask.max_file_size</tt></td>
<td>Describes the maximum permitted size for any single data file in the Bitcask directory. If a write causes the current file to exceed this size threshold then that file is closed, and a new file is opened for writes.</td>
<td><tt>2GB</tt></td>
</tr>

<tr>
<td><tt>bitcask.sync.interval</tt></td>
<td>See the description of the <tt>bitcask.sync.strategy</tt> directly below.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>bitcask.sync.strategy</tt></td>
<td>Changes the durability of writes by specifying when to synchronize data to disk. The default setting protects against data loss in the event of application failure (process death) but leaves open a small window in which data could be lost in the event of complete system failure (e.g. hardware, OS, or power). The default mode, <tt>none</tt>, writes data into operating system buffers which will be written to the disks when those buffers are flushed by the operating system. If the system fails, e.g. due power loss or crash, that data is lost before those buffers are flushed to stable storage. This is prevented by the setting <tt>o_sync</tt>, which forces the operating system to flush to stable storage at every write. The effect of flushing each write is better durability, however write throughput will suffer as each write will have to wait for the write to complete. Available sync strategies: <tt>none</tt>, which will let the operating system manage syncing writes; <tt>o_sync</tt>, which will uses the <tt>O_SYNC</tt> flag to force syncs on every write; and <tt>interval</tt>, by which will force Bitcask to sync every <tt>bitcask.sync.interval</tt> seconds.</td>
<td><tt>none</tt></td>
</tr>

<tr>
<td><tt>bitcask.open_timeout</tt></td>
<td>Specifies the maximum time Bitcask will block on startup while attempting to create or open the data directory. You generally need not change this value. If for some reason the timeout is exceeded on open you'll see a log message of the form <tt>Failed to start bitcask backend: .... </tt>. Only then should you consider a longer timeout.</td>
<td><tt>4s</tt></td>
</tr>

</tbody>
</table>

## Memory Backend

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>memory_backend.ttl</tt></td>
<td>Each value written will be written with this "time to live." Once that object's time is up, it will be deleted on the next read of its key. Minimum: <tt>1s</tt></td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>memory_backend.max_memory_per_vnode</tt></td>
<td>The maximum amount of memory consumed per vnode by the memory storage backend. Minimum: <tt>1MB</tt></td>
<td><tt></tt></td>
</tr>

</tbody>
</table>

## Multi Backend

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>multi_backend.$name.(existing_setting)</tt></td>
<td>If you're using multiple backends, you can configure the backends individually by prepending the setting with <tt>multi_backend.$name</tt>. And so if you're using multiple backends and wish to set your LevelDB <tt>data_root</tt> parameter, for example, you would do so by setting <tt>multi_backend.leveldb.data_root</tt>. All backend-specific parameters can be set in this fashion when using multiple backends.</td>
<td></td>
</tr>

<tr>
<td><tt>multi_backend.$name.storage_backend</tt></td>
<td>This parameter specifies the Erlang module defining the storage mechanism that will be used on this node.</td>
<td><tt>bitcask</tt></td>
</tr>

<tr>
<td><tt>multi_backend.default</tt></td>
<td>The default name of a backend when one is not specified.</td>
<td><tt></tt></td>
</tr>

</tbody>
</table>

## Riak Control

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>riak_control</tt></td>
<td>Set to <tt>off</tt> to disable the admin panel.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>riak_control.auth.user.$username.password</tt></td>
<td>If Riak Control's authentication mode (<tt>riak_control.auth.mode</tt>) is set to <tt>userlist</tt>, then this is the list of usernames and passwords for access to the admin panel.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>riak_control.auth.mode</tt></td>
<td>Authentication mode used for access to the admin panel. Options are <tt>off</tt> (which is the default) or <tt>userlist</tt>.</td>
<td><tt>off</tt></td>
</tr>

</tbody>
</table>

## Runtime Health

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>runtime_health.triggers.distribution_port</tt></td>
<td>Whether distribution ports with full input buffers will be counted as busy. Distribution ports connect Riak nodes within a single cluster.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>runtime_health.triggers.port</tt></td>
<td>Whether ports with full input buffers will be counted as busy. Ports can represent open files or network sockets.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>runtime_health.triggers.process.heap_size</tt></td>
<td>A process will become busy when its heap exceeds this size (in bytes).</td>
<td><tt>160444000</tt></td>
</tr>

<tr>
<td><tt>runtime_health.triggers.process.garbage_collection</tt></td>
<td>A process will become busy when it exceeds this amount of time doing garbage collection. <strong>Note</strong>: Enabling this setting can cause performance problems on multi-core systems.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>runtime_health.thresholds.busy_ports</tt></td>
<td>The threshold at which a warning will be triggered about the number of ports that are overly busy. Ports with full input buffers count toward this threshold.</td>
<td><tt>2</tt></td>
</tr>

<tr>
<td><tt>runtime_health.thresholds.busy_processes</tt></td>
<td>The threshold at which to warn a warning will be triggered about the number of processes that are overly busy. Processes with large heaps or that take a long time to garbage collect will count toward this threshold.</td>
<td><tt>30</tt></td>
</tr>

</tbody>
</table>

## Default Bucket Properties

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>buckets.default.postcommit</tt></td>
<td>A space-delimited list of functions that will be run after a value is stored. Only Erlang functions are allowed, using the <tt>module:function</tt>format.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>buckets.default.precommit</tt></td>
<td>A space delimited list of functions that will be run before a value is stored, and that can abort the write. For Erlang functions, use <tt>module:function</tt>, and for JavaScript use <tt>functionName</tt>.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>buckets.default.last_write_wins</tt></td>
<td>Whether conflicting writes resolve via timestamp.</td>
<td><tt>false</tt></td>
</tr>

<tr>
<td><tt>buckets.default.allow_mult</tt></td>
<td>Whether or not siblings are allowed. <strong>Note</strong>: See <a href="/theory/concepts/Vector-Clocks">Vector Clocks</a> for a discussion of sibling resolution.</td>
<td><tt>true</tt></td>
</tr>

<tr>
<td><tt>buckets.default.basic_quorum</tt></td>
<td>Whether not-founds will invoke the "basic quorum" optimization. This setting will short-circuit fetches where the majority of replicas report that the key is not found. Only used when <tt>notfound_ok</tt> is set to <tt>false</tt>.</td>
<td><tt>false</tt></td>
</tr>

<tr>
<td><tt>buckets.default.notfound_ok</tt></td>
<td>Whether not-founds will count toward a quorum of reads.</td>
<td><tt>true</tt></td>
</tr>

<tr>
<td><tt>buckets.default.rw</tt></td>
<td>The number of replicas which must reply to a delete request.</td>
<td><tt>quorum</tt></td>
</tr>

<tr>
<td><tt>buckets.default.dw</tt></td>
<td>The number of replicas which must reply to a write request, indicating that the write was committed to durable storage.</td>
<td><tt>quorum</tt></td>
</tr>

<tr>
<td><tt>buckets.default.pw</tt></td>
<td>The number of primary, non-fallback replicas which must reply to a write request.</td>
<td><tt>0</tt></td>
</tr>

<tr>
<td><tt>buckets.default.w</tt></td>
<td>The number of replicas which must reply to a write request, indicating that the write was received.</td>
<td><tt>quorum</tt></td>
</tr>

<tr>
<td><tt>buckets.default.r</tt></td>
<td>The number of replicas which must reply to a read request.</td>
<td><tt>quorum</tt></td>
</tr>

<tr>
<td><tt>buckets.default.pr</tt></td>
<td>The number of primary, non-fallback replicas that must reply to a read request.</td>
<td><tt>0</tt></td>
</tr>

<tr>
<td><tt>buckets.default.n_val</tt></td>
<td>The number of replicas stored. <strong>Note</strong>: See <a href="/dev/advanced/cap-controls">Replication Properties</a> for further discussion.</td>
<td><tt>3</tt></td>
</tr>

</tbody>
</table>

## Object Settings

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>object.merge_strategy</tt></td>
<td>The strategy used when merging objects that potentially have conflicts. The default is <tt>2</tt> in Riak 2.0. This setting reduces sibling creation through additional metadata on each sibling (also known as dotted version vectors). Setting this to <tt>1</tt> is the default for Riak 1.4 and earlier, and may duplicate siblings that originated in the same write.</td>
<td><tt>2</tt></td>
</tr>

<tr>
<td><tt>object.siblings.maximum</tt></td>
<td>Writing an object with more than this number of siblings will send a failure to the client.</td>
<td><tt>100</tt></td>
</tr>

<tr>
<td><tt>object.siblings.warning_threshold</tt></td>
<td>Writing an object with more than this number of siblings will generate a warning in the logs.</td>
<td><tt>25</tt></td>
</tr>

<tr>
<td><tt>object.size.maximum</tt></td>
<td>Writing an object larger than this will send a failure to the client.</td>
<td><tt>50MB</tt></td>
</tr>

<tr>
<td><tt>object.size.warning_threshold</tt></td>
<td>Reading or writing objects larger than this size will write a warning in the logs.</td>
<td><tt>5MB</tt></td>
</tr>

<tr>
<td><tt>object.format</tt></td>
<td>Controls which binary representation of a riak value is stored on disk. Options are <tt>0</tt>, which will use the original <tt>erlang:term_to_binary</tt> format but has a higher space overhead, or <tt>1</tt>, which will tell Riak to utilize a new format for more compact storage of small values.</td>
<td><tt>1</tt></td>
</tr>

</tbody>
</table>

## Erlang VM

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>erlang.distribution.port_range.maximum</tt></td>
<td>See the description for <tt>erlang.distribution.port_range.minimum</tt> directly below.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>erlang.distribution.port_range.minimum</tt></td>
<td>For ease of firewall configuration, the Erlang distribution can be bound to a limited range of TCP ports. If this parameter is set, and <tt>erlang.distribution.port_range.maximum</tt> is not set, only this port will be used. If the minimum is unset, no restriction will be made on the port range. Instead, Erlang will listen on a random high-numbered port. More information <a href="http://www.erlang.org/faq/how_do_i.html#id55090">here</a> and <a href="http://www.erlang.org/doc/man/kernel_app.html">here</a>.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>erlang.schedulers.force_wakeup_interval</tt></td>
<td>Set the scheduler forced wakeup interval. All run queues will be scanned each time period specified (in milliseconds). While there are sleeping schedulers in the system, one scheduler will be woken for each non-empty run queue found. An interval of zero disables this feature, which is the default. This feature is a workaround for lengthy executing native code, and native code that does not properly bump reductions. More information <a href="http://www.erlang.org/doc/man/erl.html#+sfwi">here</a>.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>erlang.distribution_buffer_size</tt></td>
<td>For nodes with many <tt>busy_dist_port</tt> events, Basho recommends raising the sender-side network distribution buffer size. 32MB may not be sufficient for some workloads and is a suggested starting point. Erlangers may know this as <tt>+zdbbl</tt>. See more <a href="http://www.erlang.org/doc/man/erl.html#%2bzdbbl">here</a>.</td>
<td><tt>32MB</tt></td>
</tr>

<tr>
<td><tt>erlang.process_limit</tt></td>
<td>Raises the default Erlang process limit</td>
<td><tt>256000</tt></td>
</tr>

<tr>
<td><tt>erlang.max_ets_tables</tt></td>
<td>Raises the ETS table limit</td>
<td><tt>256000</tt></td>
</tr>

<tr>
<td><tt>erlang.crash_dump</tt></td>
<td>Sets the location of crash dumps</td>
<td><tt>./log/erl_crash.dump</tt></td>
</tr>

<tr>
<td><tt>erlang.fullsweep_after</tt></td>
<td>A non-negative integer which indicates how many times generational garbage collections can be done without forcing a fullsweep collection. In low-memory systems (especially without virtual memory), setting the value to <tt>0</tt> can help to conserve memory. More information <a href="http://www.erlang.org/doc/man/erlang.html#system_flag-2">here</a>.</td>
<td><tt>0</tt></td>
</tr>

<tr>
<td><tt>erlang.max_ports</tt></td>
<td>The number of concurrent ports/sockets. The valid range is 1024 to 134217727.</td>
<td><tt>65536</tt></td>
</tr>

<tr>
<td><tt>erlang.async_threads</tt></td>
<td>Sets the number of threads in the async thread pool. The valid range is 0 to 1024. If thread support is available, the default is 64. More information <a href="http://erlang.org/doc/man/erl.html">here</a>.</td>
<td><tt>64</tt></td>
</tr>


<tr>
<td><tt>erlang.K</tt></td>
<td>Enables or disables the kernel poll functionality if the emulator supports it. If the emulator does not support kernel poll, and the <tt>K</tt> flag is passed to the emulator, a warning is issued at startup. Similar information <a href="http://erlang.org/doc/man/erl.html">here</a>.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>erlang.schedulers.online</tt></td>
<td>See the description for <tt>erlang.schedulers.total</tt> directly below.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>erlang.schedulers.total</tt></td>
<td>Sets the number of scheduler threads to create and scheduler threads to set online when <tt>erlang.smp</tt> support has been enabled. The maximum for both values is 1024. If the Erlang runtime system is able to determine the amount of logical processors configured and logical processors available, <tt>schedulers.total</tt> will default to logical processors configured, and <tt>schedulers.online</tt> will default to the number of logical processors available. Otherwise, the default values will be 1. Schedulers may be omitted if <tt>schedulers.online</tt> is not and vice versa. If <tt>schedulers.total</tt> or <tt>schedulers.online</tt> is specified as a negative number, the value is subtracted from the default number of logical processors configured or logical processors available, respectively. Specifying the value <tt>0</tt> for <tt>Schedulers</tt> or <tt>SchedulersOnline</tt> resets the number of scheduler threads or scheduler threads online respective to its default value. This option is ignored if the emulator doesn't have SMP support enabled (see the <tt>erlang.smp</tt> flag). More information <a href="http://erlang.org/doc/man/erl.html +S Schedulers:SchedulerOnline">here</a>.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>erlang.W</tt></td>
<td>Sets the mapping of warning messages for <tt>error_logger</tt>. Messages sent to the error logger using one of the warning routines can be mapped either to <tt>errors</tt>, warnings (<tt>w</tt>, which is the default), or info reports (<tt>i</tt>).</td>
<td><tt>w</tt></td>
</tr>

<tr>
<td><tt>erlang.smp</tt></td>
<td>Starts the Erlang runtime system with SMP support enabled. This may fail if no runtime system with SMP support is available. The <tt>auto</tt> setting starts the Erlang runtime system with SMP support enabled if it is available and more than one logical processor is detected. A value of <tt>disable</tt> starts a runtime system without SMP support. <strong>Note</strong>: The runtime system with SMP support will not be available on all supported platforms. See also the <tt>erlang.schedulers settings</tt>. Some native extensions (NIFs) require use of the SMP emulator. More information <a href="http://erlang.org/doc/man/erl.html">here</a>.</td>
<td><tt>enable</tt></td>
</tr>

<tr>
<td>erlang.shutdown_time</td>
<td>Useful when running a <tt>riak_test</tt> devrel</td>
<td>10s</td>
</tr>

</tbody>
</table>

## Node Metadata

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>nodename</tt></td>
<td>The name of the Erlang node</td>
<td><tt>riak@127.0.0.1</tt></td>
</tr>

<tr>
<td><tt>distributed_cookie</tt></td>
<td>Cookie for distributed node communication. All nodes in the same cluster should use the same cookie or they will not be able to communicate.</td>
<td><tt>riak</tt></td>
</tr>

</tbody>
</table>

## JavaScript MapReduce

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>javascript.source_dir</tt></td>
<td>A directory containing the Javascript source files which will be loaded by Riak when it initializes Javascript VMs.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>javascript.maximum_stack_size</tt></td>
<td>The maximum amount of thread stack memory to allocate to each JavaScript virtual machine.</td>
<td><tt>16MB</tt></td>
</tr>

<tr>
<td><tt>javascript.maximum_heap_size</tt></td>
<td>The maximum amount of memory allocated to each JavaScript virtual machine.</td>
<td><tt>8MB</tt></td>
</tr>

<tr>
<td><tt>javascript.hook_pool_size</tt></td>
<td>The number of JavaScript virtual machines available for executing pre-commit hook functions.</td>
<td><tt>2</tt></td>
</tr>

<tr>
<td><tt>javascript.reduce_pool_size</tt></td>
<td>The number of JavaScript virtual machines available for executing reduce functions.</td>
<td><tt>6</tt></td>
</tr>

<tr>
<td><tt>javascript.map_pool_size</tt></td>
<td>The number of JavaScript virtual machines available for executing map functions.</td>
<td><tt>8</tt></td>
</tr>

</tbody>
</table>

## Security

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>ssl.cacertfile</tt></td>
<td>The default signing authority location for HTTPS.</td>
<td><tt>#(platform_etc_dir)/cacertfile.pem</tt></td>
</tr>

<tr>
<td><tt>ssl.keyfile</tt></td>
<td>Default key location for HTTPS.</td>
<td><tt>#(platform_etc_dir)/key.pem</tt></td>
</tr>

<tr>
<td><tt>ssl.certfile</tt></td>
<td>Default cert location for HTTPS.</td>
<td><tt>#(platform_etc_dir)/cert.pem</tt></td>
</tr>

<tr>
<td><tt>secure_referer_check</tt></td>
<td>Measures were added to Riak 1.2 to counteract cross-site scripting and request-forgery attacks. Some reverse proxies cannot remove the <tt>Referer</tt> header and make serving data directly from Riak impossible. Turning this setting to <tt>off</tt> disables this security check.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>check_crl</tt></td>
<td>Whether to check the CRL of a client certificate. This defaults to <tt>on</tt> but some CAs may not maintain or define a CRL, so this can be disabled if no CRL is available.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>tls_protocols.sslv3</tt></td>
<td>Determine which SSL/TLS versions are allowed. By default, only TLS 1.2 is allowed, but other versions can be enabled if clients don't support the latest TLS standard. It is strongly recommended that SSLv3 not be enabled unless absolutely necessary. More than one protocol can be enabled at once. The <tt>tls_protocols</tt> parameters below can be used to turn different versions on and off.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>tls_protocols.tlsv1.2</tt></td>
<td></td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>tls_protocols.tlsv1.1</tt></td>
<td></td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>tls_protocols.tlsv1</tt></td>
<td></td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>honor_cipher_order</tt></td>
<td>Whether to prefer the order in which the server lists its ciphers. When set to <tt>off</tt>, the client's preferred cipher order dictates which cipher is chosen.</td>
<td><tt>on</tt></td>
</tr>

</tbody>
</table>

## Ring

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>ring.state_dir</tt></td>
<td>Default location of ringstate</td>
<td><tt>./data/ring</tt></td>
</tr>

<tr>
<td><tt>transfer_limit</tt></td>
<td>Number of concurrent node-to-node transfers allowed</td>
<td><tt>2</tt></td>
</tr>

<tr>
<td><tt>ring_size</tt></td>
<td>Number of partitions in the cluster (only valid when first creating the cluster). Must be a power of 2. The minimum is 8 and the maximum is 1024.</td>
<td><tt>64</tt></td>
</tr>

</tbody>
</table>

## Client Interfaces

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>protobuf.nagle</tt></td>
<td>Turns off Nagle's algorithm for Protocol Buffers connections. This is equivalent to setting the <tt>TCP_NODELAY</tt> option on the socket.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>protobuf.backlog</tt></td>
<td>The maximum length to which the queue of pending connections may grow. If set, it must be an integer greater than zero. If you anticipate a huge number of connections being initialized simultaneously, set this number higher.</td>
<td><tt>128</tt></td>
</tr>

<tr>
<td><tt>listener.protobuf.$name</tt></td>
<td>This is the IP address and TCP port to which the Riak Protocol Buffers interface will bind.</td>
<td><tt>{"127.0.0.1",8087}</tt></td>
</tr>

<tr>
<td><tt>listener.http.$name</tt></td>
<td>This is the IP address and TCP port to which the Riak HTTP interface will bind.</td>
<td><tt>{"127.0.0.1",8098}</tt></td>
</tr>

<tr>
<td><tt>listener.https.$name</tt></td>
<td>This is the IP address and TCP port to which the Riak HTTPS interface will bind.</td>
<td><tt></tt></td>
</tr>

</tbody>
</table>

## Lager

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>log.error.messages_per_second</tt></td>
<td>Maximum number of <tt>error_logger</tt> messages to handle in a second</td>
<td><tt>100</tt></td>
</tr>

<tr>
<td><tt>log.error.redirect</tt></td>
<td>Whether to redirect <tt>error_logger</tt> messages into lager.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>log.crash.rotation.keep</tt></td>
<td>The number of rotated crash logs to keep. When set to <tt>current</tt>, only the current open log file is kept. Otherwise, an integer can be specified.</td>
<td><tt>5</tt></td>
</tr>

<tr>
<td><tt>log.crash.rotation</tt></td>
<td>The schedule on which to rotate the crash log. More information <a href="https://github.com/basho/lager/blob/master/README.md#internal-log-rotation">here</a>.</td>
<td><tt>$D0</tt></td>
</tr>

<tr>
<td><tt>log.crash.size</tt></td>
<td>Maximum size of the crash log before it is rotated</td>
<td><tt>10MB</tt></td>
</tr>

<tr>
<td><tt>log.crash.maximum_message_size</tt></td>
<td>Maximum size of individual messages in the crash log</td>
<td><tt>64KB</tt></td>
</tr>

<tr>
<td><tt>log.crash.file</tt></td>
<td>If the crash log is enabled, the file where its messages will be written.</td>
<td><tt>./log/crash.log</tt></td>
</tr>

<tr>
<td><tt>log.crash</tt></td>
<td>Whether to enable the crash log.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>sasl</tt></td>
<td>Whether to enable Erlang's built-in error logger.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>log.syslog</tt></td>
<td>When set to <tt>on</tt>, enables log output to syslog.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>log.error.file</tt></td>
<td>The file where error messages will be logged.</td>
<td><tt>./log/error.log</tt></td>
</tr>

<tr>
<td><tt>log.console.file</tt></td>
<td>When <tt>log.console</tt> is set to <tt>file</tt> or <tt>both</tt>>, the file where console messages will be logged.</td>
<td><tt>./log/console.log</tt></td>
</tr>

<tr>
<td><tt>log.console.level</tt></td>
<td>The severity level of the console log.</td>
<td><tt>info</tt></td>
</tr>

<tr>
<td><tt>log.console</tt></td>
<td>Where to emit the default log messages (typically at <tt>info</tt> severity). Possible values: <tt>off</tt>, which disables console log messages; <tt>file</tt>, which specifies that log messages will be output to the file specified by <tt>log.console.file</tt>; <tt>console</tt>, which outputs messages to standard output (seen when using <tt>riak attach-direct</tt>); or <tt>both</tt>, which outputs messages both to the file specified in <tt>log.console.file</tt> and to standard out.</td>
<td><tt>file</tt></td>
</tr>

</tbody>
</table>

## Active Anti-Entropy

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>anti_entropy</tt></td>
<td>How Riak will repair out-of-sync keys. Some features require this to be set to <tt>active</tt>, including search. If set to <tt>active</tt>, out-of-sync keys will be repaired in the background; if set to <tt>passive</tt>, out-of-sync keys are only repaired on read; and if set to <tt>active-debug</tt>, verbose debugging information will be output.</td>
<td><tt>active</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.throttle</tt></td>
<td>Whether the distributed throttle for Active Anti-Entropy is enabled.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.throttle.$tier.mailbox_size</tt></td>
<td>Sets the throttling tiers for Active Anti-Entropy. Each tier is a minimum vnode mailbox size and a time-delay that the throttle should observe at that size and above. For example, <tt>anti_entropy.throttle.tier1.mailbox_size = 0</tt>, <tt>anti_entropy.throttle.tier1.delay = 0ms</tt>, <tt>anti_entropy.throttle.tier2.mailbox_size = 40</tt>, <tt>anti_entropy.throttle.tier2.delay = 5ms</tt>, etc. If configured, there must be a tier which includes a mailbox size of 0. Both <tt>.mailbox_size</tt> and <tt>.delay</tt> must be set for each tier.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>anti_entropy.throttle.$tier.delay</tt></td>
<td>See the description for <tt>anti_entropy.throttle.$tier.mailbox_size</tt> above.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>anti_entropy.bloomfilter</tt></td>
<td>Bloom filters are highly effective in shortcutting data queries that are destined to not find the requested key, though they tend to entail a small performance cost.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.max_open_files</tt></td>
<td></td>
<td><tt>20</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.write_buffer_size</tt></td>
<td>The LevelDB options used by Active Anti-Entropy to generate the LevelDB-backed on-disk hashtrees.</td>
<td><tt>4MB</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.data_dir</tt></td>
<td>The directory where AAE hash trees are stored.</td>
<td><tt>./data/anti_entropy</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.trigger_interval</tt></td>
<td>The tick determines how often the Active Anti-Entropy manager looks for work to do (building/expiring trees, triggering exchanges, etc). Lowering this value will speed up the rate at which all replicas are synced across the cluster. Increasing the value is not recommended.</td>
<td><tt>15s</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.concurrency_limit</tt></td>
<td>Limit how many Active Anti-Entropy exchanges or builds can happen concurrently.</td>
<td><tt>2</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.tree.expiry</tt></td>
<td>Determines how often hash trees are expired after being built. Periodically expiring a hash tree ensures that the on-disk hash tree data stays consistent with the actual K/V backend data. It also helps Riak identify silent disk failures and bit rot. However, expiration is not needed for normal Active Anti-Entropy operations and should be infrequent for performance reasons. The time is specified in milliseconds.</td>
<td><tt>1w</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.tree.build_limit.per_timespan</tt></td>
<td></td>
<td><tt>1h</tt></td>
</tr>

<tr>
<td><tt>anti_entropy.tree.build_limit.number</tt></td>
<td>Restrict how fast AAE can build hash trees. Building the tree for a given partition requires a full scan over that partition's data. Once built, trees stay built until they are expired. <tt>.number</tt> is the number of builds; <tt>.per_timespan</tt> is the amount of time in which that number of builds occurs.</td>
<td><tt>1</tt></td>
</tr>

</tbody>
</table>

## Intra-Cluster Handoff

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>handoff.ssl.certfile</tt></td>
<td>To encrypt <tt>riak_core</tt> intra-cluster data handoff traffic, uncomment this line and edit its path to an appropriate certfile and keyfile.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>handoff.ssl.keyfile</tt></td>
<td>The keyfile paired with the certfile specified in <tt>.certfile</tt>.</td>
<td><tt></tt></td>
</tr>

<tr>
<td><tt>handoff.port</tt></td>
<td>Specifies the TCP port that Riak uses for intra-cluster data handoff.</td>
<td><tt>8099</tt></td>
</tr>

</tbody>
</table>

## Strong Consistency

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>strong_consistency</tt></td>
<td>Enables the consensus subsystem. Set to <tt>on</tt> to enable the consensus subsystem used for strongly consistent Riak operations.</td>
<td><tt>off</tt></td>
</tr>

</tbody>
</table>

## Miscellaneous

<table class="riak-conf">
<thead>
<tr>
<th>Config</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>
<tbody>

<tr>
<td><tt>retry_put_coordinator_failure</tt></td>
<td>If forwarding to a replica-local coordinator on PUT fails, this setting will retry the operation when set to <tt>on</tt>, which is the default in Riak 2.0. It's recommended that this be set to <tt>off</tt> in Riak 1.x.</td>
<td><tt>on</tt></td>
</tr>

<tr>
<td><tt>dtrace</tt></td>
<td>Whether <a href="http://dtrace.org/blogs/">DTrace</a> is enabled. Do not enable unless your Erlang/OTP runtime is compiled to support DTrace, which is available in R15B01 (supported by the official source package) and in R14B04 via a custom repository and branch.</td>
<td><tt>off</tt></td>
</tr>

<tr>
<td><tt>max_concurrent_requests</tt></td>
<td>The maximum number of concurrent requests of each type (GET or PUT) that is allowed. Setting this value to <tt>infinite</tt> disables overload protection. The <tt>erlang.process_limit</tt> should be at least 3 times this setting.</td>
<td><tt>50000</tt></td>
</tr>

</tbody>
</table>