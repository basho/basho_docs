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
<td><code>storage_backend</code></td>
<td>Specifies the storage engine used for Riak's key-value data and secondary indexes (if supported).</td>
<td><code>bitcask</code></td>
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
<td><code>platform_log_dir</code></td>
<td></td>
<td><code>./log</code></td>
</tr>

<tr>
<td><code>platform_lib_dir</code></td>
<td></td>
<td><code>./lib</code></td>
</tr>

<tr>
<td><code>platform_etc_dir</code></td>
<td></td>
<td><code>./etc</code></td>
</tr>

<tr>
<td><code>platform_data_dir</code></td>
<td></td>
<td><code>./data</code></td>
</tr>

<tr>
<td><code>platform_bin_dir</code></td>
<td></td>
<td><code>./bin</code></td>
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
<td><code>search</code></td>
<td>To enable Search, set this to <code>on</code>.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>search.root_dir</code></td>
<td>The root directory for Riak Search, under which index data and configuration is stored.</td>
<td><code>./data/yz</code></td>
</tr>

<tr>
<td><code>search.anti_entropy.data_dir</code></td>
<td>The directory in which Search's Active Anti-Entropy data files are stored</td>
<td><code>./data/yz_anti_entropy</code></td>
</tr>

<tr>
<td><code>search.solr.jvm_options</code></td>
<td>The options to pass to the Solr JVM. Non-standard options, i.e. <code>-XX</code>, may not be portable across JVM implementations. Example: <code>XX:+UseCompressedStrings</code></td>
<td><code>-Xms1g -Xmx1g -XX:+UseStringCache -XX:+UseCompressedOops</code></td>
</tr>

<tr>
<td><code>search.solr.jmx_port</code></td>
<td>The port to which Solr JMX binds. <strong>Note</strong>: Binds on every interface.</td>
<td><code>8985</code></td>
</tr>

<tr>
<td><code>search.solr.port</code></td>
<td>The port to which Solr binds. <strong>Note</strong>: Binds on every interface.</td>
<td><code>8093</code></td>
</tr>

<tr>
<td><code>search.solr.start_timeout</code></td>
<td>How long Riak will wait for Solr to start. The start sequence will be tried twice. If both attempts time out, the Riak node will be shut down. This may need to be increased as more data is indexed and Solr takes longer to start. Values lower than <code>1s</code> will be rounded up to the minimum <code>1s</code>.</td>
<td><code>30s</code></td>
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
<td><code>snmp.nodePutTime100Threshold</code></td>
<td>Maximum PUT time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodePutTime99Threshold</code></td>
<td>99th percentile PUT time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodePutTime95Threshold</code></td>
<td>95th percentile PUT time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodePutTimeMedianThreshold</code></td>
<td>Median PUT time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodePutTimeMeanThreshold</code></td>
<td>Mean PUT time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodeGetTime100Threshold</code></td>
<td>Maximum GET time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodeGetTime99Threshold</code></td>
<td>99th percentile GET time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodeGetTime95Threshold</code></td>
<td>95th percentile GET time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodeGetTimeMedianThreshold</code></td>
<td>Median GET time</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.nodeGetTimeMeanThreshold</code></td>
<td>The threshold for the SNMP gauge at which traps are sent. Set to <code>off</code> to disable traps for this gauge. When set to a positive integer in microseconds, the rising trap will be sent when the gauge crosses above the threshold, and the falling trap will be sent when the gauge crosses below the threshold. In the case of the <code>nodeGetTimeMean</code> gauge, the threshold is <code>nodeGetTimeMeanThreshold</code>, the rising trap is <code>nodeGetTimeMeanAlarmRising</code>, and the falling trap is <code>nodeGetTimeMeanFalling</code>. Other gauge thresholds follow this naming pattern.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.traps.replication</code></td>
<td>Enable or disable traps for Multi-Datacenter Replication.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>snmp.refresh_frequency</code></td>
<td>How often SNMP will refresh its counters out of Riak's internal stats.</td>
<td><code>1m</code></td>
</tr>

<tr>
<td><code>snmp.database_dir</code></td>
<td>The directory in which SNMP will store its internal database.</td>
<td><code>./data/snmp/agent/db</code></td>
</tr>

<tr>
<td><code>snmp.force_reload</code></td>
<td>Whether to force SNMP information to be repopulated on startup.</td>
<td><code>on</code></td>
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
<td><code>jmx</code></td>
<td>Turns on Java Management Extensions for Riak</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>jmx.refresh_rate</code></td>
<td>How often to refresh stats</td>
<td><code>30s</code></td>
</tr>

<tr>
<td><code>jmx.restart_check</code></td>
<td>Time to wait between restarts of JMX. This is only for retrying JMX if the JMX server crashes.</td>
<td><code>10m</code></td>
</tr>

<tr>
<td><code>jmx.port</code></td>
<td>The port on which JMX will listen</td>
<td><code>41110</code></td>
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
<td><code>leveldb.data_root</code></td>
<td>The directory in which LevelDB will store its data.</td>
<td><code>./data/leveldb</code></td>
</tr>

<tr>
<td><code>leveldb.total_mem_percent</code></td>
<td>Defines the percentage (between 1 and 100) of total server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes as Riak activates/inactivates vnodes on this server to stay within this size.</td>
<td><code>70</code></td>
</tr>

<tr>
<td><code>leveldb.compression</code></td>
<td>Enabling this setting (<code>on</code>), which is the default, saves disk space. Disabling it may reduce read latency but increase overall disk activity. This option can be changed at any time, but it will not impact data on disk until the next time a file requires compaction.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>leveldb.compaction.trigger.tombstone_count</code></td>
<td>Controls when a background compaction initiates solely due to the number of delete tombstones within an individual <code>.sst</code> table file.  Value of <code>off</code> disables the feature.</td>
<td><code>1000</code></td>
</tr>

<tr>
<td><code>leveldb.fadvise_willneed</code></td>
<td>Option to override LevelDB's use of <code>fadvise(DONTNEED)</code> with <code>fadvise(WILLNEED)</code> instead. <code>WILLNEED</code> can reduce disk activity on systems where physical memory exceeds the database size.</td>
<td><code>false</code></td>
</tr>

<tr>
<td><code>leveldb.threads</code></td>
<td>The number of worker threads performing LevelDB operations.</td>
<td><code>71</code></td>
</tr>

<tr>
<td><code>leveldb.verify_compaction</code></td>
<td>Enables or disables the verification of LevelDB data during compaction.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>leveldb.verify_checksums</code></td>
<td>Enables or disables the verification of the data fetched from LevelDB against internal checksums.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>leveldb.block_cache_threshold</code></td>
<td>This setting defines the limit past which block cache memory can no longer be released in favor of the page cache. This setting has no impact in favor of file cache. The value is set on a per-vnode basis.</td>
<td><code>32MB</code></td>
</tr>

<tr>
<td><code>leveldb.block.size_steps</code></td>
<td>Defines the number of incremental adjustments to attempt between the <code>block.size</code> value and the maximum <code>block.size</code> for an <code>.sst</code> table file. A value of zero disables the underlying dynamic <code>block_size</code> feature.</td>
<td><code>16</code></td>
</tr>

<tr>
<td><code>leveldb.block.restart_interval</code></td>
<td>Defines the key count threshold for a new key entry in the key index for a block. Most deployments should leave this parameter alone.</td>
<td><code>16</code></td>
</tr>

<tr>
<td><code>leveldb.block.size</code></td>
<td>Defines the size threshold for a block/chunk of data within one <code>.sst</code> table file. Each new block gets an index entry in the <code>.sst</code> table file's master index.</td>
<td><code>4KB</code></td>
</tr>

<tr>
<td><code>leveldb.bloomfilter</code></td>
<td>Each database <code>.sst</code> table file can include an optional "bloom filter" that is highly effective in shortcutting data queries that are destined to not find the requested key. The Bloom filter typically increases the size of an <code>.sst</code> table file by about 2%.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>leveldb.write_buffer_size_max</code></td>
<td>See <code>leveldb.write_buffer_size_min</code> directly below.</td>
<td><code>60MB</code></td>
</tr>

<tr>
<td><code>leveldb.write_buffer_size_min</code></td>
<td>Each vnode first stores new key/value data in a memory-based write buffer. This write buffer is in parallel to the recovery log mentioned in the <code>sync</code> parameter. Riak creates each vnode with a randomly sized write buffer for performance reasons. The random size is somewhere between <code>write_buffer_size_min</code> and <code>write_buffer_size_max</code>.</td>
<td><code>30MB</code></td>
</tr>

<tr>
<td><code>leveldb.limited_developer_mem</code></td>
<td>This is a Riak-specific option that is used when a developer is testing a high number of vnodes and/or several VMs on a machine with limited physical memory. Do <em>not</em> use this option if making performance measurements.  This option overwrites values given to <code>write_buffer_size_min</code> and <code>write_buffer_size_max</code>.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>leveldb.sync_on_write</code></td>
<td>Whether LevelDB will flush after every write. <strong>Note</strong>: If you are familiar with fsync, this is analagous to calling fsync after every write.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>leveldb.maximum_memory</code></td>
<td>This parameter defines the number of bytes of server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes to stay within this size. The memory size can alternately be assigned as percentage of total server memory via <code>leveldb.maximum_memory.percent</code>.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>leveldb.maximum_memory.percent</code></td>
<td>This parameter defines the percentage of total server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes to stay within this size. The memory size can alternately be assigned as a byte count via <code>leveldb.maximum_memory</code> instead.</td>
<td><code>80</code></td>
</tr>

<tr>
<td><code>leveldb.tiered</code></td>
<td>The level number at which LevelDB data switches from the faster to the slower array. The default of <code>0</code> disables the feature. More information can be found in the <a href="/ops/advanced/backends/leveldb">LevelDB</a> documentation.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>leveldb.tiered.path.fast</code></td>
<td>The path prefix for <code>.sst</code> files below the level set by <code>leveldb.tiered</code>.</td>
<td></td>
</tr>

<tr>
<td><code>leveldb.tiered.path.slow</code></td>
<td>The path prefix for <code>.sst</code> files below the level set by <code>leveldb.tiered</code>.</td>
<td></td>
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
<td><code>bitcask.data_root</code></td>
<td>The directory under which Bitcask will store its data.</td>
<td><code>./data/bitcask</code></td>
</tr>

<tr>
<td><code>bitcask.io_mode</code></td>
<td>Configure how Bitcask writes data to disk. If set to <code>erlang</code>, writes are made via Erlang's built-in file API; if set to <code>nif</code>, writes are made via direct calls to the POSIX C API. The <code>nif</code> mode provides higher throughput for certain workloads, but has the potential to negatively impact the Erlang VM, leading to higher worst-case latencies and possible throughput collapse.</td>
<td><code>erlang</code></td>
</tr>

<tr>
<td><code>bitcask.expiry.grace_time</code></td>
<td>By default, Bitcask will trigger a merge whenever a data file contains an expired key. This may result in excessive merging under some usage patterns. To prevent this you can set the <code>bitcask.expiry.grace_time</code> option.  Bitcask will defer triggering a merge solely for key expiry by the configured number of seconds. Setting this to <code>1h</code> effectively limits each cask to merging for expiry once per hour.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>bitcask.hintfile_checksums</code></td>
<td>Whether to allow the CRC to be present at the end of hintfiles. Setting this to <code>allow_missing</code> runs Bitcask in a backwards-compatible mode in which old hint files will still be accepted without CRC signatures.</td>
<td><code>strict</code></td>
</tr>

<tr>
<td><code>bitcask.expiry</code></td>
<td>By default, Bitcask keeps all of your data around. If your data has limited time value, or if for space reasons you need to purge data, you can set the <code>expiry</code> option. For example, if you need to purge data automatically after 1 day, set the value to <code>1d</code>. <code>off</code> disables automatic expiration</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>bitcask.fold.max_puts</code></td>
<td>See the description for the <code>bitcask.fold.max_age</code> config directly below.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>bitcask.fold.max_age</code></td>
<td>Fold keys thresholds will reuse the keydir if another fold was started less than <code>fold.max_age</code> ago and there were fewer than <code>fold.max_puts</code> updates. Otherwise, it will wait until all current fold keys complete and then start. Set either option to <code>unlimited</code> to disable.</td>
<td><code>unlimited</code></td>
</tr>

<tr>
<td><code>bitcask.merge.thresholds.small_file</code></td>
<td>Describes the minimum size a file must have to be excluded from the merge. Files smaller than the threshold will be included. Increasing the value will cause more files to be merged, whereas decreasing the value will cause fewer files to be merged.</td>
<td><code>10MB</code></td>
</tr>

<tr>
<td><code>bitcask.merge.thresholds.dead_bytes</code></td>
<td>Describes the minimum amount of data occupied by dead keys in a file to cause it to be included in the merge. Increasing the value will cause fewer files to be merged, whereas decreasing the value will cause more files to be merged.</td>
<td><code>128MB</code></td>
</tr>

<tr>
<td><code>bitcask.merge.thresholds.fragmentation</code></td>
<td>Describes which ratio of dead keys to total keys in a file will cause it to be included in the merge. The value of this setting is a percentage from 0 to 100. For example, if a data file contains 4 dead keys and 6 live keys, it will be included in the merge at the default ratio (which is 40). Increasing the value will cause fewer files to be merged, decreasing the value will cause more files to be merged.</td>
<td><code>40</code></td>
</tr>

<tr>
<td><code>bitcask.merge.triggers.dead_bytes</code></td>
<td>Describes how much data stored for dead keys in a single file will trigger merging. The value is in bytes. If a file meets or exceeds the trigger value for dead bytes, merge will be triggered. Increasing the value will cause merging to occur less often, whereas decreasing the value will cause merging to happen more often. When either of these constraints are met by any file in the directory, Bitcask will attempt to merge files.</td>
<td><code>512MB</code></td>
</tr>

<tr>
<td><code>bitcask.merge.triggers.fragmentation</code></td>
<td>Describes which ratio of dead keys to total keys in a file will trigger merging. The value of this setting is a percentage from 0 to 100. For example, if a data file contains 6 dead keys and 4 live keys, then merge will be triggered at the default setting. Increasing this value will cause merging to occur less often, whereas decreasing the value will cause merging to happen more often.</td>
<td><code>60</code></td>
</tr>

<tr>
<td><code>bitcask.merge.window.end</code></td>
<td>See the description of the <code>bitcask.merge.policy</code> config below.</td>
<td><code>23</code></td>
</tr>

<tr>
<td><code>bitcask.merge.window.start</code></td>
<td>See the description of the <code>bitcask.merge.policy</code> config below.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>bitcask.merge.policy</code></td>
<td>Lets you specify when during the day merge operations are allowed to be triggered. Valid options are: <code>always</code>, meaning no restrictions; <code>never</code>, meaning that merging will never be attempted; and <code>window</code>, specifying the hours during which merging is permitted, where <code>bitcask.merge.window.start</code> and <code>bitcask.merge.window.end</code> are integers between 0 and 23. If merging has a significant impact on performance of your cluster, or your cluster has quiet periods in which little storage activity occurs, you may want to change this setting from the default.</td>
<td><code>always</code></td>
</tr>

<tr>
<td><code>bitcask.max_file_size</code></td>
<td>Describes the maximum permitted size for any single data file in the Bitcask directory. If a write causes the current file to exceed this size threshold then that file is closed, and a new file is opened for writes.</td>
<td><code>2GB</code></td>
</tr>

<tr>
<td><code>bitcask.sync.interval</code></td>
<td>See the description of the <code>bitcask.sync.strategy</code> directly below.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>bitcask.sync.strategy</code></td>
<td>Changes the durability of writes by specifying when to synchronize data to disk. The default setting protects against data loss in the event of application failure (process death) but leaves open a small window in which data could be lost in the event of complete system failure (e.g. hardware, OS, or power). The default mode, <code>none</code>, writes data into operating system buffers which will be written to the disks when those buffers are flushed by the operating system. If the system fails, e.g. due power loss or crash, that data is lost before those buffers are flushed to stable storage. This is prevented by the setting <code>o_sync</code>, which forces the operating system to flush to stable storage at every write. The effect of flushing each write is better durability, however write throughput will suffer as each write will have to wait for the write to complete. Available sync strategies: <code>none</code>, which will let the operating system manage syncing writes; <code>o_sync</code>, which will uses the <code>O_SYNC</code> flag to force syncs on every write; and <code>interval</code>, by which will force Bitcask to sync every <code>bitcask.sync.interval</code> seconds.</td>
<td><code>none</code></td>
</tr>

<tr>
<td><code>bitcask.open_timeout</code></td>
<td>Specifies the maximum time Bitcask will block on startup while attempting to create or open the data directory. You generally need not change this value. If for some reason the timeout is exceeded on open you'll see a log message of the form <code>Failed to start bitcask backend: .... </code>. Only then should you consider a longer timeout.</td>
<td><code>4s</code></td>
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
<td><code>memory_backend.ttl</code></td>
<td>Each value written will be written with this "time to live." Once that object's time is up, it will be deleted on the next read of its key. Minimum: <code>1s</code></td>
<td><code></code></td>
</tr>

<tr>
<td><code>memory_backend.max_memory_per_vnode</code></td>
<td>The maximum amount of memory consumed per vnode by the memory storage backend. Minimum: <code>1MB</code></td>
<td><code></code></td>
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
<td><code>multi_backend.$name.(existing_setting)</code></td>
<td>If you're using multiple backends, you can configure the backends individually by prepending the setting with <code>multi_backend.$name</code>. And so if you're using multiple backends and wish to set your LevelDB <code>data_root</code> parameter, for example, you would do so by setting <code>multi_backend.leveldb.data_root</code>. All backend-specific parameters can be set in this fashion when using multiple backends.</td>
<td></td>
</tr>

<tr>
<td><code>multi_backend.$name.storage_backend</code></td>
<td>This parameter specifies the Erlang module defining the storage mechanism that will be used on this node.</td>
<td><code>bitcask</code></td>
</tr>

<tr>
<td><code>multi_backend.default</code></td>
<td>The default name of a backend when one is not specified.</td>
<td><code></code></td>
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
<td><code>riak_control</code></td>
<td>Set to <code>off</code> to disable the admin panel.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>riak_control.auth.user.$username.password</code></td>
<td>If Riak Control's authentication mode (<code>riak_control.auth.mode</code>) is set to <code>userlist</code>, then this is the list of usernames and passwords for access to the admin panel.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>riak_control.auth.mode</code></td>
<td>Authentication mode used for access to the admin panel. Options are <code>off</code> (which is the default) or <code>userlist</code>.</td>
<td><code>off</code></td>
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
<td><code>runtime_health.triggers.distribution_port</code></td>
<td>Whether distribution ports with full input buffers will be counted as busy. Distribution ports connect Riak nodes within a single cluster.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>runtime_health.triggers.port</code></td>
<td>Whether ports with full input buffers will be counted as busy. Ports can represent open files or network sockets.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>runtime_health.triggers.process.heap_size</code></td>
<td>A process will become busy when its heap exceeds this size (in bytes).</td>
<td><code>160444000</code></td>
</tr>

<tr>
<td><code>runtime_health.triggers.process.garbage_collection</code></td>
<td>A process will become busy when it exceeds this amount of time doing garbage collection. <strong>Note</strong>: Enabling this setting can cause performance problems on multi-core systems.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>runtime_health.thresholds.busy_ports</code></td>
<td>The threshold at which a warning will be triggered about the number of ports that are overly busy. Ports with full input buffers count toward this threshold.</td>
<td><code>2</code></td>
</tr>

<tr>
<td><code>runtime_health.thresholds.busy_processes</code></td>
<td>The threshold at which to warn a warning will be triggered about the number of processes that are overly busy. Processes with large heaps or that take a long time to garbage collect will count toward this threshold.</td>
<td><code>30</code></td>
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
<td><code>buckets.default.postcommit</code></td>
<td>A space-delimited list of functions that will be run after a value is stored. Only Erlang functions are allowed, using the <code>module:function</code>format.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>buckets.default.precommit</code></td>
<td>A space delimited list of functions that will be run before a value is stored, and that can abort the write. For Erlang functions, use <code>module:function</code>, and for JavaScript use <code>functionName</code>.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>buckets.default.last_write_wins</code></td>
<td>Whether conflicting writes resolve via timestamp.</td>
<td><code>false</code></td>
</tr>

<tr>
<td><code>buckets.default.allow_mult</code></td>
<td>Whether or not siblings are allowed. <strong>Note</strong>: See <a href="/theory/concepts/Vector-Clocks">Vector Clocks</a> for a discussion of sibling resolution.</td>
<td><code>true</code></td>
</tr>

<tr>
<td><code>buckets.default.basic_quorum</code></td>
<td>Whether not-founds will invoke the "basic quorum" optimization. This setting will short-circuit fetches where the majority of replicas report that the key is not found. Only used when <code>notfound_ok</code> is set to <code>false</code>.</td>
<td><code>false</code></td>
</tr>

<tr>
<td><code>buckets.default.notfound_ok</code></td>
<td>Whether not-founds will count toward a quorum of reads.</td>
<td><code>true</code></td>
</tr>

<tr>
<td><code>buckets.default.rw</code></td>
<td>The number of replicas which must reply to a delete request.</td>
<td><code>quorum</code></td>
</tr>

<tr>
<td><code>buckets.default.dw</code></td>
<td>The number of replicas which must reply to a write request, indicating that the write was committed to durable storage.</td>
<td><code>quorum</code></td>
</tr>

<tr>
<td><code>buckets.default.pw</code></td>
<td>The number of primary, non-fallback replicas which must reply to a write request.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>buckets.default.w</code></td>
<td>The number of replicas which must reply to a write request, indicating that the write was received.</td>
<td><code>quorum</code></td>
</tr>

<tr>
<td><code>buckets.default.r</code></td>
<td>The number of replicas which must reply to a read request.</td>
<td><code>quorum</code></td>
</tr>

<tr>
<td><code>buckets.default.pr</code></td>
<td>The number of primary, non-fallback replicas that must reply to a read request.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>buckets.default.n_val</code></td>
<td>The number of replicas stored. <strong>Note</strong>: See <a href="/dev/advanced/cap-controls">Replication Properties</a> for further discussion.</td>
<td><code>3</code></td>
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
<td><code>object.merge_strategy</code></td>
<td>The strategy used when merging objects that potentially have conflicts. The default is <code>2</code> in Riak 2.0. This setting reduces sibling creation through additional metadata on each sibling (also known as dotted version vectors). Setting this to <code>1</code> is the default for Riak 1.4 and earlier, and may duplicate siblings that originated in the same write.</td>
<td><code>2</code></td>
</tr>

<tr>
<td><code>object.siblings.maximum</code></td>
<td>Writing an object with more than this number of siblings will send a failure to the client.</td>
<td><code>100</code></td>
</tr>

<tr>
<td><code>object.siblings.warning_threshold</code></td>
<td>Writing an object with more than this number of siblings will generate a warning in the logs.</td>
<td><code>25</code></td>
</tr>

<tr>
<td><code>object.size.maximum</code></td>
<td>Writing an object larger than this will send a failure to the client.</td>
<td><code>50MB</code></td>
</tr>

<tr>
<td><code>object.size.warning_threshold</code></td>
<td>Reading or writing objects larger than this size will write a warning in the logs.</td>
<td><code>5MB</code></td>
</tr>

<tr>
<td><code>object.format</code></td>
<td>Controls which binary representation of a riak value is stored on disk. Options are <code>0</code>, which will use the original <code>erlang:term_to_binary</code> format but has a higher space overhead, or <code>1</code>, which will tell Riak to utilize a new format for more compact storage of small values.</td>
<td><code>1</code></td>
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
<td><code>erlang.distribution.port_range.maximum</code></td>
<td>See the description for <code>erlang.distribution.port_range.minimum</code> directly below.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>erlang.distribution.port_range.minimum</code></td>
<td>For ease of firewall configuration, the Erlang distribution can be bound to a limited range of TCP ports. If this parameter is set, and <code>erlang.distribution.port_range.maximum</code> is not set, only this port will be used. If the minimum is unset, no restriction will be made on the port range. Instead, Erlang will listen on a random high-numbered port. More information <a href="http://www.erlang.org/faq/how_do_i.html#id55090">here</a> and <a href="http://www.erlang.org/doc/man/kernel_app.html">here</a>.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>erlang.schedulers.force_wakeup_interval</code></td>
<td>Set the scheduler forced wakeup interval. All run queues will be scanned each time period specified (in milliseconds). While there are sleeping schedulers in the system, one scheduler will be woken for each non-empty run queue found. An interval of zero disables this feature, which is the default. This feature is a workaround for lengthy executing native code, and native code that does not properly bump reductions. More information <a href="http://www.erlang.org/doc/man/erl.html#+sfwi">here</a>.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>erlang.distribution_buffer_size</code></td>
<td>For nodes with many <code>busy_dist_port</code> events, Basho recommends raising the sender-side network distribution buffer size. 32MB may not be sufficient for some workloads and is a suggested starting point. Erlangers may know this as <code>+zdbbl</code>. See more <a href="http://www.erlang.org/doc/man/erl.html#%2bzdbbl">here</a>.</td>
<td><code>32MB</code></td>
</tr>

<tr>
<td><code>erlang.process_limit</code></td>
<td>Raises the default Erlang process limit</td>
<td><code>256000</code></td>
</tr>

<tr>
<td><code>erlang.max_ets_tables</code></td>
<td>Raises the ETS table limit</td>
<td><code>256000</code></td>
</tr>

<tr>
<td><code>erlang.crash_dump</code></td>
<td>Sets the location of crash dumps</td>
<td><code>./log/erl_crash.dump</code></td>
</tr>

<tr>
<td><code>erlang.fullsweep_after</code></td>
<td>A non-negative integer which indicates how many times generational garbage collections can be done without forcing a fullsweep collection. In low-memory systems (especially without virtual memory), setting the value to <code>0</code> can help to conserve memory. More information <a href="http://www.erlang.org/doc/man/erlang.html#system_flag-2">here</a>.</td>
<td><code>0</code></td>
</tr>

<tr>
<td><code>erlang.max_ports</code></td>
<td>The number of concurrent ports/sockets. The valid range is 1024 to 134217727.</td>
<td><code>65536</code></td>
</tr>

<tr>
<td><code>erlang.async_threads</code></td>
<td>Sets the number of threads in the async thread pool. The valid range is 0 to 1024. If thread support is available, the default is 64. More information <a href="http://erlang.org/doc/man/erl.html">here</a>.</td>
<td><code>64</code></td>
</tr>


<tr>
<td><code>erlang.K</code></td>
<td>Enables or disables the kernel poll functionality if the emulator supports it. If the emulator does not support kernel poll, and the <code>K</code> flag is passed to the emulator, a warning is issued at startup. Similar information <a href="http://erlang.org/doc/man/erl.html">here</a>.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>erlang.schedulers.online</code></td>
<td>See the description for <code>erlang.schedulers.total</code> directly below.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>erlang.schedulers.total</code></td>
<td>Sets the number of scheduler threads to create and scheduler threads to set online when <code>erlang.smp</code> support has been enabled. The maximum for both values is 1024. If the Erlang runtime system is able to determine the amount of logical processors configured and logical processors available, <code>schedulers.total</code> will default to logical processors configured, and <code>schedulers.online</code> will default to the number of logical processors available. Otherwise, the default values will be 1. Schedulers may be omitted if <code>schedulers.online</code> is not and vice versa. If <code>schedulers.total</code> or <code>schedulers.online</code> is specified as a negative number, the value is subtracted from the default number of logical processors configured or logical processors available, respectively. Specifying the value <code>0</code> for <code>Schedulers</code> or <code>SchedulersOnline</code> resets the number of scheduler threads or scheduler threads online respective to its default value. This option is ignored if the emulator doesn't have SMP support enabled (see the <code>erlang.smp</code> flag). More information <a href="http://erlang.org/doc/man/erl.html +S Schedulers:SchedulerOnline">here</a>.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>erlang.W</code></td>
<td>Sets the mapping of warning messages for <code>error_logger</code>. Messages sent to the error logger using one of the warning routines can be mapped either to <code>errors</code>, warnings (<code>w</code>, which is the default), or info reports (<code>i</code>).</td>
<td><code>w</code></td>
</tr>

<tr>
<td><code>erlang.smp</code></td>
<td>Starts the Erlang runtime system with SMP support enabled. This may fail if no runtime system with SMP support is available. The <code>auto</code> setting starts the Erlang runtime system with SMP support enabled if it is available and more than one logical processor is detected. A value of <code>disable</code> starts a runtime system without SMP support. <strong>Note</strong>: The runtime system with SMP support will not be available on all supported platforms. See also the <code>erlang.schedulers settings</code>. Some native extensions (NIFs) require use of the SMP emulator. More information <a href="http://erlang.org/doc/man/erl.html">here</a>.</td>
<td><code>enable</code></td>
</tr>

<tr>
<td>erlang.shutdown_time</td>
<td>Useful when running a <code>riak_test</code> devrel</td>
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
<td><code>nodename</code></td>
<td>The name of the Erlang node</td>
<td><code>riak@127.0.0.1</code></td>
</tr>

<tr>
<td><code>distributed_cookie</code></td>
<td>Cookie for distributed node communication. All nodes in the same cluster should use the same cookie or they will not be able to communicate.</td>
<td><code>riak</code></td>
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
<td><code>javascript.source_dir</code></td>
<td>A directory containing the Javascript source files which will be loaded by Riak when it initializes Javascript VMs.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>javascript.maximum_stack_size</code></td>
<td>The maximum amount of thread stack memory to allocate to each JavaScript virtual machine.</td>
<td><code>16MB</code></td>
</tr>

<tr>
<td><code>javascript.maximum_heap_size</code></td>
<td>The maximum amount of memory allocated to each JavaScript virtual machine.</td>
<td><code>8MB</code></td>
</tr>

<tr>
<td><code>javascript.hook_pool_size</code></td>
<td>The number of JavaScript virtual machines available for executing pre-commit hook functions.</td>
<td><code>2</code></td>
</tr>

<tr>
<td><code>javascript.reduce_pool_size</code></td>
<td>The number of JavaScript virtual machines available for executing reduce functions.</td>
<td><code>6</code></td>
</tr>

<tr>
<td><code>javascript.map_pool_size</code></td>
<td>The number of JavaScript virtual machines available for executing map functions.</td>
<td><code>8</code></td>
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
<td><code>ssl.cacertfile</code></td>
<td>The default signing authority location for HTTPS.</td>
<td><code>#(platform_etc_dir)/cacertfile.pem</code></td>
</tr>

<tr>
<td><code>ssl.keyfile</code></td>
<td>Default key location for HTTPS.</td>
<td><code>#(platform_etc_dir)/key.pem</code></td>
</tr>

<tr>
<td><code>ssl.certfile</code></td>
<td>Default cert location for HTTPS.</td>
<td><code>#(platform_etc_dir)/cert.pem</code></td>
</tr>

<tr>
<td><code>secure_referer_check</code></td>
<td>Measures were added to Riak 1.2 to counteract cross-site scripting and request-forgery attacks. Some reverse proxies cannot remove the <code>Referer</code> header and make serving data directly from Riak impossible. Turning this setting to <code>off</code> disables this security check.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>check_crl</code></td>
<td>Whether to check the CRL of a client certificate. This defaults to <code>on</code> but some CAs may not maintain or define a CRL, so this can be disabled if no CRL is available.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>tls_protocols.sslv3</code></td>
<td>Determine which SSL/TLS versions are allowed. By default, only TLS 1.2 is allowed, but other versions can be enabled if clients don't support the latest TLS standard. It is strongly recommended that SSLv3 not be enabled unless absolutely necessary. More than one protocol can be enabled at once. The <code>tls_protocols</code> parameters below can be used to turn different versions on and off.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>tls_protocols.tlsv1.2</code></td>
<td></td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>tls_protocols.tlsv1.1</code></td>
<td></td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>tls_protocols.tlsv1</code></td>
<td></td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>honor_cipher_order</code></td>
<td>Whether to prefer the order in which the server lists its ciphers. When set to <code>off</code>, the client's preferred cipher order dictates which cipher is chosen.</td>
<td><code>on</code></td>
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
<td><code>ring.state_dir</code></td>
<td>Default location of ringstate</td>
<td><code>./data/ring</code></td>
</tr>

<tr>
<td><code>transfer_limit</code></td>
<td>Number of concurrent node-to-node transfers allowed</td>
<td><code>2</code></td>
</tr>

<tr>
<td><code>ring_size</code></td>
<td>Number of partitions in the cluster (only valid when first creating the cluster). Must be a power of 2. The minimum is 8 and the maximum is 1024.</td>
<td><code>64</code></td>
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
<td><code>protobuf.nagle</code></td>
<td>Turns off Nagle's algorithm for Protocol Buffers connections. This is equivalent to setting the <code>TCP_NODELAY</code> option on the socket.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>protobuf.backlog</code></td>
<td>The maximum length to which the queue of pending connections may grow. If set, it must be an integer greater than zero. If you anticipate a huge number of connections being initialized simultaneously, set this number higher.</td>
<td><code>128</code></td>
</tr>

<tr>
<td><code>listener.protobuf.$name</code></td>
<td>This is the IP address and TCP port to which the Riak Protocol Buffers interface will bind.</td>
<td><code>{"127.0.0.1",8087}</code></td>
</tr>

<tr>
<td><code>listener.http.$name</code></td>
<td>This is the IP address and TCP port to which the Riak HTTP interface will bind.</td>
<td><code>{"127.0.0.1",8098}</code></td>
</tr>

<tr>
<td><code>listener.https.$name</code></td>
<td>This is the IP address and TCP port to which the Riak HTTPS interface will bind.</td>
<td><code></code></td>
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
<td><code>log.error.messages_per_second</code></td>
<td>Maximum number of <code>error_logger</code> messages to handle in a second</td>
<td><code>100</code></td>
</tr>

<tr>
<td><code>log.error.redirect</code></td>
<td>Whether to redirect <code>error_logger</code> messages into lager.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>log.crash.rotation.keep</code></td>
<td>The number of rotated crash logs to keep. When set to <code>current</code>, only the current open log file is kept. Otherwise, an integer can be specified.</td>
<td><code>5</code></td>
</tr>

<tr>
<td><code>log.crash.rotation</code></td>
<td>The schedule on which to rotate the crash log. More information <a href="https://github.com/basho/lager/blob/master/README.md#internal-log-rotation">here</a>.</td>
<td><code>$D0</code></td>
</tr>

<tr>
<td><code>log.crash.size</code></td>
<td>Maximum size of the crash log before it is rotated</td>
<td><code>10MB</code></td>
</tr>

<tr>
<td><code>log.crash.maximum_message_size</code></td>
<td>Maximum size of individual messages in the crash log</td>
<td><code>64KB</code></td>
</tr>

<tr>
<td><code>log.crash.file</code></td>
<td>If the crash log is enabled, the file where its messages will be written.</td>
<td><code>./log/crash.log</code></td>
</tr>

<tr>
<td><code>log.crash</code></td>
<td>Whether to enable the crash log.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>sasl</code></td>
<td>Whether to enable Erlang's built-in error logger.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>log.syslog</code></td>
<td>When set to <code>on</code>, enables log output to syslog.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>log.error.file</code></td>
<td>The file where error messages will be logged.</td>
<td><code>./log/error.log</code></td>
</tr>

<tr>
<td><code>log.console.file</code></td>
<td>When <code>log.console</code> is set to <code>file</code> or <code>both</code>>, the file where console messages will be logged.</td>
<td><code>./log/console.log</code></td>
</tr>

<tr>
<td><code>log.console.level</code></td>
<td>The severity level of the console log.</td>
<td><code>info</code></td>
</tr>

<tr>
<td><code>log.console</code></td>
<td>Where to emit the default log messages (typically at <code>info</code> severity). Possible values: <code>off</code>, which disables console log messages; <code>file</code>, which specifies that log messages will be output to the file specified by <code>log.console.file</code>; <code>console</code>, which outputs messages to standard output (seen when using <code>riak attach-direct</code>); or <code>both</code>, which outputs messages both to the file specified in <code>log.console.file</code> and to standard out.</td>
<td><code>file</code></td>
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
<td><code>anti_entropy</code></td>
<td>How Riak will repair out-of-sync keys. Some features require this to be set to <code>active</code>, including search. If set to <code>active</code>, out-of-sync keys will be repaired in the background; if set to <code>passive</code>, out-of-sync keys are only repaired on read; and if set to <code>active-debug</code>, verbose debugging information will be output.</td>
<td><code>active</code></td>
</tr>

<tr>
<td><code>anti_entropy.throttle</code></td>
<td>Whether the distributed throttle for Active Anti-Entropy is enabled.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>anti_entropy.throttle.$tier.mailbox_size</code></td>
<td>Sets the throttling tiers for Active Anti-Entropy. Each tier is a minimum vnode mailbox size and a time-delay that the throttle should observe at that size and above. For example, <code>anti_entropy.throttle.tier1.mailbox_size = 0</code>, <code>anti_entropy.throttle.tier1.delay = 0ms</code>, <code>anti_entropy.throttle.tier2.mailbox_size = 40</code>, <code>anti_entropy.throttle.tier2.delay = 5ms</code>, etc. If configured, there must be a tier which includes a mailbox size of 0. Both <code>.mailbox_size</code> and <code>.delay</code> must be set for each tier.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>anti_entropy.throttle.$tier.delay</code></td>
<td>See the description for <code>anti_entropy.throttle.$tier.mailbox_size</code> above.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>anti_entropy.bloomfilter</code></td>
<td>Bloom filters are highly effective in shortcutting data queries that are destined to not find the requested key, though they tend to entail a small performance cost.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>anti_entropy.max_open_files</code></td>
<td></td>
<td><code>20</code></td>
</tr>

<tr>
<td><code>anti_entropy.write_buffer_size</code></td>
<td>The LevelDB options used by Active Anti-Entropy to generate the LevelDB-backed on-disk hashtrees.</td>
<td><code>4MB</code></td>
</tr>

<tr>
<td><code>anti_entropy.data_dir</code></td>
<td>The directory where AAE hash trees are stored.</td>
<td><code>./data/anti_entropy</code></td>
</tr>

<tr>
<td><code>anti_entropy.trigger_interval</code></td>
<td>The tick determines how often the Active Anti-Entropy manager looks for work to do (building/expiring trees, triggering exchanges, etc). Lowering this value will speed up the rate at which all replicas are synced across the cluster. Increasing the value is not recommended.</td>
<td><code>15s</code></td>
</tr>

<tr>
<td><code>anti_entropy.concurrency_limit</code></td>
<td>Limit how many Active Anti-Entropy exchanges or builds can happen concurrently.</td>
<td><code>2</code></td>
</tr>

<tr>
<td><code>anti_entropy.tree.expiry</code></td>
<td>Determines how often hash trees are expired after being built. Periodically expiring a hash tree ensures that the on-disk hash tree data stays consistent with the actual K/V backend data. It also helps Riak identify silent disk failures and bit rot. However, expiration is not needed for normal Active Anti-Entropy operations and should be infrequent for performance reasons. The time is specified in milliseconds.</td>
<td><code>1w</code></td>
</tr>

<tr>
<td><code>anti_entropy.tree.build_limit.per_timespan</code></td>
<td></td>
<td><code>1h</code></td>
</tr>

<tr>
<td><code>anti_entropy.tree.build_limit.number</code></td>
<td>Restrict how fast AAE can build hash trees. Building the tree for a given partition requires a full scan over that partition's data. Once built, trees stay built until they are expired. <code>.number</code> is the number of builds; <code>.per_timespan</code> is the amount of time in which that number of builds occurs.</td>
<td><code>1</code></td>
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
<td><code>handoff.ssl.certfile</code></td>
<td>To encrypt <code>riak_core</code> intra-cluster data handoff traffic, uncomment this line and edit its path to an appropriate certfile and keyfile.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>handoff.ssl.keyfile</code></td>
<td>The keyfile paired with the certfile specified in <code>.certfile</code>.</td>
<td><code></code></td>
</tr>

<tr>
<td><code>handoff.port</code></td>
<td>Specifies the TCP port that Riak uses for intra-cluster data handoff.</td>
<td><code>8099</code></td>
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
<td><code>strong_consistency</code></td>
<td>Enables the consensus subsystem. Set to <code>on</code> to enable the consensus subsystem used for strongly consistent Riak operations.</td>
<td><code>off</code></td>
</tr>

</tbody>
</table>

## Riak Data Types

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
<td><code>datatypes.compression_level</code></td>
<td>Whether serialized Data Types will use compression and at what level. When set to an integer, the parameter refers to the aggressiveness of compression, on a scale from 0 to 9. <code>on</code> is equivalent to 6, whereas <code>off</code> is equivalent to 0. Higher values for compression tend to be more CPU intensive.<td>
<td><code>1</code></td>
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
<td><code>metadata_cache_size</code></td>
<td>This setting controls the size of the metadata cache for each vnode. The cache can be disabled by setting it to <code>off</code> (this is the default). Enabling the cache should not be necessary in disk-based backends (i.e. LevelDB and Bitcask) but it can help performance in the Memory backend. Note that this setting adjusts the size of the ETS table rather than the actual data. Thus, more space may be used than the simple size * number-of-vnodes calculation would imply.<br /><br /><strong>Caution</strong>: This setting should not be changed without extensive benchmarking.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>retry_put_coordinator_failure</code></td>
<td>If forwarding to a replica-local coordinator on PUT fails, this setting will retry the operation when set to <code>on</code>, which is the default in Riak 2.0. It's recommended that this be set to <code>off</code> in Riak 1.x.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>dtrace</code></td>
<td>Whether <a href="http://dtrace.org/blogs/">DTrace</a> is enabled. Do not enable unless your Erlang/OTP runtime is compiled to support DTrace, which is available in R15B01 (supported by the official source package) and in R14B04 via a custom repository and branch.</td>
<td><code>off</code></td>
</tr>

<tr>
<td><code>max_concurrent_requests</code></td>
<td>The maximum number of concurrent requests of each type (GET or PUT) that is allowed. Setting this value to <code>infinite</code> disables overload protection. The <code>erlang.process_limit</code> should be at least 3 times this setting.</td>
<td><code>50000</code></td>
</tr>

</tbody>
</table>