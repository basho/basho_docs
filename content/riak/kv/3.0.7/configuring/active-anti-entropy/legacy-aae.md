---
title_supertext: "Configuring:"
title: "Legacy Active Anti-Entropy"
description: ""
project: "riak_kv"
project_version: "3.0.7"
lastmod: 2021-07-17T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.7:
    name: "Legacy AAE"
    identifier: "configuring_legacy_aae"
    weight: 103
    parent: "configuring-active-anti-entropy"
toc: true
version_history:
  in: "2.9.0p5+"
since: 2.9.0p5
aliases:
---

The configuration for the legacy AAE is kept in
 the `riak.conf` configuration file.

## Validate Settings

Once your configuration is set, you can verify its correctness by
running the `riak` command-line tool:

```bash
riak chkconfig
```

## riak.conf Settings

Configurable parameters for Riak's legacy active anti-entropy subsystem.

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
<td>How Riak will repair out-of-sync keys. If set to
<code>active</code>, out-of-sync keys will be repaired in the
background; if set to <code>passive</code>, out-of-sync keys are only
repaired on read; and if set to <code>active-debug</code>, verbose
debugging information will be output.</td>
<td><code>active</code></td>
</tr>

<tr>
<td><code>anti_entropy.bloomfilter</code></td>
<td>Bloom filters are highly effective in shortcutting data queries
that are destined to not find the requested key, though they tend to
entail a small performance cost.</td>
<td><code>on</code></td>
</tr>

<tr>
<td><code>anti_entropy.max_open_files</code></td>
<td></td>
<td><code>20</code></td>
</tr>

<tr>
<td><code>anti_entropy.write_buffer_size</code></td>
<td>The LevelDB options used by Active Anti-Entropy to generate the
LevelDB-backed on-disk hashtrees.</td>
<td><code>4MB</code></td>
</tr>

<tr>
<td><code>anti_entropy.data_dir</code></td>
<td>The directory where AAE hash trees are stored.</td>
<td><code>./data/anti_entropy</code></td>
</tr>

<tr>
<td><code>anti_entropy.trigger_interval</code></td>
<td>The tick determines how often the Active Anti-Entropy manager looks
for work to do (building/expiring trees, triggering exchanges, etc).
Lowering this value will speed up the rate at which all replicas are
synced across the cluster. Increasing the value is not recommended.
</td>
<td><code>15s</code></td>
</tr>

<tr>
<td><code>anti_entropy.concurrency_limit</code></td>
<td>Limit how many Active Anti-Entropy exchanges or builds can happen
concurrently.</td>
<td><code>2</code></td>
</tr>

<tr>
<td><code>anti_entropy.tree.expiry</code></td>
<td>Determines how often hash trees are expired after being built.
Periodically expiring a hash tree ensures that the on-disk hash tree
data stays consistent with the actual K/V backend data. It also helps
Riak identify silent disk failures and bit rot. However, expiration is
not needed for normal active anti-entropy operations and should be
infrequent for performance reasons. The time is specified in
milliseconds.</td>
<td><code>1w</code></td>
</tr>

<tr>
<td><code>anti_entropy.tree.build_limit.per_timespan</code></td>
<td></td>
<td><code>1h</code></td>
</tr>

<tr>
<td><code>anti_entropy.tree.build_limit.number</code></td>
<td>Restrict how fast AAE can build hash trees. Building the tree for a
given partition requires a full scan over that partition's data. Once
built, trees stay built until they are expired. <code>.number</code> is
the number of builds; <code>.per_timespan</code> is the amount of time
in which that number of builds occurs.</td>
<td><code>1</code></td>
</tr>

<tr>
<td><code>anti_entropy.use_background_manager</code></td>
<td>Whether AAE is to use a background process to limit AAE tree
rebuilds. If set to <code>on</code>, this will help to prevent system
response degradation under times of heavy load from multiple background
tasks that contend for the same system resources; setting this parameter
to <code>off</code> can cut down on system resource usage.
</td>
<td><code>off</code></td>
</tr>

</tbody>
</table>
