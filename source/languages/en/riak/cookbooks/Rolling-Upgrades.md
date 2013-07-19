---
title: Rolling Upgrades
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [upgrading]
---

<!-- To avoid downtime of a Riak cluster we suggest performing upgrades in a rolling
fashion. This process involves stopping, upgrading, and restarting one node at a
time. This process is known to work as of Riak 0.13 (i.e. upgrading 0.13 or 0.14
to 1.0).
 -->
{{#1.4.0+}}
<div class="note"><div class="title">Note on upgrading to Riak 1.4+</div>
<p>Due to the differences between Riak 1.0 and Riak 1.4, you cannot upgrade directly. Instead, you should first upgrade to Riak 1.3.2, then to 1.4.0+</p>
<p>If you run riak_control you should disable it during rolling upgrade process.</p>
</div>
{{/1.4.0+}}

Riak nodes now negotiate with each other to determine supported operating modes. This allows clusters containing mixed-versions of Riak to properly interoperate without special configuration, and simplifies rolling upgrades.

In previous Riak versions, users were required to disable new features during the rolling upgrade process, and then enable them after all nodes were upgraded. This is now handled automatically by Riak.

<div class="note"><div class="title">Note on upgrading from Riak Search</div>
<p>If you are upgrading from Riak Search, please read
[[Upgrading from Riak Search]] before performing a rolling upgrade.</p>
</div>

{{#1.1.0-}}

<div class="note"><div class="title">Note on upgrading to Riak 1.0</div>
<p>Rolling upgrades should work when moving from Riak 0.13 or later to Riak 1.0
following the OS specific instructions below, but there are a few considerations
to keep in mind when doing so. Riak 1.0 has new features that add additional
steps to the rolling upgrade procedure, specifically Riak Pipe, the new data
processing library backing MapReduce, and the updated backend API supporting
asynchronous keylisting. If these features are not explicitly enabled after
upgrading, the legacy variant of the feature will be used instead. These
features can only be enabled once *all* nodes in the cluster have been upgraded
to 1.0.</p>

<p>Before starting an upgrade to 1.0 issue the following command on each
pre-1.0.0 node in the cluster to make the transfers command report correctly.
Use `riak attach`if you are not already on the riak console.</p>

```erlang
> riak_core_node_watcher:service_up(riak_pipe, self()).
```

<p>If you forget (or any of the pre-1.0.0 nodes are restarted) it is safe to
re-issue the command.</p>

<p>After upgrading to 1.0, make sure to follow steps 9 and 10 of the applicable
platform specific
instructions.</p>
</div>

{{/1.1.0-}}

## Debian/Ubuntu

The following example demonstrates upgrading a Riak node that has been installed
with the Debian packages provided by Basho.

1\. Stop Riak

```bash
riak stop
```


2\. Backup Riak etc and data directories

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```


3\. Upgrade Riak

```bash
sudo dpkg -i <riak_package_name>.deb
```


4\. Restart Riak

```bash
riak start
```


5\. Verify Riak is running the new version

```bash
riak-admin status
```


6\. Wait for the riak_kv service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```


* &lt;target_node&gt; is the node which you have just upgraded (e.g.
riak@192.168.1.11)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```


* While the node was offline other nodes may have accepted writes on it's
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

{{#1.3.1+}}
<div class="info"><div class="title">Note for Secondary Index users</div>
If you use Riak's Secondary Indexes and are upgrading from a version prior to
Riak version 1.3.1, you need to reformat the indexes using the 
[[riak-admin reformat-indexes|Command-Line-Tools - riak-admin#reformat-indexes]] command. More details about reformatting indexes are available
 in the
[release notes](https://github.com/basho/riak/blob/master/RELEASE-NOTES.md).
</div>
{{/1.3.1+}}

{{#1.1.0-}}

<div class="note">Only perform the following two steps if you are upgrading to
Riak 1.0 from an earlier release.
</div>

9\. Once all nodes have been upgraded, make the following additions to the
`app.config` file in /etc/riak on each node. First add to the `riak_kv` section:

```erlang
{legacy_keylisting, false},
{mapred_system, pipe},
{vnode_vclocks, true}
```

and then add to the `riak_core` section:

```erlang
{platform_data_dir, "/var/lib/riak"}
```

10\. Either run `riak stop` followed by `riak start` on all of the nodes in
the cluster or use `riak attach` on each node and execute the following
commands:

```erlang
> application:set_env(riak_kv, legacy_keylisting, false).
> application:set_env(riak_kv, mapred_system, pipe).
> application:set_env(riak_kv, vnode_vclocks, true).
```

{{/1.1.0-}}

## RHEL/CentOS

The following example demonstrates upgrading a Riak node that has been installed
with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak

```bash
riak stop
```


2\. Backup Riak etc and data directories

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```


3\. Upgrade Riak

```bash
sudo rpm -Uvh <riak_package_name>.rpm
```


4\. Restart Riak

```bash
riak start
```


5\. Verify Riak is running the new version

```bash
riak-admin status
```


6\. Wait for the riak_kv service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```


* &lt;target_node&gt; is the node which you have just upgraded (e.g.
riak@192.168.1.11)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```


* While the node was offline other nodes may have accepted writes on it's
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

{{#1.3.1+}}
<div class="info"><div class="title">Note for Secondary Index users</div>
If you use Riak's Secondary Indexes and are upgrading from a version prior to
Riak version 1.3.1, you need to reformat the indexes using the 
[[riak-admin reformat-indexes|Command-Line-Tools - riak-admin#reformat-indexes]] command. More details about reformatting indexes are available
 in the
[release notes](https://github.com/basho/riak/blob/master/RELEASE-NOTES.md).
</div>
{{/1.3.1+}}

{{#1.1.0-}}
<div class="note">Only perform the following two steps if you are upgrading to
Riak 1.0 from an earlier release.
</div>

9\. Once all nodes have been upgraded, add the following additions to the
`app.config` file in /etc/riak on each node. First add to the `riak_kv` section:

```erlang
{legacy_keylisting, false},
{mapred_system, pipe},
{vnode_vclocks, true}
```

and then add to the `riak_core` section:

```erlang
{platform_data_dir, "/var/lib/riak"}
```

10\. Either run `riak stop` followed by `riak start` on all of the nodes in
the cluster or use `riak attach` on each node and execute the following
commands:

```erlang
> application:set_env(riak_kv, legacy_keylisting, false).
> application:set_env(riak_kv, mapred_system, pipe).
> application:set_env(riak_kv, vnode_vclocks, true).
```

{{/1.1.0-}}

## Solaris/OpenSolaris

The following example demonstrates upgrading a Riak node that has been installed
with the Solaris/OpenSolaris packages provided by Basho.

1\. Stop Riak

```bash
riak stop
```



<div class="note">If you are using the service management facility (SMF) to
manage Riak you will have to stop Riak via "svcadm" instead of using "riak
stop":
<br /><br />
```bash
sudo svcadm disable riak
```
</div>


2\. Backup Riak etc and data directories

```bash
sudo gtar -czf riak_backup.tar.gz /opt/riak/data /opt/riak/etc
```


3\. Uninstall Riak

```bash
sudo pkgrm BASHOriak
```


4\. Install the new version of Riak

```bash
sudo pkgadd -d <riak_package_name>.pkg
```

{{#1.1.0-}}
<div class="note">If you are upgrading from Riak 0.12 you will have to restore
the etc directory from the backups made in step 2. The 0.12 package removes the
etc files when uninstalled.</div>
{{/1.1.0-}}

4\. Restart Riak

```bash
riak start
```

<div class="note">If you are using the SMF you should start Riak via "svcadm":
<br /><br />
```bash
sudo svcadm enable riak
```
</div>


5\. Verify Riak is running the new version

```bash
riak-admin status
```


6\. Wait for the riak_kv service to start

```bash
riak-admin wait-for-service riak_kv <target_node>
```


* &lt;target_node&gt; is the node which you have just upgraded (e.g.
riak@192.168.1.11)

7\. Wait for any hinted handoff transfers to complete

```bash
riak-admin transfers
```


* While the node was offline other nodes may have accepted writes on it's
behalf. This data is transferred to the node when it becomes available.

8\. Repeat the process for the remaining nodes in the cluster

{{#1.3.1+}}
<div class="info"><div class="title">Note for Secondary Index users</div>
If you use Riak's Secondary Indexes and are upgrading from a version prior to
Riak version 1.3.1, you need to reformat the indexes using the 
[[riak-admin reformat-indexes|Command-Line-Tools - riak-admin#reformat-indexes]] command. More details about reformatting indexes are available
 in the
[release notes](https://github.com/basho/riak/blob/master/RELEASE-NOTES.md).
</div>
{{/1.3.1+}}

{{#1.1.0-}}

<div class="note">Only perform the following two steps if you are upgrading to
Riak 1.0 from an earlier release.
</div>

9\. Once all nodes have been upgraded, add the following additions to the
`app.config` file in /etc/riak on each node. First add to the `riak_kv` section:

```erlang
{legacy_keylisting, false},
{mapred_system, pipe},
{vnode_vclocks, true}
```

and then add to the `riak_core` section:

```erlang
{platform_data_dir, "/opt/riak/data"}
```

10.\ Either run `riak stop` followed by `riak start` on all of the nodes in
the cluster or use `riak attach` on each node and execute the following
commands:

```erlang
> application:set_env(riak_kv, legacy_keylisting, false).
> application:set_env(riak_kv, mapred_system, pipe).
> application:set_env(riak_kv, vnode_vclocks, true).
```

{{/1.1.0-}}

## Basho Patches

After upgrading, you should ensure that any custom patches contained in the
`basho-patches` directory are examined to determine their application to
the upgraded version. If you find that patches no longer apply to the upgraded
version, you should remove them from the `basho-patches` directory prior to
operating the node in production.

The following table lists locations of the `basho-patches` directory for each
supported operating system:

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>CentOS &amp; RHEL Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/lib64/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Debian &amp; Ubuntu Linux</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/lib/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>FreeBSD</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/usr/local/lib/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>SmartOS</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/opt/local/lib/riak/lib/basho-patches</tt></p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Solaris 10</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p><tt>/opt/riak/lib/basho-patches</tt></p>
</td>
</tr>
</tbody>
</table>

{{#1.3.0+}}
## Riaknostic

It is a good idea to also verify some basic configuration and general health
of the Riak node after upgrading by using Riak's built-in diagnostic
utility *Riaknostic*.

Ensure that Riak is running on the node, and issue the following command:

```
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal node
operation.
{{/1.3.0+}}
