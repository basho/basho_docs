---
title: "Multi Data Center Replication: Upgrading from V2 to V3"
project: riakee
version: 1.3.2+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl]
---

This guide walks through the process of upgrading Riak Enterprise MDC Replication from version 2 to version 3. Please read the [[Multi Data Center Replication UpgradeV2toV3]] guide for terminology differences between versions.

### Upgrade Process

1. Ensure all system backups have completed, are accurate, and are readily available.
2. If fullsync replication is periodically started via `cron`, disable it.
3. Stop fullsync replication on every cluster participating in MDC:

	`riak-repl cancel-fullsync`

4. Name the source and sink clusters that will be participating in replication. In the following examples, the source cluster will be named `newyork`, while the sink cluster will be named `boston`.
	
	On the source cluster:
	
	`riak-repl clustername neywork`
	
	On the sink cluster:
	
	`riak-repl clustername boston`

	You can verify the cluster names have been established on each cluster by issuing the `riak-repl clustername` command without parameters.

5. Connect the source cluster to the sink cluster. Assuming the sink cluster name *boston*:
  `riak-repl connect boston`

6. Ensure cluster connections have been established:
`riak-repl connections`

7. To establish bidirectional replication: 
	Bidirectional replication between two named clusters can be established by connecting from *sink* to *source*.

	On any node in the *sink* cluster:

	`riak-repl connect newyork`

8. Enable Realtime Replication
	To begin queuing objects (but not yet replicating) on the source cluster for realtime replication:
	`riak-repl realtime enable boston`

	Running `riak-repl status` on any node of the source will show which sinks are currently enabled for realtime.

9. Start realtime replication from source to sink:
`riak-repl realtime start boston`

	This will start process any objects in the replication queue, as well as any updated objects in the source cluster.

	Running `riak-repl status` on any node of the source will show which sinks are currently enabled and running for realtime via the `realtime_enabled` and `realtime_started` statistics. See the [[Multi Data Center Replication Statistics]] guide for a full list of available statistics. 

10. Enable Fullsync Replication
	To prepare a source cluster for fullsync replication:
	
	`riak-repl fullsync enable boston`

	Running `riak-repl status` on any node of the source will show which sinks are currently enabled for realtime.

11. Start fullsync replication from source to sink:
	`riak-repl fullsync start boston`

	Running `riak-repl status` on any node of the source will show which sinks are currently enabled and running for fullsync via the `fullsync_enabled` and `fullsync_started` statistics. See the [[Multi Data Center Replication Statistics]] guide for a full list of available statistics. 

12. Riak CS Enterprise proxy_get 

	If you are using Riak CS Enterprise proxy_get, connections for version 3 replication are enabled at runtime using the following command:

	`riak-repl proxy_get enable boston`
	
	See the [[Multi Data Center Replication v3 Operations]] and [[Multi Data Center Replication v3 Configuration]] guides for more information.

13. Remove existing listeners/sites
  
    Remove all configured listeners and sites from both source and sink clusters using the `del-listener` and `del-site` commands, like in the following examples:

    <ul>
    <li><code>riak-repl del-site newyork</code></li>
    <li><code>riak-repl del-listener riak@10.0.1.156 10.0.1.156 9010</code></li>
    </ul>

    Currently configured listeners and sites can be seen using `riak-repl status` on each node in the source and sink clusters.

    See the [[Multi Data Center Replication Operations]] guide for more information on `del-listener` and `del-site` commands.

14. Remove and/or update cron tasks

	If you are using `cron` to schedule replication fullsyncs, please update the crontab to use the version 3 fullsync syntax

	For example, version 2 replication is started with `riak-repl start-fullsync`, while version 3 replication is started with `riak-repl fullsync start boston` (using *boston* as an example sink cluster name).

	See the [[Multi Data Center Replication v3 Scheduling Full Sync]] guide for configuring scheduled replication fullsyncs.

15. Disable version 2 replication bucket hooks

	By default, Riak 1.3.+ has replication bucket hooks enabled for both version 2 and version 3. The version 2 replication bucket hook can be disabled. This step is not required, but leaving the version 2 replication bucket hook enabled can cause inaccurate `objects_dropped_no_leader` and `objects_dropped_no_clients` statistics.

	To disable the version 2 replication bucket hook:

	`riak-repl modes mode_repl13`

	Example:

    ```
    $riak-repl modes
    Current replication modes: [mode_repl12,mode_repl13]

    $riak-repl modes mode_repl13
    Current replication modes: [mode_repl13]
    ```

