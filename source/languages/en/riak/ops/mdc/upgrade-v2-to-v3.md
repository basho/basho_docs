---
title: "Multi Data Center Replication: Upgrading from V2 to V3"
project: riak
header: riakee
version: 1.3.2+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-UpgradeV2toV3'
}
---

This guide walks you through the process of upgrading Riak Enterprise
Multi-Datacenter Replication from version 2 to version 3. Please read
the [[comparison guide|Multi Data Center Replication: Comparison]] for
terminology differences between versions.

## Upgrade Process

In all of the following examples, the source cluster is named `newyork`
while the sink cluster is named `boston`.

1. Ensure that all system backups have completed, are accurate, and are
   readily available.
2. If fullsync replication is periodically started via `cron`,
   disable it.
3. Stop fullsync replication on every cluster participating in MDC:
    
    ```bash
    riak-repl cancel-fullsync
    ```

4. If custom replication hooks are being used, make a note of each
   before upgrading. Version 2 hooks *are* compatible with version 3
   hooks but they may be useful to Basho Support in the event of an
   upgrade issue. See the documentation on [[hooks|Multi Data Center
   Replication: Hooks]] for more information.
5. If NAT was configured with version 2 replication, please review the
   documentation on [[NAT in version 3|Multi Data Center Replication v3
   With NAT]] before proceeding. A mapping from external host/IP to the
   internal IP address can be created using the `riak-repl nat-map`
   command:
    
    ```bash
    riak-repl nat-map add 50.16.238.120:5555 192.168.2.40
    ```

6. If SSL was configured for version 2 replication, please review the
   documentation for [[version 3 SSL|Multi Data Center Replication v3
   SSL]] before proceeding. While all SSL configuration parameters can
   retain the same values from version 2, they will need to be moved
   from the `riak-repl` section of `app.config` to the `riak-core`
   section:

    ```appconfig
    {ssl_enabled, true},
    {certfile, "/full/path/to/site1-cert.pem"},
    {keyfile, "/full/path/to/site1-key.pem"},
    {cacertdir, "/full/path/to/cacertsdir"}
    ```

7. Name the source and sink clusters that will be participating in
   replication. Remember that the source cluster is `newyork` while the
   sink cluster is `boston`.

    On the source cluster:

    ```bash
    riak-repl clustername newyork
    ```

    On the sink cluster:

    ```bash
    riak-repl clustername boston
    ```

    You can verify that the cluster names have been established on each
    cluster by issuing the `riak-repl clustername` command without
    parameters.

8. Connect the source cluster to the sink cluster (assuming the sink
   cluster is named *boston* and the `cluster_mgr` is running on port
   `9080`):

    ```bash
    riak-repl connect boston:9080
    ```

9. Ensure that cluster connections have been established:

    ```bash
    riak-repl connections
    ```

10. Enable bidirectional replication, which can be established between
    two clusters by connecting from the sink (`boston`) to the source
    (`newyork`). On any node in the *sink* cluster, connect to the
    source:

    ```bash
    riak-repl connect newyork:9080
    ```

11. Enable realtime replication. To begin queueing---but not yet
    replicating---objects on the source cluster for realtime
    replication:
    
    ```bash
	riak-repl realtime enable boston
    ```

	Running `riak-repl status` on any node of the source will show which
    sinks are currently enabled for realtime.

12. Start realtime replication from source to sink:

    ```bash
    riak-repl realtime start boston
    ```

	This will process any objects in the replication queue as well as
    any updated objects in the source cluster.

	Running `riak-repl status` on any node of the source will show which
    sinks are currently enabled and running for realtime via the
    `realtime_enabled` and `realtime_started` statistics. See our
    documentation on [[replication statistics|Multi Data Center
    Replication: Statistics]] for a full list of available statistics. 

13. Enable fullsync replication by preparing a source cluster for
    fullsync replication:
	
    ```bash
	riak-repl fullsync enable boston
    ```

	Running `riak-repl status` on any node of the source will show which
    sinks are currently enabled for realtime.

14. Start fullsync replication from source to sink:
	
    ```bash
    riak-repl fullsync start boston
    ```

	Running `riak-repl status` on any node of the source will show which
    sinks are currently enabled and running for fullsync via the
    `fullsync_enabled` and `fullsync_started` statistics. See our
    documentation on [[replication statistics|Multi Data Center
    Replication: Statistics]] for a full list of available statistics.

15. If you are using Riak CS Enterprise `proxy_get`, you can enable
    connections for version 3 replication are enabled at runtime using
    the following command:

    ```bash
	riak-repl proxy_get enable boston
    ```

	See our guides to version 3 [[operations|Multi Data Center 
    Replication v3 Operations]] and [[configuration|Multi Data Center
    Replication v3 Configuration]] for more information.

16. Remove existing listeners/sites from both source and sink clusters
    using the `del-listener` and `del-site` commands, as in the
    following examples:

    ```bash
    riak-repl del-site newyork
    riak-repl del-listener riak@10.0.1.156 10.0.1.156 9010
    ```

    Currently configured listeners and sites can be seen using
    `riak-repl status` on each node in the source and sink clusters.

    See the [[operations guide|Multi Data Center Replication:
    Operations]] guide for more information on the `del-listener` and
    `del-site` commands.

17. If you are using cron to schedule replication fullsyncs, update the
    `crontab` to use the version 3 fullsync syntax. For example, version
    2 replication is started with:

    ```bash
    riak-repl start-fullsync
    ```

    Version 3 replication is started with the following (using `boston`
    as an example sink cluster name):

    ```bash
    riak-repl fullsync start boston
    ```

    See the guide to [[scheduling version 3 fullsync|Multi Data Center
    Replication: v3 Scheduling Fullsync]] for information on configuring
    scheduled replication fullsyncs.

18. Disable version 2 replication bucket hooks. By default,
    Riak 1.3.+ has replication bucket hooks enabled for both version 2
    and version 3. The version 2 replication bucket hook can be
    disabled. This step is not required, but leaving the version 2
    replication bucket hook enabled can cause inaccurate
    `objects_dropped_no_leader` and `objects_dropped_no_clients`
    statistics.

	To disable the version 2 replication bucket hook:

    ```bash
    riak-repl modes mode_repl13
    ```

	Example:

    ```bash
    riak-repl modes
    ```

    Output:

    ```
    Current replication modes: [mode_repl12,mode_repl13]
    ```

    Another example:

    ```
    riak-repl modes mode_repl13
    ```

    Output:

    ```
    Current replication modes: [mode_repl13]
    ```
