---
title: "Rolling Restarts"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Rolling Restarts"
    identifier: "repair_recover_restart"
    weight: 103
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/2.0.4/ops/running/recovery/rolling-restart
  - /riak/kv/2.0.4/ops/running/recovery/rolling-restart
---

Because Riak functions as a multi-node system, cluster-level [Riak version upgrades]({{<baseurl>}}riak/kv/2.0.4/setup/upgrading/cluster) and restarts can be performed on a node-by-node, "rolling" basis.

The following steps should be undertaken on each Riak node that you wish to restart:

1\. Stop Riak

```bash
riak stop
```

2\. Perform any necessary maintenance, upgrade, or other work in your cluster.

3\. Start Riak again

```bash
riak start
```

4\. Verify that the `riak_kv` service is once again available on the target node

```bash
riak-admin wait-for-service riak_kv <nodename>
```

If this responds with `riak_kv is up`, then the service is available and you can move on to the next step. Otherwise, the console will periodically return `riak_kv is not up` until the service is available.

5\. Verify that all in-progress handoffs have been completed

```bash
riak-admin transfers
```

If this responds with `No transfers active`, then all handoffs are complete. You can either run this command periodically until no more transfers are active or run the following script, which will run the `riak-admin transfers` command every 5 seconds until the transfers are complete:

```bash
while ! riak-admin transfers | grep -iqF 'No transfers active'
do
    echo 'Transfers in progress'
    sleep 5
done
```

6\. Repeat the above process for any other nodes that need to be restarted.
