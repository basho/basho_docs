---
title_supertext: "Multi Data Center Replication:"
title: "System Tuning"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Multi-Datacenter Replication"
    identifier: "performance_multi_datacenter_tuning"
    weight: 110
    parent: "managing_performance"
toc: true
commercial_offering: true
aliases:
---

[perf index]: {{<baseurl>}}riak/kv/2.9.8/using/performance

Depending on the size of your objects and your replication latency
needs, you may need to configure your kernel settings to optimize
throughput.

## Linux

Refer to the [System Performance Tuning][perf index] document.

## Solaris

On Solaris, the following settings are suggested:

```bash
/usr/sbin/ndd -set /dev/tcp tcp_ip_abort_interval 60000
/usr/sbin/ndd -set /dev/tcp tcp_keepalive_interval 900000
/usr/sbin/ndd -set /dev/tcp tcp_rexmit_interval_initial 3000
/usr/sbin/ndd -set /dev/tcp tcp_rexmit_interval_max 10000
/usr/sbin/ndd -set /dev/tcp tcp_rexmit_interval_min 3000
/usr/sbin/ndd -set /dev/tcp tcp_time_wait_interval 60000
/usr/sbin/ndd -set /dev/tcp tcp_max_buf 4000000
/usr/sbin/ndd -set /dev/tcp tcp_cwnd_max 4000000
/usr/sbin/ndd -set /dev/tcp tcp_xmit_hiwat 4000000
/usr/sbin/ndd -set /dev/tcp tcp_recv_hiwat 4000000
```




