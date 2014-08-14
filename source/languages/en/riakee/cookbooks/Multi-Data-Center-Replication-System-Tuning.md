---
title: "Multi Data Center Replication: System Tuning"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, os]
---

Depending on the size of your objects and your replication latency needs, you may need to configure your kernel settings to optimize throughput.

## Linux

Refer to our documentation on [[Linux Performance Tuning|System Performance Tuning]].

## Solaris

On Solaris, the following settings are suggested.

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
