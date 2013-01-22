---
title: "Multi Data Center Replication: System Tuning"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, os]
---

Depending on the size of your objects, and your replication latency needs, you may need to configure your kernel settings to optimize throughput.

## Linux
The following settings in `/etc/sysctl.conf` on Linux have provided very good throughput and latency. These settings are especially helpful when you have more than ~40ms network latency.

    net.ipv4.tcp_rmem = 10240 87380 12582912
    net.ipv4.tcp_wmem = 10240 87380 12582912
    net.ipv4.tcp_window_scaling = 1
    net.ipv4.tcp_timestamps = 1
    net.ipv4.tcp_sack = 1
    net.core.netdev_max_backlog = 5000

To reload these settings at runtime:

```
sysctl -p /etc/sysctl.conf
``` 

## Solaris
On Solaris the following settings are suggested.

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
