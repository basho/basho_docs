---
title: "Downgrading Riak TS"
description: "Downgrading from Riak TS 1.4.0 to 1.3.1"
menu:
  riak_ts-1.4.0:
    name: "Downgrade"
    identifier: "downgrade"
    weight: 300
    parent: "setup"
project: "riak_ts"
project_version: "1.4.0"
toc: true
version_history:
  in: "1.4.0+"
aliases:
    - /riakts/1.4.0/setup/downgrading/
    - /riakts/1.4.0/downgrading/
---

[ts upgrade]: {{<baseurl>}}riak/ts/1.4.0/setup/upgrading
[change riakconf]: #change-riak-conf-before-downgrade

Downgrades of Riak TS are tested and supported for two feature release
versions, with the general procedure similar to a
[rolling upgrade][ts upgrade].

## Debian/Ubuntu

The following example demonstrates downgrading a Riak TS node that has been
installed with the Debian/Ubuntu packages provided by Basho. You should perform the following actions on each node:

1\. Stop Riak TS:

```bash
riak stop
```

2\. Back up the Riak TS node's `/etc` and `/data` directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Downgrade Riak TS:

```bash
sudo dpkg -i »riakts_package_name«.deb
```

4\. Restart Riak TS:

```bash
riak start
```

5\. Verify Riak TS is running the downgraded version:

```bash
riak version
```

6\. Wait for the `riak_ts` service to start:

```bash
riak-admin wait-for-service riak_ts »target node«
```

* `»target node«` is the node which you have just downgraded (e.g.
`riak@192.168.1.11`)

7\. Repeat the process for the remaining nodes in the cluster.

## RHEL/CentOS

The following example demonstrates downgrading a Riak TS node that has been
installed with the RHEL/CentOS packages provided by Basho.

1\. Stop Riak TS:

```bash
riak stop
```

2\. Back up Riak TS's `/etc` and `/data` directories:

```bash
sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
```

3\. Downgrade Riak TS:

```bash
sudo rpm -Uvh --oldpackage »riakts_package_name«.rpm
```

4\. Restart Riak TS:

```bash
riak start
```

5\. Verify that Riak TS is running the downgraded version:

```bash
riak version
```

6\. Wait for the `riak_ts` service to start:

```bash
riak-admin wait-for-service riak_ts »target node«
```

* `»target node«` is the node which you have just downgraded (e.g.
riak@192.168.1.11)

7\. Repeat the process for the remaining nodes in the cluster.

## Change riak.conf For Downgrade

If you freshly installed Riak TS 1.4.0 and did NOT upgrade from 1.3.1, and then choose to downgrade to 1.3.1, you will need to change your riak.conf to preserve your configuration settings. Three TS-specific configuration parameters have been changed when defining riak.conf:

1.4.0 Name | 1.3.1 Name
:----------|:-------------|
`riak_kv.query.timeseries.timeout`|`timeseries_query_timeout_ms`
`riak_kv.query.timeseries.max_quanta_span`|`timeseries_query_max_quanta_span`
`riak_kv.query.timeseries.max_concurrent_queries`|`timeseries_max_concurrent_queries`

So, for example, the `riak_kv.query.timeseries.timeout` parameter would be changed to `timeseries_query_timeout_ms`.

## Basho Patches

After downgrading, you should ensure that any custom patches contained in
the `basho-patches` directory are examined to determine their
application to the downgraded version. If you find that patches no longer
apply to the downgraded version, you should remove them from the
`basho-patches` directory prior to operating the node in production.

The following lists locations of the `basho-patches` directory for
each supported operating system:

- CentOS & RHEL Linux: `/usr/lib64/riak/lib/basho-patches`
- Debian & Ubuntu Linux: `/usr/lib/riak/lib/basho-patches`
