---
title: "Downgrading Riak TS"
description: "Downgrading from Riak TS 1.5.0 to 1.4.0"
menu:
  riak_ts-1.5.0:
    name: "Downgrade"
    identifier: "downgrade"
    weight: 300
    parent: "setup"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
aliases:
    - /riakts/1.5.0/setup/downgrading/
    - /riakts/1.5.0/downgrading/
canonical_link: "https://docs.basho.com/riak/ts/latest/setup/downgrading/"
---

## Caution

At the moment, there is no downgrade path between Riak TS 1.5.0 and Riak TS 1.4.0. Any data written to TS 1.5.0 will not be readable by TS 1.4.0. A downgrade path will be available when 1.4.1 is released. Please perform backups before you upgrade and be very sure you are ready to upgrade entirely before doing so.
