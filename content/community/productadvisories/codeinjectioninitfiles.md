---
title: "Possibility of Code Injection on Riak Init File"
description: ""
project: community
project_version: "community"
lastmod: 2016-04-14T00:00:00-00:00
sitemap:
  priority: 0.5
menu:
  community:
    name: "Possibility of Code Injection on Riak Init File"
    identifier: "codeinjection"
    weight: 150
    parent: "productadvisories"
toc: true
aliases:
  - /riak/latest/community/product-advisories/codeinjectioninitfiles/
---

Info | Value
:----|:-----
Date issued | March 1, 2016
Product | Riak KV, Riak CS, and Riak TS
Affected Riak KV versions | 2.1.3 and lower
Affected Riak CS versions | 2.1.1 and lower
Affected Riak TS versions | 1.2.0 and lower

## Overview

Riak KV, Riak CS, and Riak TS package an init script to handle several Riak operations. By default, the `/etc/init.d/riak` file in KV and TS and the `/etc/init.d/riak-cs` and `/etc/init.d/stanchion` files in CS are owned by the Riak user when installed via `yum` or .rpm packages. Any other user able to run commands as the Riak user would be able to edit this file and include malicious code to be executed when the script is initialized.

## Description

Precompiled packages for RedHat/CentOS variants of Riak KV, Riak CS, and Riak TS installed either via `yum` or .rpm are at risk. Instances of Riak KV, Riak CS, and Riak TS installed via source or downloaded zip and then compiled are unaffected. `apt-get` and .deb installations of Riak KV/CS/TS are also unaffected. Riak is executed via the bin/riak application and no init scripts are deployed to start Riak on boot.

## Affected Users

Check the table to see if you are impacted; an 'X' indicates affected users:

| OS                | installed via `yum`/`apt-get` | installed via rpm/deb |
| ------------------|:-----------------------------:|:-----:|
| RHEL/CentOS       | X                             | X     |
| Debian/Ubuntu     | -                             | -     |
| OpenSUSE          | -                             | X     |
| Fedora (19 x64)   | X                             | X     |
| FreeBSD (9.2 x64) | -                             | X     |

## Mitigation Instructions

Change the ownership of the Riak init file from Riak user to root user.

### Riak KV/Riak TS

After you have installed Riak KV or TS, change the ownership of the Riak init file by running the following:

`chown root:root /etc/init.d/riak`

### Riak CS

After you have installed Riak CS, change the ownership of the Riak init files by running the following:

`chown root:root /etc/init.d/riak-cs /etc/init.d/stanchion`

## Moving Forward

Changes to the packaging and `yum` directives will be provided in the next release of each product to secure the init script without the need for manual intervention.

The changes to the security permissions will be included in Riak KV 2.0.7 and 2.2.0, Riak TS 1.3, and Riak CS 2.1.2.
