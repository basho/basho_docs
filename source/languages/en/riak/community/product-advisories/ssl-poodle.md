---
title: SSL 3.0 Vulnerability and POODLE Attack
project: riak
version: 1.0.0+
versions: false
document: reference
---

Info | Value
:----|:-----
Date issued | January 27, 2015
Product | Riak and Riak CS
Affected Riak versions | 1.3 series, 1.4 series, 2.0.0-2.0.2
Affected Riak CS versions | 1.3 series, 1.4 series, 1.5 series (i.e. all versions of Riak CS)

## Overview

In certain configurations, the Erlang VM on which Riak and Riak CS rely
is vulnerable to an exploit of an old version of SSL. This vulnerability
has been named POODLE. More details can be found
[here](https://www.us-cert.gov/ncas/alerts/TA14-290A).

## Description

This fix is very narrow in scope. It instructs Erlang's SSL library to
forbid SSL version 3 traffic.

## Mitigation Strategy

You will need to install an Erlang patch. Installation instructions can
be found in the `README` file in the appropriate ZIP for your version
of Riak or Riak CS, as they differ between versions.

Proper ZIP file | Versions
:---------------|:--------
[poodle-1.x.zip](https://github.com/basho/basho_docs/raw/master/source/data/poodle-1.x.zip) | Open-source Riak for the 1.4.x series

## Moving Forward

This patch is included in Riak 2.0.5 and all releases thereafter.
