---
title: "Riak KV 3.0.1 Release Notes"
description: ""
project: "riak_kv"
project_version: 3.0.1
menu:
  riak_kv-3.0.1:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.1/community/release-notes
  - /riak/kv/3.0.1/intro-v20
  - /riak/3.0.1/intro-v20
  - /riak/kv/3.0.1/introduction

---

Released Aug 20, 2020.


## Overview

This major release allows Riak to run on OTP versions 20, 21 and 22 - but is not fully backwards-compatible with previous releases. Some limitations and key changes should be noted:

- It is not possible to run this release on any OTP version prior to OTP 20. Testing of node-by-node upgrades is the responsibility of Riak customers, there has been no comprehensive testing of this upgrade managed centrally. Most customer testing of upgrades has been spent on testing an uplift from 2.2.x and OTP R16 to 3.0 and OTP 20, so this is likely to be the safest transition.

- This release will not by default include Yokozuna support, but Yokozuna can be added in by reverting the commented lines in rebar.config. There are a number of riak_test failures with Yokozuna, and these have not been resolved prior to release. Upgrading with yokozuna will be a breaking change, and data my be lost due to the uplift in solr version. Any migration will require bespoke management of any data within yokozuna.

- Packaging support is not currently proven for any platform other than CentOS, Debian or Ubuntu. Riak will build from source on other platforms - e.g. `make locked-deps; make rel`.

- As part of the release there has been a comprehensive review of all tests across the dependencies (riak_test, eunit, eqc and pulse), as well as removal of all dialyzer and xref warnings and addition where possible of travis tests. The intention is to continue to raise the bar on test stability before accepting Pull Requests going forward.

- If using riak_client directly (e.g. `{ok, C} = riak:local_client()`), then please use `riak_client:F(*Args, C) not C:F(*Args)` when calling functions within riak_client - the latter mechanism now has issues within OTP 20+.

- Instead of `riak-admin` the command `riak admin` should now be used for admin CLI commands.

- Other than the limitations listed above, the release should be functionally identical to Riak KV 2.9.7. Throughput improvements may be seen as a result of the OTP 20 upgrade on some CPU-bound workloads. For disk-bound workloads, additional benefit may be achieved by upgrading further to OTP 22.

[Previous Release Notes](#previous-release-notes)

## Previous Release Notes

Please see the KV 2.9.7 release notes [here]({{<baseurl>}}riak/kv/2.9.7/release-notes/), the KV 2.9.4 release notes [here]({{<baseurl>}}riak/kv/2.9.4/release-notes/), and the KV 2.9.1 release notes [here]({{<baseurl>}}riak/kv/2.9.1/release-notes/).




