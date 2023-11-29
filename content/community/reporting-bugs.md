---
title: "Reporting Bugs"
description: ""
project_version: "community"
lastmod: 2016-08-10T00:00:00-00:00
sitemap:
  priority: 0.5
menu:
  community:
    name: "Reporting Bugs"
    identifier: "reporting_bugs"
    weight: 500
    parent: "community_overview"
toc: true
---

Bugs happen. The Basho Dev Team does all that it can to ensure our users
are kept up-to-date on critical bugs and issues that may occur. Here's
the protocol to follow when you think you might have found something
that needs reporting.

## Before You File a Bug or Issue

Before you file a bug or issue, we ask that you put in the necessary
time and research to ensure that what you are reporting is in fact a
bona fide bug and not the result of some other configuration, operating
system, or application problem. To ensure that your bug report is worth
filing, please attempt do the following:

* [Riak Critical Issues Mailing
  List](http://lists.basho.com/mailman/listinfo/riak-critical-issues_lists.basho.com)
  --- A low-traffic list used to disseminate information about bugs or
  issues that might negatively impact a production Riak cluster or deal
  with data integrity
* Project Issues are all managed under their respective [Basho GitHub
  repos](https://github.com/basho/). The following are good repos to pay
  particular attention to:
  * [Riak issues](https://github.com/basho/riak/issues)
  * [Riak Core issues](https://github.com/basho/riak_core/issues)
  * [Riak KV issues](https://github.com/basho/riak_kv/issues)
  * [Riak Search issues](https://github.com/basho/riak_search/issues)
  * [Bitcask issues](https://github.com/basho/bitcask/issues)
  * [eLevelDB issues](https://github.com/basho/eleveldb/issues)
* Search the [Riak Mailing List Archives](http://riak.markmail.org/) for
  similar issues and possible resolution paths. The [Mailman
  archives](http://lists.basho.com/pipermail/riak-users_lists.basho.com/)
  of the list archives can also be accessed.
* Email the [Riak Mailing
  List](http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com)
  with your issue to see if there is a simple path to resolution

## Writing and Filing Your Bug Report

You'll need a GitHub account to file an issue. If you do not have one,
you can [sign up](https://github/com/signup/free) for a free account.

* Select the [appropriate GitHub repo](https://github.com/basho/) for
  your bug. There is a lot of code that makes up Riak, so be sure to
  select the correct repository. For example, if Riak is failing to
  build due to some specific environment variable, this should be filed
  against the [Riak repo](https://github.com/basho/riak/issues). If
  Riak's Search API is failing for a given query, this is an issue for
  the [yokozuna](https://github.com/basho/yokozuna/issues) repository.
* Compose and submit your issue, providing as much detail as possible.
  Strive to be concise, precise, and clear. Include version numbers,
  relevant code snippets, steps to reproduce, etc.

If at any point you have a question about how to file a bug, please
reach out to the [Riak Mailing
List](http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com).

And thank you for being part of the community! We love you for it.
