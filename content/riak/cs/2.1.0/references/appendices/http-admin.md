---
title: "HTTP Administration Overview"
description: ""
menu:
  riak_cs-2.1.0:
    name: "Accounts & Admin"
    identifier: "http_admin"
    weight: 100
    parent: "develop"
project: "riak_cs"
project_version: "2.1.0"
aliases:
  - /riakcs/2.1.0/references/appendices/Http-Administration/
  - /riak/cs/2.1.0/references/appendices/Http-Administration/
---

Riak CS exposes the following administrative capabilities over HTTP
above and beyond those associated with Riak itself:

Task | CS URI | Further reading
:----|:-------|:---------------
User management        | `/riak-cs/user`  | [Account Management]({{<baseurl>}}riak/cs/2.1.0/cookbooks/account-management)
User access statistics | `/riak-cs/usage` | [Querying Access Statistics]({{<baseurl>}}riak/cs/2.1.0/cookbooks/querying-access-statistics)
Storage statistics     | `/riak-cs/usage` | [Querying Storage Statistics]({{<baseurl>}}riak/cs/2.1.0/cookbooks/querying-storage-statistics)
Global statistics      | `/riak-cs/stats` | [Monitoring and Metrics]({{<baseurl>}}riak/cs/2.1.0/cookbooks/monitoring-and-metrics)

By default, these are accessible over the same IP/port as the rest of
the CS API, but they can be configured to run elsewhere, with or without
authentication.

## Output format

For these requests, results are available as either JSON or XML. Request
the appropriate data format by using the HTTP `Accept` header with
either `application/json` or `application/xml`, respectively.

## URLs

Each of these requests is performed over the CS HTTP port (`8080` by
default) or administrative port if configured via `admin_port`. The
`admin_ip` configuration setting can be used to further isolate the
administrative commands.

Only the admin user can view other users' details unless the
`admin_auth_enabled` config is set to `false`.

## Retrieving Statistics Via S3 Objects

As an alternative to raw HTTP requests, the administrative requests can
be issued via the S3 API. See the GitHub documents linked below for more
details.

## Related Resources

* [configuring Riak CS]({{<baseurl>}}riak/cs/2.1.0/cookbooks/configuration/riak-cs)
* [Querying Access Statistics]({{<baseurl>}}riak/cs/2.1.0/cookbooks/querying-access-statistics)
    * [Usage and Billing Data]({{<baseurl>}}riak/cs/2.1.0/cookbooks/usage-and-billing-data)
    * [Github wiki](https://github.com/basho/riak_cs/wiki/Querying-Access-Stats)
* [Querying Storage Statistics]({{<baseurl>}}riak/cs/2.1.0/cookbooks/querying-storage-statistics)
    * [Enabling storage statistics](https://github.com/basho/riak_cs/wiki/Logging-Storage-Stats)
    * [Github wiki](https://github.com/basho/riak_cs/wiki/Logging-Storage-Stats)
* [Account Management]({{<baseurl>}}riak/cs/2.1.0/cookbooks/account-management)
    * [Github wiki](https://github.com/basho/riak_cs/wiki/User-Management)
* [Monitoring and Metrics]({{<baseurl>}}riak/cs/2.1.0/cookbooks/monitoring-and-metrics)
