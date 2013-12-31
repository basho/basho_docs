---
title: HTTP Administration Overview
project: riakcs
version: 1.3.0+
document: appendix
toc: true
audience: intermediate
keywords: [api]
---

Riak CS exposes the following administrative capabilities over HTTP above and
beyond those associated with Riak itself:

| Task                   | CS URI           | Further reading                 |
| ---------------------- | ---------------- | ------------------------------- |
| User management        | `/riak-cs/user`  | [[Account Management]]          |
| User access statistics | `/riak-cs/usage` | [[Querying Access Statistics]]  |
| Storage statistics     | `/riak-cs/usage` | [[Querying Storage Statistics]] |
| Global statistics      | `/riak-cs/stats` | [[Monitoring and Metrics]]      |

By default, these are accessible over the same IP/port as the rest of the CS API, but they can be configured to run elsewhere, with or without authentication.

## Output format

For these requests, results are available as either JSON or XML. Request the appropriate one by using the HTTP `Accept` header with either `application/json` or `application/xml`, respectively.

## URLs

Each of these requests is performed over the CS HTTP port (`8080` by
default) or administrative port if configured via `admin_port`. The
`admin_ip` configuration setting can be used to further isolate the
administrative commands.

Only the admin user can view other users' details unless the `admin_auth_enabled` config is set to `false`.

## Retrieving statistics via S3 objects

As an alternative to raw HTTP requests, the administrative requests can be issued via the S3 API. See the GitHub documents linked below for more details.

Related Resources
-----------------

- [[Configuring Riak CS]]
- [[Querying Access Statistics]]
    - [[Usage and Billing Data]]
    - [Github wiki](https://github.com/basho/riak_cs/wiki/Querying-Access-Stats)
- [[Querying Storage Statistics]]
    - [Enabling storage statistics](https://github.com/basho/riak_cs/wiki/Logging-Storage-Stats)
    - [Github wiki](https://github.com/basho/riak_cs/wiki/Logging-Storage-Stats)
- [[Account Management]]
    - [Github wiki](https://github.com/basho/riak_cs/wiki/User-Management)
- [[Monitoring and Metrics]]
