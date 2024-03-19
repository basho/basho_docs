---
title: "HTTP 204"
description: ""
project: "riak_kv"
project_version: "3.0.16"
lastmod: 2023-06-23T00:00:00-00:00
sitemap:
  priority: 0.9
menu:
  riak_kv-3.0.16:
    name: "HTTP 204"
    identifier: "troubleshooting_http_204"
    weight: 101
    parent: "managing_troubleshooting"
toc: true
aliases:
---

In the HTTP standard, a `204 No Content` is returned when the request was successful but there is nothing to return other than HTTP headers.

If you add `returnbody=true` in the `PUT` request, you will receive a `200 OK` and the content you just stored, otherwise you will receive a `204 No Content`.

