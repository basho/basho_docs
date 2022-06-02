---
title: "HTTP 204"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "HTTP 204"
    identifier: "troubleshooting_http_204"
    weight: 101
    parent: "managing_troubleshooting"
toc: true
aliases:
  - /riak/2.9.0p5/using/troubleshooting/http-204/
  - /riak/2.9.0/using/troubleshooting/http-204/
  - /riak/kv/2.9.0/using/troubleshooting/http-204/
  - /riak/kv/2.9.0p1/using/troubleshooting/http-204/
  - /riak/kv/2.9.0p2/using/troubleshooting/http-204/
  - /riak/kv/2.9.0p3/using/troubleshooting/http-204/
  - /riak/kv/2.9.0p4/using/troubleshooting/http-204/
---


In the HTTP standard, a `204 No Content` is returned when the request was successful but there is nothing to return other than HTTP headers.

If you add `returnbody=true` in the `PUT` request, you will receive a `200 OK` and the content you just stored, otherwise you will receive a `204 No Content`.
