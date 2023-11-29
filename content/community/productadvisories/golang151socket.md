---
title: "Socket reuse issue with Riak Golang client 1.5.1"
description: ""
project_version: "community"
lastmod: 2016-04-09T00:00:00-00:00
sitemap:
  priority: 0.5
menu:
  community:
    name: "Socket Reuse in Riak Golang 1.5.1"
    identifier: "socketgolang"
    weight: 175
    parent: "productadvisories"
toc: true
---

Info | Value
:----|:-----
Date issued | March 1st 2016
Product | Golang client 1.5.1
Affected versions | 1.5.1

## Overview

When using the Riak golang client 1.5.1, if the socket read timeout is exceeded during execution of a Riak request, data may be contaminated between subsequent requests that use the same socket.

## Description

When using the Riak golang client 1.5.1, if the socket read timeout is exceeded during execution of a Riak request, instead of closing it, the connection will be returned to the connection pool for use by a different request. The server still sends a response on that socket and it will be available the next time the socket is read. If the socket is provided by the connection pool, the next request will read data from Riak intended for the original request.

If the next request is of a different type than the first that timed out, you will see an error similar to the following raised by the golang client: `expected response code X, got: Y`. If the next request is of the same type as the first one that timed out, the original possibly unrelated response, will be returned by the client library without logging any message.

## Affected Users

This issue will affect you only if both of the following are true:

* You are using the golang 1.5.1 client, AND
* The client request timeout is being hit.

## Impact

Results from client requests may be returned against the wrong call. For example, results from a previous search request may be returned instead of the correct ones.

If it occurs in a GET/PUT cycles this may result in the incorrect GET response being used in a  PUT request, leaving the object permanently modified.

## Mitigation Strategy

Downgrade to 1.5.0 or upgrade to the 1.6.0 Golang client which can be found here:
[https://github.com/basho/riak-go-client/releases](https://github.com/basho/riak-go-client/releases)

If you cannot upgrade, set `RequestTimeout` in `NodeOptions` to be a very large value (5 minutes, for instance) to reduce the likelihood of the socket being reused.
