---
title: "HTTP Link Walking"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "Link Walking"
    identifier: "http_link_walking"
    weight: 118
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.9/dev/references/http/link-walking
  - /riak/kv/2.9.9/dev/references/http/link-walking
---

{{% note title="Deprecation Warning" %}}
This feature is deprecated and will be removed in a future version.
{{% /note %}}

Link walking (traversal) finds and returns objects by following links attached
to them, starting from the object specified by the bucket and key portion.  It
is a special case of [MapReduce]({{<baseurl>}}riak/kv/2.9.9/developing/usage/mapreduce), and can be expressed more verbosely as such.
[Read more about Links]({{<baseurl>}}riak/kv/2.9.9/learn/glossary/#links).

## Request

```bash
GET /buckets/bucket/keys/key/[bucket],[tag],[keep]
```

{{% note title="Link filters" %}}
A link filter within the request URL is made of three parts, separated by
commas:

* Bucket - a bucket name to limit the links to
* Tag - a "riaktag" to limit the links to
* Keep - 0 or 1, whether to return results from this phase

Any of the three parts may be replaced with `_` (underscore), signifying that
any value is valid. Multiple phases of links can be followed by adding
additional path segments to the URL, separating the link filters by slashes.
The final phase in the link-walking query implicitly returns its results.
{{% /note %}}

## Response

Normal status codes:

* `200 OK`

Typical error codes:

* `400 Bad Request` - if the format of the query in the URL is invalid
* `404 Not Found` - if the origin object of the walk was missing

Important headers:

* `Content-Type` - always `multipart/mixed`, with a boundary specified

> **Understanding the response body**
>
> The response body will always be `multipart/mixed`, with each
chunk representing a single phase of the link-walking query. Each phase will
also be encoded in `multipart/mixed`, with each chunk representing a
single object that was found. If no objects were found or "keep" was not set on
the phase, no chunks will be present in that phase.  Objects inside phase
results will include `Location` headers that can be used to determine
bucket and key. In fact, you can treat each object-chunk similarly to a complete
response from [fetching the object]({{<baseurl>}}riak/kv/2.9.9/developing/api/http/fetch-object), without the status
code.

## Example

```curl
$ curl -v http://127.0.0.1:8098/riak/test/doc3/test,_,1/_,next,1
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test/doc3/test,_,1/_,next,1 HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Expires: Wed, 10 Mar 2010 20:24:49 GMT
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: multipart/mixed; boundary=JZi8W8pB0Z3nO3odw11GUB4LQCN
< Content-Length: 970
<

--JZi8W8pB0Z3nO3odw11GUB4LQCN
Content-Type: multipart/mixed; boundary=OjZ8Km9J5vbsmxtcn1p48J91cJP

--OjZ8Km9J5vbsmxtcn1p48J91cJP
X-Riak-Vclock: a85hYGDgymDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt7HlGBhnqejARXmq0QyZnnxE6jwVJBwFgA=
Location: /riak/test/doc
Content-Type: application/json
Link: </riak/test>; rel="up", </riak/test/doc2>; riaktag="next"
Etag: 3pvmY35coyWPxh8mh4uBQC
Last-Modified: Wed, 10 Mar 2010 20:14:13 GMT

{"riak":"CAP"}
--OjZ8Km9J5vbsmxtcn1p48J91cJP--

--JZi8W8pB0Z3nO3odw11GUB4LQCN
Content-Type: multipart/mixed; boundary=RJKFlAs9PrdBNfd74HANycvbA8C

--RJKFlAs9PrdBNfd74HANycvbA8C
X-Riak-Vclock: a85hYGBgzGDKBVIsbLvm1WYwJTLmsTLcjeE5ypcFAA==
Location: /riak/test/doc2
Content-Type: application/json
Link: </riak/test>; rel="up"
Etag: 6dQBm9oYA1mxRSH0e96l5W
Last-Modified: Wed, 10 Mar 2010 18:11:41 GMT

{"foo":"bar"}
--RJKFlAs9PrdBNfd74HANycvbA8C--

--JZi8W8pB0Z3nO3odw11GUB4LQCN--
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```




