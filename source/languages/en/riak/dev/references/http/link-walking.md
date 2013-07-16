---
title: HTTP Link Walking
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Query Operations"
moved: {
  '1.4.0-': '/references/apis/http/HTTP-Link-Walking'
}
---

Link walking (traversal) finds and returns objects by following links attached
to them, starting from the object specified by the bucket and key portion.  It
is a special case of [[MapReduce|Using MapReduce]], and can be expressed more verbosely as such.
[[Read more about Links|Links]].

## Request

```bash
GET /riak/bucket/key/[bucket],[tag],[keep]            # Old format
GET /buckets/bucket/keys/key/[bucket],[tag],[keep]    # New format
```

<div class="info"><div class="title">Link filters</div>
<p>A link filter within the request URL is made of three parts, separated by
commas:</p>

<ul>
<li>Bucket - a bucket name to limit the links to</li>
<li>Tag - a "riaktag" to limit the links to</li>
<li>Keep - 0 or 1, whether to return results from this phase</li>
</ul>

<p>Any of the three parts may be replaced with <code>_</code> (underscore),
signifying that any value is valid. Multiple phases of links can be followed by
adding additional path segments to the URL, separating the link filters by
slashes. The final phase in the link-walking query implicitly returns its
results.</p>
</div>

## Response

Normal status codes:

* `200 OK`

Typical error codes:

* `400 Bad Request` - if the format of the query in the URL is invalid
* `404 Not Found` - if the origin object of the walk was missing

Important headers:

* `Content-Type` - always `multipart/mixed`, with a boundary specified

<div class="note"><div class="title">Understanding the response body</div>
<p>The response body will always be <code>multipart/mixed</code>, with each
chunk representing a single phase of the link-walking query. Each phase will
also be encoded in <code>multipart/mixed</code>, with each chunk representing a
single object that was found. If no objects were found or "keep" was not set on
the phase, no chunks will be present in that phase.  Objects inside phase
results will include <code>Location</code> headers that can be used to determine
bucket and key. In fact, you can treat each object-chunk similarly to a complete
response from [[fetching the object|HTTP Fetch Object]], without the status
code.</p>
</div>

## Example

```bash
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
