---
title: HTTP List Keys
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/http/HTTP-List-Keys/'
}
---

Lists keys in a bucket.

<div class="note">
<div class="title">Not for production use</div>

This operation requires traversing all keys stored in the cluster and should not be used in production.

</div>

## Request

```bash
GET /riak/bucket?keys=true            # List all keys, old format
GET /buckets/bucket/keys?keys=true    # List all keys, new format
GET /riak/bucket?keys=stream          # Stream keys to the client, old format
GET /buckets/bucket/keys?keys=stream  # Stream keys to the client, new format
```

Required query parameters:

* `keys` - defaults to `false`. When set to `true` all keys will be returned in
a single payload.  When set to `stream`, keys will be returned in
chunked-encoding.

Optional query parameters:

* `props` - defaults to `true`, which will also return [[bucket properties|HTTP-Get-Bucket-Properties]] in the response. Set to `false` to suppress properties
in the response.

## Response

Normal response codes:

* `200 OK`

Important headers:

* `Content-Type` - `application/json`
* `Transfer-Encoding` - `chunked` when the `keys` query parameter is set to
`stream`.

The JSON object in the response will contain up to two entries, `"props"` and
`"keys"` which are present or missing according to the query parameters.  If
`keys=stream` in the query parameters, multiple JSON objects in chunked-encoding
will be returned containing `"keys"` entries.

## Example

```bash
$ curl -i http://localhost:8098/riak/jsconf?keys=true\&props=false
HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
Link: </riak/jsconf/challenge.jpg>; riaktag="contained",
</riak/jsconf/puddi.png>; riaktag="contained", </riak/jsconf/basho.gif>;
riaktag="contained", </riak/jsconf/puddikid.jpg>; riaktag="contained",
</riak/jsconf/yay.png>; riaktag="contained", </riak/jsconf/thinking.png>;
riaktag="contained", </riak/jsconf/victory.gif>; riaktag="contained",
</riak/jsconf/slides>; riaktag="contained", </riak/jsconf/joyent.png>;
riaktag="contained", </riak/jsconf/seancribbs-small.jpg>; riaktag="contained",
</riak/jsconf/trollface.jpg>; riaktag="contained",
</riak/jsconf/riak_logo_animated1.gif>; riaktag="contained",
</riak/jsconf/victory.jpg>; riaktag="contained", </riak/jsconf/challenge.png>;
riaktag="contained", </riak/jsconf/team_cribbs.png>; riaktag="contained"
Date: Fri, 30 Sep 2011 15:24:35 GMT
Content-Type: application/json
Content-Length: 239

{"keys":["challenge.jpg","puddi.png","basho.gif","puddikid.jpg","yay.png","
thinking.png","victory.gif","slides","joyent.png","seancribbs-small.jpg","
trollface.jpg","riak_logo_animated1.gif","victory.jpg","challenge.png","
team_cribbs.png"]}
```
