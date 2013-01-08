---
title: Querying Access Statistics
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
---

Access statistics are tracked on a per-user basis, as rollups for slices
of time. Querying these statistics is done via the `/usage/$USER_KEY_ID`
resource.

For information about how access statistics are logged, please read
[[Usage and Billing
Data]].

The following sections discuss accessing the access statistics using
bare HTTP requests. Query parameters are used to specify the types and
date ranges of information to include. For information on using `s3cmd`
(or other tools) to fetch statistics as S3 objects, skip to the [The
Magic `usage` Bucket](#The-Magic-usage-Bucket) section.

## Choosing the Result Format
Results are available as either JSON or XML. Request the appropriate one
by using the HTTP `Accept` header (with either `application/json` or
`application/xml`, respectively).

## Specifying the User
Access statistics are provided on a per-user basis. Specify which user's
statistics you want by providing that user's `key_id` in the URL. For
example, to get access statistics for the user key
`8NK4FH2SGKJJM8JIP2GU`, use the URL `/usage/8NK4FH2SGKJJM8JIP2GU`.
*Note*: the new user id generator should not include non-URL-safe
characters, but if it does, those characters will need to be escaped in
this url.

A `404` code with an error message body will be returned if the user
does not exist. For example, there is no `ASDF` user in my cluster, so
fetching `http://localhost:8080/usage/ASDF` produces:

JSON:

```
HTTP/1.1 404 Object Not Found

{"Error":{"Message":"Unknown user"}}
```

XML (reformatted for easy reading):

```xml
HTTP/1.1 404 Object Not Found

<?xml version="1.0" encoding="UTF-8"?>
<Error><Message>Unknown user</Message></Error>
```

## Enable Access Results

<div class="note"><div class="title">Authentication Required</div>Queries to the usage resources
described here must be authenticated as described in the
[[Authentication documentation|Authentication]]. Keep this in mind when using
<tt>curl</tt>. Authentication credentials for <tt>s3cmd</tt> or
<tt>s3-curl</tt> can be specified in their respective configuration
files.</div>

The usage HTTP resource provides both access and storage statistics.
Since each of these queries can be taxing in its own right, they are
both omitted from the result by default:

    curl http://localhost:8080/usage/8NK4FH2SGKJJM8JIP2GU

JSON:

```json
{"Access":"not_requested","Storage":"not_requested"}
```

XML (reformatted for easy reading):

```xml
<?xml version="1.0" encoding="UTF-8"?>
    <Usage>
        <Access>not_requested</Access>
        <Storage>not_requested</Storage>
</Usage>
```

To request that access results be included, pass the query parameter `a`
to the resource (any true-ish value will work, including just the bare
`a`, `t`, `true`, `1`, `y`, and `yes`):

    curl http://localhost:8080/usage/8NK4FH2SGKJJM8JIP2GU?a

JSON (reformatted for easy reading):

```json
{"Access":[{"Errors":[]}], "Storage":"not_requested"}
```

XML (reformatted for easy reading):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Usage>
    <Access><Errors/></Access>
    <Storage>not_requested</Storage>
</Usage>
```

There are no statistics included in this report because the default time
span is *now*, which is not available in the archives.

## Specifying the Time Span to Report

Request the time span you want data for by passing `s` (start) and `e`
(end) query parameters to the resource. The slices for which data will
be returned are all of those between `s` and `e`, as well as the slice
including `s` and the slice including `e`.

For example, for slices `A`-`I`:

       A     B     C     D     E     F     G     H     I
    |-----|-----|-----|-----|-----|-----|-----|-----|-----|
                  s                  e

Specifying an `s` that falls somewhere in slice `C` and an `e` that
falls somewhere in slice `F` means that data for slices `C`, `D`, `E`,
and `F` will be returned.

Each should be provided in ISO8601 format (`yyyymmddThhmmssZ`). For
example, the following values would request the span between 2:00pm and
4:00pm (GMT) on January 30, 2012:

    http://localhost:8080/usage/8NK4FH2SGKJJM8JIP2GU?a&s=20120315T140000Z&e=20120315T160000Z

JSON:

```json
{"Access":[
   {"Node":"riak_moss@127.0.0.1",
    "Samples":[{"StartTime":"20120315T150000Z",
                "EndTime":"20120315T152931Z",
                "KeyWrite":{"BytesIn":32505856,"Count":1},
                "KeyRead":{"BytesOut":32505856,"Count":1},
                "BucketRead":{"BytesOut":3633,"Count":5}}]},
   {"Errors":[]}],
"Storage":"not_requested"}
```

XML:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Usage>
   <Access>
      <Node name="riak_moss@127.0.0.1">
         <Sample StartTime="20120315T150000Z" EndTime="20120315T152931Z">
            <Operation type="KeyWrite">
               <BytesIn>32505856</BytesIn>
               <Count>1</Count>
            </Operation>
            <Operation type="KeyRead">
               <BytesOut>32505856</BytesOut>
               <Count>1</Count>
            </Operation>
            <Operation type="BucketRead">
               <BytesOut>3633</BytesOut>
               <Count>5</Count>
            </Operation>
         </Sample>
      </Node>
      <Errors/>
   </Access>
   <Storage>not_requested</Storage>
</Usage>
```

The behavior of the resource when the `s` or `e` parameter is omitted
may change, but is currently:

-   omitting `e` will cause the resource to return only data for the
    slice in which `s` falls
-   omitting `s` will cause the resource to return data for all slices
    from `e` through the current time

Or, more simply, the default `s` is *now* and the default `e` is equal
to `s`.

### Time Span Limit
To prevent excessive time and memory from being consumed accidentally,
the amount of time that may be retrieved in any request is limited.

The limit is configured by the `riak_moss` application environment
variable `usage_request_limit`. The value is expressed as an integer
number of archive intervals (see [[Usage and Billing Data]]
for a description of archive intervals).

The default value is `744`, which is 31 days at the default archive
interval of one hour.

## The Magic `usage` Bucket
If you would prefer to use `s3cmd` or another S3 library to fetch access
stats, you may do so by referencing objects in the global `usage`
bucket. The format for objects in the usage bucket is:

    s3://usage/UserKeyId/Options/StartTime/EndTime

Or, if `/` is automatically quoted (`%2f`) by your client, the `.`
character may be used (this is also nicer for s3cmd, since it will
automatically choose a more useful name for the file it creates):

    s3://usage/UserKeyId.Options.StartTime.EndTime

That is, in the usage bucket, this is a sub-bucket named for the user's
`key_id` (the `UserKeyId` part of the path).

Inside the user's bucket is a sub-bucket named for the contents and
their representation (the `Options` part of the path). This portion
should be:

-   `aj` to receive access statistics as JSON data
-   `ax` to receive access statistics as XML data

The next two portions of the path, `StartTime` and `EndTime` are the
start and end times for the window to report, respectively. These take
the same ISO8601 format that the `s` and `e` query parameters take in
the other request method.

As an example, making the same request as the last example, for
JSON-format access statistics between 2:00pm and 4:00pm GMT on January
30, 2012, looks like this:

    s3cmd get s3://usage/8NK4FH2SGKJJM8JIP2GU/aj/20120315T140000Z/20120315T160000Z

*NOTE*: All objects in the `usage` bucket are read-only. `PUT` and
`DELETE` requests will fail for them.

*NOTE*: Regular users are only allowed to accss the statistics bucket
for their own `key_id`. The admin user is allowed to access any stat
bucket.

## Interpreting the Results
Results of the access query are grouped by node. That is, within the
access field of the result will be one entry for each Riak CS node that
had data for the requested time span.

Each node entry will contain one or more "samples" for each time slice
that the user accessed that Riak CS node. The sample will have a start
time and end time describing what span the sample covers.

The other entries of each sample are the operations the user performed
during the sampled time. Operation statistics are provided as rollups
for each operation type. The rollup includes one or more of the
following fields:

-   `Count` -- the number of times this operation was used successfully
-   `UserErrorCount` -- the number of times this operation was used, but
    ended in a 400-499 response code
-   `SystemErrorCount` -- the number of times this operation was used,
    but ended in a 500-599 response code
-   `BytesIn` -- the number of bytes that were included in the request
    bodies of successful operations
-   `UserErrorBytesIn` -- the number of bytes that were included in the
    request bodies of operations that ended in 400-499 response codes
-   `SystemErrorBytesIn` -- the number of bytes that were included in
    the request bodies of operations that ended in 500-599 response
    codes
-   `BytesOut` -- the number of bytes that were included in the response
    bodies of successful operations
-   `UserErrorBytesOut` -- the number of bytes that were included in the
    response bodies of operations that ended in 400-499 response codes
-   `SystemErrorBytesOut` -- the number of bytes that were included in
    the response bodies of operations that ended in 500-599 response
    codes
-   `BytesOutIncomplete` -- the number of bytes that were sent in
    response bodies before the client disconnected, if there was more
    that could have been sent afterward (i.e. the byte count of partial
    downloads)

It is important to note that accesses are only logged when the
webmachine request finishes. This means that, for example, an upload
started in one time slice but ended in another will only add to the
"bytes in" field for the time slice in which in finished, rather than
splitting the statistics between the slices in which they actually
happened.

### Operation Types
The operation types that are currently tracked are:

-   `ListBuckets` -- listing a user's buckets (`GET /`)

-   `UsageRead` -- reading a user's usage statistics
    (`GET /usage/user/*`)

-   `BucketRead` -- listing the files in a bucket (`GET /bucket`)

-   `BucketStat` -- checking the existence of a bucket (`HEAD /bucket`)

-   `BucketCreate` -- creating a bucket (`PUT /bucket`)

-   `BucketDelete` -- deleting a bucket (`DELETE /bucket`)

-   `BucketUnknown` -- unknown bucket operation (`?? /bucket`)

-   `BucketReadACL` -- retrieving the ACL of a bucket
    (`GET /bucket?acl`)

-   `BucketStatACL` -- checking the existence of a bucket
    (`HEAD /bucket?acl`)

-   `BucketWriteACL` -- changing the ACL of a bucket (`PUT /bucket?acl`)

-   `BucketUnknownACL` -- unknown bucket ACL operation
    (`?? /bucket?acl`)

-   `KeyRead` -- fetching a object (`GET /bucket/key`)

-   `KeyStat` -- checking the existence of a object (`HEAD /bucket/key`)

-   `KeyWrite` -- uploading a object (`PUT /bucket/key`)

-   `KeyDelete` -- deleting a object (`DELETE /bucket/key`)

-   `KeyUnknown` -- unknown object operation (`?? /bucket/key`)

-   `KeyReadACL` -- retrieving the ACL of a key (`GET /bucket/key?acl`)

-   `KeyStatACL` -- checking the existence of a object
    (`HEAD /bucket/key?acl`)

-   `KeyWriteACL` -- changing the ACL of a object
    (`PUT /bucket/key?acl`)

-   `KeyUnknownACL` - unknown key ACL operation (`?? /bucket/key?acl`)

-   `UnknownGET` -- a `GET` was issued on an unrecognized resource;
    likely means that the `riak_moss_access_logger:operation/1` function
    is out of date

-   `UnknownHEAD` -- see `UnknownGET`

-   `UnknownPUT` -- see `UnknownGET`

-   `UnknownPOST` -- see `UnknownGET`

-   `UnknownDELETE` -- see `UnknownGET`

### Lookup Errors
In addition to the node entries in the access results, there is also an
entry for errors that Riak CS encountered while fetching access
archives. The errors list is very similar to the samples of a node list:
each entry will contain the start and end times of the period, as well
as the "reason" the lookup failed.

For example, if the Riak lookups that Riak CS uses end in timeout
instead of success, the result including an errors list might look like
the following.

JSON:

```json
{"Access":[
   {"Errors":[
      {"StartTime":"20120315T160000Z",
       "EndTime":"20120315T170000Z",
       "Reason":"timeout"}]}],
"Storage":"not_requested"}
```

XML:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Usage>
   <Access>
      <Errors>
         <Sample StartTime="20120315T160000Z" EndTime="20120315T170000Z">
            <Reason>timeout</Reason>
         </Sample>
      </Errors>
   </Access>
   <Storage>not_requested</Storage>
</Usage>
```
