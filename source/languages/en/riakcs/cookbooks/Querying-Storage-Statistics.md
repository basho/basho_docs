---
title: Querying Storage Statistics
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
---

Storage statistics are tracked on a per-user basis, as rollups for slices of time. Querying these statistics is done via the {{#1.3.0-}}`/usage/$USER_KEY_ID`{{/1.3.0-}}{{#1.3.0+}}`/riak-cs/usage/$USER_KEY_ID`{{/1.3.0+}} resource.

**Note**: Storage statistics are not calculated by default. Please read [[Usage and Billing Data]] for details about how to enable storage calculation archiving.

The basics of querying storage statistics, including the URL used and the parameters for specifying the time slice, are the same as they are for [[Querying Access Statistics]].

Please refer to the descriptions there for more details.

{{#1.3.0+}}The examples on this page assume that the `admin_port` has not
been configured to something other than default CS port of `8080`.{{/1.3.0+}}

## Enable Storage Results

<div class="note"><div class="title">Authentication Required</div>Queries to the usage resources described here must be authenticated as described in the
[[Authentication documentation|Authentication]]. Keep this in mind when using
<tt>curl</tt>. Authentication credentials for <tt>s3cmd</tt> or <tt>s3-curl</tt> can be specified in their respective configuration files.</div>

The usage HTTP resource provides both access and storage statistics. Since each of these queries can be taxing in its own right, they are both omitted from the result by default:

{{#1.3.0-}}

```curl
$ curl http://localhost:8080/usage/8NK4FH2SGKJJM8JIP2GU
```
{{/1.3.0-}}
{{#1.3.0+}}

```curl
$ curl http://localhost:8080/riak-cs/usage/8NK4FH2SGKJJM8JIP2GU
```
{{/1.3.0+}}

Sample responses (reformatted for easy reading):

```json
{
  "Access": "not_requested",
  "Storage": "not_requested"
}
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
  <Usage>
    <Access>not_requested</Access>
    <Storage>not_requested</Storage>
  </Usage>
```

To request that storage results be included, pass the query parameter `b` to the resource (any true-ish value will work, including just the bare `b`, `t`, `true`, `1`, `y`, and `yes`):

{{#1.3.0-}}

```curl
$ curl http://localhost:8080/usage/8NK4FH2SGKJJM8JIP2GU?b
```
{{/1.3.0-}}
{{#1.3.0+}}

```curl
$ curl http://localhost:8080/riak-cs/usage/8NK4FH2SGKJJM8JIP2GU?b
```
{{/1.3.0+}}

Sample responses (reformatted for easy reading):

```json
{
  "Access": "not_requested",
  "Storage": [
    {
      "Errors":[]
    }
  ]
}
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
  <Usage>
    <Access>not_requested</Access>
    <Storage>
      <Errors/>
    </Storage>
  </Usage>
```

There are no statistics included in this report because the default time span is *now*, which is not available in the archives.

### S3 Object-style Access

As described in [[Querying Access Statistics]], these statistics are also available as S3 objects. To add storage statistics to the result, add the character `b` to the `Options` portion of the object's path. For example, the following command would produce storage statistics in XML format:

{{#1.3.0-}}

```bash
$ s3cmd get s3://usage/8NK4FH2SGKJJM8JIP2GU/bx/20120315T140000Z/20120315T160000Z
```
{{/1.3.0-}}
{{#1.3.0+}}

```bash
$ s3cmd get s3://riak-cs/usage/8NK4FH2SGKJJM8JIP2GU/bx/20120315T140000Z/20120315T160000Z
```
{{/1.3.0+}}

You may also pass both `b` and `a` as `Options` to fetch both types of stats, as in:

{{#1.3.0-}}

```bash
$ s3cmd get s3://usage/8NK4FH2SGKJJM8JIP2GU/abx/20120315T140000Z/20120315T160000Z
```
{{/1.3.0-}}
{{#1.3.0+}}

```bash
$ s3cmd get s3://riak-cs/usage/8NK4FH2SGKJJM8JIP2GU/abx/20120315T140000Z/20120315T160000Z
```
{{/1.3.0+}}

## Interpreting the Results

The result of the storage query is one or more "samples" for each time slice in which storage was calculated for the user. The sample will have a start time and end time describing what span the sample covers.

The other entries of each sample are the buckets the user owned during the sampled time. Bucket statistics are provided as rollups including each of the following fields:

* `Objects` --- the number of active---not deleted and not incompletely uploaded---files in the bucket
* `Bytes` --- the total of number of bytes stored in the files of the bucket

For example, a user that owns two buckets, `foo` and `bar`, where `foo` contains one 32MB file and `bar` contains 4 32MB files, would have a sample similar to the following.

Sample responses (reformatted for easy reading):

```json
{
  "Access": "not_requested",
  "Storage": [
    {
      "StartTime": "20120316T123318Z",
      "EndTime": "20120316T123319Z",
      "foo": {
        "Objects": 1,
        "Bytes": 32505856
      },
      "bar": {
        "Objects": 4,
        "Bytes": 130023424
      }
    },
    {
      "Errors": []
    }
  ]
}
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
  <Usage>
    <Access>not_requested</Access>
    <Storage>
      <Sample StartTime="20120316T123318Z" EndTime="20120316T123319Z">
        <Bucket name="hooray">
          <Objects>1</Objects>
          <Bytes>32505856</Bytes>
        </Bucket>
        <Bucket name="foo6">
          <Objects>4</Objects>
          <Bytes>130023424</Bytes>
        </Bucket>
      </Sample>
      <Errors/>
    </Storage>
  </Usage>
```

{{#1.4.4+}}

If any errors occurred during calculation for a bucket, the error will
be returned (e.g., timeout) instead of a bucket's usage.

```json
    {
      "StartTime": "20120316T123318Z",
      "EndTime": "20120316T123319Z",
      "baz": "{error,{timeout,[]}}",
      "bar": {
        "Objects": 4,
        "Bytes": 130023424
      }
    },
```

{{/1.4.4+}}
