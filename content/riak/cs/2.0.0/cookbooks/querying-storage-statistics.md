---
title: "Querying Storage Statistics"
description: ""
menu:
  riak_cs-2.0.0:
    name: "Storage Statistics"
    identifier: "storage_stats"
    weight: 301
    parent: "develop"
project: "riak_cs"
project_version: "2.0.0"
aliases:
  - /riakcs/2.0.0/cookbooks/Querying-Storage-Statistics/
---

Storage statistics are tracked on a per-user basis, as rollups for
slices of time. Querying these statistics is done via the
`/riak-cs/usage/$USER_KEY_ID` resource.

{{% note title="Note on terminology" %}}
In this and other documents in the Riak CS documentation, the terms "storage"
and "billing" are used interchangeably. The same goes for the terms "usage"
and access.
{{% /note %}}


> **Note**:
> 
> Storage statistics are not calculated by default. Please read [Usage and Billing Data]({{<baseurl>}}riak/cs/2.0.0/cookbooks/usage-and-billing-data) for details about how to enable storage calculation archiving.

The basics of querying storage statistics, including the URL used and the parameters for specifying the time slice, are the same as they are for [Querying Access Statistics]({{<baseurl>}}riak/cs/2.0.0/cookbooks/querying-access-statistics).

Please refer to the descriptions there for more details.

The examples on this page assume that the `admin_port` has not
been configured to something other than default CS port of `8080`.

## Enable Storage Results

> **Authentication Required**
>
> Queries to the usage resources described here must be authenticated as described in the [Authentication documentation]({{<baseurl>}}riak/cs/2.0.0/cookbooks/authentication). Keep this in mind when using `curl`. Authentication credentials for `s3cmd` or `s3-curl` can be specified in their respective configuration files.

The usage HTTP resource provides both access and storage statistics. Since each of these queries can be taxing in its own right, they are both omitted from the result by default:

```curl
curl http://localhost:8080/riak-cs/usage/8NK4FH2SGKJJM8JIP2GU
```

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

```curl
curl http://localhost:8080/riak-cs/usage/8NK4FH2SGKJJM8JIP2GU?b
```

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

As described in [Querying Access Statistics]({{<baseurl>}}riak/cs/2.0.0/cookbooks/querying-access-statistics), these statistics are also available as S3 objects. To add storage statistics to the result, add the character `b` to the `Options` portion of the object's path. For example, the following command would produce storage statistics in XML format:

```bash
s3cmd get s3://riak-cs/usage/8NK4FH2SGKJJM8JIP2GU/bx/20120315T140000Z/20120315T160000Z
```

You may also pass both `b` and `a` as `Options` to fetch both types of stats, as in:

```bash
s3cmd get s3://riak-cs/usage/8NK4FH2SGKJJM8JIP2GU/abx/20120315T140000Z/20120315T160000Z
```

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
