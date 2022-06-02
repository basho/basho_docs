---
title: "Riak CS GET Bucket"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-GET-Bucket
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-GET-Bucket
  - /riak/cs/latest/references/apis/storage/s3/get-bucket/
---

The `GET Bucket` operation returns a list of objects (all or up to 1,000) in a bucket.

*Note:* You must have READ access to the bucket to use this operation.

## Requests

### Request Syntax

```
GET / HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

### Request Parameters

GET Bucket uses the following parameters to return a subset of the objects in a bucket.

**prefix** - A string with which keys must begin to be included in the response.

You can use prefixes to separate the objects in a bucket into groupings of keys.

* *Type*: String
* *Default*: None

**Delimiter** - Keys that contain the same string between the `prefix` and the first occurrence of the `delimiter` are rolled up into a single result in the `CommonPrefixes` collection and aren't returned anywhere else in the response.

* *Type*: String
* *Default*: None

**Marker** - The starting location in the bucket for the list of objects.

* *Type*: String
* *Default*: None

**MaxKeys** - The maximum number of keys returned in the response body.

* *Type*: String
* *Default*: 1000

## Response Elements

**Contents** - Metadata about each object returned in the response.

* *Type*: XML metadata
* *Ancestry*: ListBucketResult

**CommonPrefixes** - Keys, if any, between the `Prefix` and the next occurrence of the `delimiter` string.

A response contains `CommonPrefixes` only if the request includes a `delimiter`. `CommonPrefixes` lists keys that act like subdirectories in the directory specified by `Prefix`. If `Prefix` is *projects/* and `delimiter` is */*, the common prefix in *projects/marketing/2012* is *projects/marketing/*. The keys rolled up into a common prefix represent a single return for the calculation of the number of returns (which is limited by `MaxKeys`).


* *Type*: String
* *Ancestry*: ListBucketResult

**Delimiter** - Keys that contain the same string between the `prefix` and the first occurrence of the `delimiter` are rolled up into a single result in the `CommonPrefixes` collection and aren't returned anywhere else in the response.


* *Type*: String
* *Ancestry*: ListBucketResult

**DisplayName** - Object owner's display name.


* *Type*: String
* *Ancestry*: ListBucketResult.Contents.Owner

**ETag** - The entity tag is an MD5 hash of the object and reflects only changes to the object contents, not the object's metadata.


* *Type*: String
* *Ancestry*: ListBucketResult.Contents

**ID** - Object owner's user ID.


* *Type*: String
* *Ancestry*: ListBucketResult.Contents.Owner

**IsTruncated** - Indicates whether all of the results were returned (`true`) or only a subset (`false`) because the number of results returned exceeded the maximum specified by `MaxKeys`.


* *Type*: String
* *Ancestry*: boolean

**Key** - The object key.


* *Type*: String
* *Ancestry*: ListBucketResult.Contents

**LastModified** - The date and time that the object was last modified.


* *Type*: Date
* *Ancestry*: ListBucketResult.Contents

**Marker** - The starting location in the bucket for the list of objects.


* *Type*: String
* *Ancestry*: ListBucketResult

**MaxKeys** - The maximum number of keys returned in the response body.


* *Type*: String
* *Ancestry*: ListBucketResult

**Name** - Bucket's name.


* *Type*: String
* *Ancestry*: ListBucketResult

**Owner** - Bucket owner.


* *Type*: String
* *Children*: DisplayName, ID
* *Ancestry*: ListBucketResult.Contents|CommonPrefixes

**Prefix** - Keys that begin with the indicated prefix.


* *Type*: String
* *Ancestry*: ListBucketResult

**Size** - The object's size in bytes.


* *Type*: String
* *Ancestry*: ListBucketResult.Contents

**StorageClass** - Always STANDARD.


* *Type*: String
* *Ancestry*: ListBucketResult.Contents

## Examples

### Sample Request
A request that returns the objects in the bucket, `projects`.

```
GET / HTTP/1.1
Host: projects.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
Content-Type: text/plain
```

### Sample Response

```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <ListBucketResult xmlns="http://data.basho.com/2012-06-12">
      <Name>projects</Name>
      <Prefix/>
      <Marker/>
      <MaxKeys>1000</MaxKeys>
      <IsTruncated>false</IsTruncated>
      <Contents>
          <Key>scheduleQ1.jpg</Key>
          <LastModified>2012-06-01T09:20:03.000Z</LastModified>
          <ETag>"f77127731fba39869dede5c9645a3328"</ETag>
          <Size>519226</Size>
          <StorageClass>STANDARD</StorageClass>
          <Owner>
              <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
              <DisplayNamefoobar</DisplayName>
          </Owner>
      </Contents>
      <Contents>
         <Key>scheduleQ2.jpg</Key>
           <LastModified>2012-06-02T11:02:42</LastModified>
          <ETag>"645a39851b2cf27731c974f535343328"</ETag>
          <Size>990102</Size>
          <StorageClass>STANDARD</StorageClass>
          <Owner>
              <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
              <DisplayName>foobar</DisplayName>
          </Owner>
      </Contents>
  </ListBucketResult>
```

### Sample Request Using Request Parameters

This sample request lists up to 100 keys in the `projects` bucket that start with `IT` and occur after the key that begins with `ITdb`.

```
GET ?prefix=IT HTTP/1.1
Host: projects.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### Sample Response Based on Request Parameters

```
HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 06 Jun 2012 20:48:15 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: BashoData

  <?xml version="1.0" encoding="UTF-8"?>
  <ListBucketResult xmlns="http://data.basho.com/2012-06-12/">
    <Name>projects</Name>
    <Prefix>IT</Prefix>
    <Marker></Marker>
    <MaxKeys>1000</MaxKeys>
    <IsTruncated>false</IsTruncated>
    <Contents>
      <Key>ITdb</Key>
      <LastModified>2012-06-01T09:20:03.000Z</LastModified>
      <ETag>"f77127731fba39869dede5c9645a3328"</ETag>
      <Size>29493</Size>
      <StorageClass>STANDARD</StorageClass>
      <Owner>
        <ID>B420EFC086821B324ABC0713CD0FAE7ED81442C</ID>
        <DisplayName>richardp</DisplayName>
       </Owner>
    </Contents>
    <Contents>
      <Key>ITstorage</Key>
      <LastModified>2012-04-14T04:20:10.000Z</LastModified>
      <ETag>"a96f00ad9f27c3828ef3fdf83fc9ac7f"</ETag>
      <Size>4</Size>
      <StorageClass>STANDARD</StorageClass>
       <Owner>
        <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
        <DisplayName>foobar</DisplayName>
      </Owner>
   </Contents>
  </ListBucketResult>
```
