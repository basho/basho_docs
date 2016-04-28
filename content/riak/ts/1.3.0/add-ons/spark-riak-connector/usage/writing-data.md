---
title_supertext: "Using The Spark-Riak Connector"
title: "Writing Data"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Writing Data"
    identifier: "spark_riak_usage_write_data"
    weight: 103
    parent: "spark_riak_usage"
toc: true
canonical_link: "docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector/usage/writing-data"
---

> **Note:**
>
> Currently, if you are using Python, only Riak TS tables, Spark DataFrames and Spark SQL are supported. Reading and writing to Riak KV buckets is not supported yet with Python.

## Writing Data To KV Bucket

To be able to write data out from an RDD into a Riak KV bucket, the following import for a writer is needed:

```scala
import com.basho.riak.spark.writer.{WriteDataMapper, WriteDataMapperFactory}
```

Define the output bucket and issue `saveToRiak` method on an RDD:

```scala
val output_kv_bucket = new Namespace("test-bucket")
rdd.saveToRiak(output_kv_bucket)
```

## Writing Data To KV Bucket With 2i Indices

There are two ways to add 2i indices to your data when writing into KV buckets. The first way involves creating a `case class` for your data and annotating the key and 2i index fields. In the following example, we create a data object called `ORMDomainObject`, annotate the key field with `@(RiakKey@field)`, and annotate the 2i field with `@(RiakIndex@field) (name = "groupId")`. Then we create a list of data, create an RDD with `val rdd:RDD[ORMDomainObject] = sc.parallelize(data, 1)` and write to a KV bucket with `rdd.saveToRiak(bucket)`. Note that `login: String` is just a regular data field, it is not a key or a 2i index.

```scala

case class ORMDomainObject(
    @(RiakKey@field)
    user_id: String,

    @(RiakIndex@field) (name = "groupId")
    group_id: Long,

    login: String)
  
  
    val data = List(
    ORMDomainObject("u1", 100, "user 1"), 
    ORMDomainObject("u2", 200, "user 2"), 
    ORMDomainObject("u3", 100, "user 3")
    )
    val rdd:RDD[ORMDomainObject] = sc.parallelize(data, 1)

    rdd.saveToRiak(bucket)
```

The second way of writing data with 2i indices to KV bucket is slightly more complicated. You must create a `case class` for a user defined data type that describes the data. Then, place some instances of the data into an RDD. You will then need to modify the custom data mapper section. The data mapper wil take in the user defined data type and map a key-value pair with 2i indices to RiakObjects. If the user defined data type is called something other than `DomainObject` you will need to change these parameters to be the name of your user defined data type. Then you will need to supply what type the 2i index is in the following:

```scala
ro.getIndexes.getIndex[LongIntIndex, LongIntIndex.Name](LongIntIndex.named("groupId")).add(value.group_id)
```

Here we are getting all `LongIntIndex` 2i indices and adding `group_id` to that list. If you wanted something other than a numeric 2i index, you could change `LongIntIndex` to `StringIndex`. Next, we are setting the return value of the mapper to `(value.user_id, ro)` which is a key-value pair that represents the structure of our RiakObjects. Finally, we store our RDD into a KV bucket.

```scala
case class DomainObject(
    user_id: String,
    group_id: Long,
    login: String)
  
  
  val data = List(
    DomainObject("u1", 100, "user 1"), 
    DomainObject("u2", 200,"user 2"), 
    DomainObject("u3", 100, "user 3")
    )
    val rdd:RDD[DomainObject] = sc.parallelize(data, 1)
    
  // create custom data mapper
    implicit val vwf = new WriteDataMapperFactory[DomainObject, KeyValue] {
      override def dataMapper(bucket: BucketDef): WriteDataMapper[DomainObject, KeyValue] = {
        new WriteDataMapper[DomainObject, KeyValue] {
      override def mapValue(value: DomainObject): (String, RiakObject) = {
        val ro = RiakObjectConversionUtil.to(value)

        ro.getIndexes.getIndex[LongIntIndex, LongIntIndex.Name](LongIntIndex.named("groupId")).add(value.group_id)

        (value.user_id, ro)
          }
        }
      }
    }

    rdd.saveToRiak(bucket)
```

## Writing Data To TS Table

You can save an RDD of type `<org.apache.spark.sql.Row>` to Riak TS as follows

```scala
val output_ts_table = "test-bucket"
rdd.saveToRiakTS(output_ts_table);
```
