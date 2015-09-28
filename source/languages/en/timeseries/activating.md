---
title: Activating Your Riak TS Bucket
project: timeseries
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

Once you have [designed your bucket][designing] you can create it via `riak-admin`.

## Creating Your Bucket
Remember the example bucket?

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature float,
PRIMARY KEY (
     (quantum(time, 15, 'm'), myfamily, myseries),
     time, myfamily, myseries

            )
)
```

To create the example bucket, run:

```sh
riak-admin bucket-type create my_bucket_type '{"props":{"n_val":1, "table_def": "CREATE TABLE.........."}}'
```

Then it must be activated like any other bucket type:

```sh
riak-admin bucket-type activate my_bucket_type
```

Because there is a one-to-one correspondence between bucket types and buckets in Riak TS, we recommend that you name the bucket type the same as the table (bucket) in the CREATE TABLE command.
