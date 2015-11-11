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
    (myfamily, myseries, quantum(time, 15, 'm')),
    myfamily, myseries, time

            )
)
```

To create the example bucket, run:

```sh
riak-admin bucket-type create GeoCheckin '{"props":{"n_val":1, "table_def": "CREATE TABLE GeoCheckin (myfamily varchar not null, myseries varchar not null, time timestamp not null, weather varchar not null, temperature double, PRIMARY KEY (myfamily, myseries, (quantum (time, 15, 'm')), myfamily, myseries, time))"}}'
```

>Please note that Bucket Type Name must equal the Table Name.

Then it must be activated like any other bucket type:

```sh
riak-admin bucket-type activate GeoCheckin
```

Because there is a one-to-one correspondence between bucket types and buckets in Riak TS, we recommend that you name the bucket type the same as the table (bucket) in the `CREATE TABLE` command.

## Viewing Table Scheme

To view the Table scheme use the following:

```sh
riak-admin bucket-type status
```

So for the example `GeoCheckin` Bucket Type:

```sh
riak-admin GeoCheckin status
```

To check if your Bucket Type was properly created, see the `ddl` section of the `riak-admin bucket-type status` response. For example:

```sh
$riak-admin bucket-type status GeoCheckin
GeoCheckin is active
...
ddl: {ddl_v1,<<"GeoCheckin">>,
             [{riak_field_v1,<<"myfamily">>,1,binary,false},
              {riak_field_v1,<<"myseries">>,2,binary,false},
              {riak_field_v1,<<"time">>,3,timestamp,false},
              {riak_field_v1,<<"weather">>,4,binary,false},
              {riak_field_v1,<<"temperature">>,5,float,true}],
             {key_v1,[{hash_fn_v1,riak_ql_quanta,quantum,
                                  {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]},
                      [{param_v1,[<<"time">>]},15,m],
                                  timestamp},
             {key_v1,[{param_v1,[<<"time">>]},
                      {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]}}
```

The format can be read as:

```sh
ddl:  { ddl_v1, TABLE_NAME, 
[ ARRAY OF COLUMNS], 
[ KEY INFO ]}}
```

The columns are each:

```sh
{riak_field_v1,<<"FIELD_NAME">>,COLUMN_INDEX,COLUMN_TYPE,NULLABLE}
```

The Key info contains the columns used for the Partition Key (defines how the data set is chunked, and then chunk data is co-located), and the Local Key(the unique identifier within a chunk). These two sets of columns will mostly be the same, but the Partition Key will have an additional Quantum definition for the timestamp column:

```sh
{key_v1,[
   {hash_fn_v1,riak_ql_quanta,quantum,
               {param_v1,[<<"myfamily">>]},               <- Partition Key Part 1
               {param_v1,[<<"myseries">>]},               <- Partition Key Part 2 
               [{param_v1,[<<"time">>]},15,m],timestamp}  <- Partition Key Part 3

]},
{key_v1,[
   {param_v1,[<<"myfamily">>]},  <- Local Key part 1
   {param_v1,[<<"myseries">>]},  <- Local Key part 2
   {param_v1,[<<"time">>]}       <- Local Key part 3
]}
```
