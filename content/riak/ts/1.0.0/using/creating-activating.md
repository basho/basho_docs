---
title: "Creating and Activating Your Riak TS Table"
description: "Creating and Activating Your Riak TS Table"
menu:
  riak_ts-1.0.0:
    name: "Create and Activate Your Table"
    identifier: "creating_activating_riakts"
    weight: 302
    parent: "using"
project: "riak_ts"
project_version: "1.0.0"
toc: true
aliases:
    - /riakts/1.0.0/using/creating-activating/
---

[planning]: ../planning/
[writing]: ../writingdata/

Once you have [planned out your table][planning] you can create it using `riak-admin`.

>**Note:** You will need to have access to `sudo` and `su` with the below commands, unless you are operating on Mac OS X.

## Creating Your Table

Remember the example table?

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
     myfamily, myseries, time
   )
)
```

To create the example table, first run:
(**Note: Mac OS X users can skip this step**)

```bash
sudo su riak
```

This will put you in a shell as the riak user. Then run:

```sh
riak-admin bucket-type create GeoCheckin '{"props":{"table_def": "CREATE TABLE GeoCheckin (myfamily varchar not null, myseries varchar not null, time timestamp not null, weather varchar not null, temperature double, PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), myfamily, myseries, time))"}}'
```

Please take care:

* The `bucket-type` name **must** equal the table name.
* The syntax is very sensitive to whitespace and quoting.
* It is easy to create a very long bucket type name with no corresponding
  timeseries table if you leave out the space between the bucket type name
  and the opening quote of the JSON properties.
* The table and field names are currently constrained to ASCII.

Also note that if you discover something wrong with the setup of your DDL, you will need to create it again and decide whether to scrap the data in the existing table or move it from the old table to the new one.

## Activating Your Table

You activate your table just like you would activate a bucket type:

```sh
riak-admin bucket-type activate »TABLE NAME«
```

For the example `GeoCheckin` table:

```sh
riak-admin bucket-type activate GeoCheckin
```

## Verify Activation

You can verify that your table was properly created by looking at the `ddl` section of the `riak-admin bucket-type status` response. For example:

```sh
$ riak-admin bucket-type status GeoCheckin
GeoCheckin is active
...
ddl: {ddl_v1,<<"GeoCheckin">>,
             [{riak_field_v1,<<"myfamily">>,1,binary,false},
              {riak_field_v1,<<"myseries">>,2,binary,false},
              {riak_field_v1,<<"time">>,3,timestamp,false},
              {riak_field_v1,<<"weather">>,4,binary,false},
              {riak_field_v1,<<"temperature">>,5,double,true}],
             {key_v1,[{hash_fn_v1,riak_ql_quanta,quantum,
                                  {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]},
                      [{param_v1,[<<"time">>]},15,m],
                                  timestamp},
             {key_v1,[{param_v1,[<<"time">>]},
                      {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]}}
```

## Next Steps

Now that you've created and activated your Riak TS table, you can [write data][writing] to it.
