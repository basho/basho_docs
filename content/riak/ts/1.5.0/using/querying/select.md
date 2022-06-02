---
title: "SELECT in Riak TS"
description: "Using the SELECT statement in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "SELECT"
    identifier: "select_riakts"
    weight: 100
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/querying/select
---


[aggregate functions]: aggregate-functions/
[arithmetic operations]: arithmetic-operations/
[GROUP BY]: group-by/
[guidelines]: {{<baseurl>}}riak/ts/1.5.0/using/querying/guidelines
[iso8601]: ../../timerepresentations/
[iso8601 accuracy]: {{<baseurl>}}riak/ts/1.5.0/using/timerepresentations/#reduced-accuracy
[ISO 8601]: https://en.wikipedia.org/wiki/ISO_8601
[learn timestamps accuracy]: {{<baseurl>}}riak/ts/1.5.0/learn-about/timestamps/#reduced-accuracy

You can use the SELECT statement in Riak TS to query your TS dataset. This document will show you how to run various queries using `SELECT`.

* See the [guidelines] for more information on limitations and rules for queries in TS.
* See [aggregate functions] to learn how turn a set of rows in your Riak TS table into a value.
* See [arithmetic operations] to see a list of operations available with `SELECT`.
* See [GROUP BY] to learn how to condense rows sharing the same value.


For all of the examples on this page, we are using our standard example GeoCheckin table:

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
)
```

{{% note title="SQL Injection" %}}

When querying with user-supplied data, it is essential that you protect against SQL injection. Please verify the user-supplied data before constructing queries.

{{% /note %}}


## Querying Columns

All queries using `SELECT` must include a 'where' clause with all components.

You can select particular columns from the data to query:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'
```

Client-specific examples:

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "region = 'South Atlantic' and state = 'South Carolina'";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'").issue!
```

```python
fmt = """
select weather, temperature from GeoCheckin where
    time > {t1} and time < {t2} and
    region = 'South Atlantic' and state = 'South Carolina'
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)
```

```csharp
var now = DateTime.UtcNow;
var tenMinsAgo = now.AddMinutes(-10);
var qfmt = "SELECT * FROM GeoCheckin WHERE time > {0} and time < {1} and region = 'South Atlantic' and state = 'South Carolina'";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(tenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(now));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var callback = function(err, resp) {
    // resp.rows and resp.columns
    // have data
};
// NB: t1/t2 are integers representing unix timestamps with
// millisecond resolution
var queryText = "select * from GeoCheckin where time > " + t1 +
                " and time < " + t2 +
                " and region = 'South Atlantic' and state = 'South Carolina'";
var q = new Riak.Commands.TS.Query.Builder()
    .withQuery(queryText)
    .withCallback(callback)
    .build();
cluster.execute(q);
```

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'").
```

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569")
    ->build()
    ->execute();
```

```golang
cmd, err := riak.NewTsQueryCommandBuilder()
    .WithQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569")
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

## Extended Queries

You can extend the query beyond the primary key and use secondary columns to filter results. In this example, we are extending our query to filter based on `temperature`:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0
```

Client-specific examples:

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "region = 'South Atlantic' and state = 'South Carolina' " +
                   "temperature > 27.0";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0").issue!
```

```python
fmt = """
select weather, temperature from GeoCheckin where
    time > {t1} and time < {t2} and
    region = 'South Atlantic' and state = 'South Carolina' and
    temperature > 27.0
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)
```

```csharp
var now = DateTime.UtcNow;
var tenMinsAgo = now.AddMinutes(-10);
var qfmt = "SELECT weather, temperature FROM GeoCheckin WHERE time > {0} and time < {1} and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(tenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(now));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var callback = function(err, resp) {
    // resp.rows and resp.columns
    // have data
};
// NB: t1/t2 are integers representing unix timestamps with
// millisecond resolution
var queryText = "select weather, temperature from GeoCheckin where time > " + t1 +
                " and time < " + t2 +
                " and region = 'South Atlantic' and state = 'South Carolina'" +
                " and temperature > 27.0";
var q = new Riak.Commands.TS.Query.Builder()
    .withQuery(queryText)
    .withCallback(callback)
    .build();
cluster.execute(q);
```

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0").
```

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569 and temperature > 27.0")
    ->build()
    ->execute();
```

```golang
cmd, err := riak.NewTsQueryCommandBuilder()
    .WithQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569 and temperature > 27.0")
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

You can also use `or` when querying against column values, such as `temperature` in our example. Note that the parentheses are required:

```sql
SELECT weather, temperature FROM GeoCheckin WHERE time > 1234560 AND time < 1234569 AND region = 'South Atlantic' AND state = 'South Carolina' AND (temperature > 27.0 OR temperature < 0.0)
```

You cannot use `or` between two complete clauses, since keys cannot be specified twice.

## ISO 8601

It is possible to use [ISO 8601]-compliant date/time strings in SELECT statements instead of integer timestamps:

```
time > '2015-12-11 20:04:37' and time < '2015-12-11 20:04:50'
```

You cannot use [reduced accuracy time representations][iso8601 accuracy]. Instead, you must specify your time down to the second (or use fractional
times).

ISO 8601 time strings are converted to a millisecond timestamp, which the query then uses.


```sql
SELECT weather, temperature FROM GeoCheckin WHERE time > '2009-11-01 03:15:00+07' AND time < '2009-11-01 03:45:00+07' AND region = 'South Atlantic' AND state = 'South Carolina'
```

The timestamps used inside Riak TS for those comparisons will be
1257020100000 and 1257021900000.

The same query, expressed with fractional time:

```sql
SELECT weather, temperature FROM GeoCheckin WHERE time > '2009-11-01 03.25+07' AND time < '2009-11-01 03.75+07' AND region = 'South Atlantic' AND state = 'South Carolina'
```

See [our documentation on ISO 8601 support][iso8601] for more details on how to use ISO 8601.

## IS [NOT] NULL

For queries which operate on ranges which contain columns that are nullable, the
WHERE clause may contain NULL condition predicates.

```sql
_expression_ IS NULL
_expression_ IS NOT NULL
NOT _expression_ IS NULL -- equivalent to: _expression_ IS NOT NULL
NOT _expression_ IS NOT NULL -- equivalent to: _expression_ IS NULL
```

For example, the following query extracts the key column values for all records
within the time range, which do not have a value set for the temperature column:

```sql
SELECT region, state, time
FROM GeoCheckin
WHERE time > 1234560 AND time < 1234569
      AND region = 'South Atlantic' AND state = 'South Carolina'
      AND temperature IS NULL
```

IS NOT NULL is often used to operate on records which are fully populated to the
point that an application requires. Within this context, the above query to
locate records which do not satisfy such a requirement can be utilized to
investigate and subsequently reconcile records which are partially populated.

Another common use of NULL condition predicates is determining the relative
cardinality of the subset not containing a value for the _column_.

## Client NULL Results

In this example, we will show how NULL cell results are returned through each
client with the following query:

```sql
SELECT region, temperature
FROM GeoCheckin
WHERE time > 1234560 AND time < 1234569
      AND region = 'South Atlantic' AND state = 'South Carolina'
      AND temperature IS NULL
```

```java
String queryText =
    "SELECT region, temperature FROM GeoCheckin" +
    "WHERE time > 1234560 AND time < 1234569" +
    "AND region = 'South Atlantic' AND state = 'South Carolina'" +
    "AND temperature IS NULL";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
Row firstRow = queryResult.getRowsCopy().get(0);
List<Cell> cells = firstRow.getCellsCopy();

assertTrue(cells.size() == 2);

assertTrue(cells.get(0) != null);
assertTrue(cells.get(0).hasVarcharValue());
assertTrue(cells.get(0).getVarcharAsUTF8String().equals("South Atlantic"));

assertTrue(cells.get(1) == null); // Null cells are represented as literal nulls in the row object's cell iterator.
```

```ruby
Riak::Timeseries::Query.new(client, "SELECT region, temperature FROM GeoCheckin WHERE time > 1234560 AND time < 1234569 AND region = 'South Atlantic' AND state = 'South Carolina' AND temperature IS NULL").issue!

#=> [["South Atlantic", nil], ["South Atlantic", nil]]
```

```python
fmt = """
SELECT region, temperature FROM GeoCheckin
WHERE time > {t1} AND time < {t2}
AND region = 'South Atlantic' AND state = 'South Carolina'
AND temperature IS NULL
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)

print ts_obj.rows
# [["South Atlantic", None], ["South Atlantic", None]]
```

```csharp
var now = DateTime.UtcNow;
var tenMinsAgo = now.AddMinutes(-10);
var qfmt = "SELECT region, temperature FROM GeoCheckin WHERE time > 1234560 AND time < 1234569 AND region = 'South Atlantic' AND state = 'South Carolina' AND temperature IS NULL";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(tenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(now));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);

QueryResponse response = cmd.Response;
Row firstRow = response.Value.First();
Assert.AreEqual("South Atlantic", firstRow.Cells.ElementAt(0).Value);
Assert.IsNull(firstRow.Cells.ElementAt(1).Value);
```

```javascript
var callback = function(err, resp) {
    // resp.rows and resp.columns
    // have data
    // each row is an array of cells or null values.
    var firstRow = result.rows[0];
    firstRow[0]; // Returns "South Atlantic"
    firstRow[1]; // Returns null
};
// NB: t1/t2 are integers representing unix timestamps with
// millisecond resolution
var queryText = "SELECT region, temperature FROM GeoCheckin " +
                "WHERE time > " + t1 + " AND time < " + t2 +
                " AND region = 'South Atlantic' AND state = 'South Carolina' " +
                " AND temperature IS NULL";
var q = new Riak.Commands.TS.Query.Builder()
    .withQuery(queryText)
    .withCallback(callback)
    .build();
cluster.execute(q);
```

```erlang
riakc_ts:query(Pid, "SELECT region, temperature FROM GeoCheckin WHERE time > 1234560 AND time < 1234569 AND region = 'South Atlantic' AND state = 'South Carolina' AND temperature IS NULL").
%% Returns
%% {ok,{[<<"region">>,<<"temperature">>],
%%     [{<<"South Atlantic">>,[]},{<<"South Atlantic">>,[]}]}}
%%
%% In the Erlang client, null cells are represented as empty lists ([])
```

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("SELECT region, temperature FROM GeoCheckin WHERE time > 1234560 AND time < 1234569 AND region = 'South Atlantic' AND state = 'South Carolina' AND temperature IS NULL")
    ->build()
    ->execute();
$response->getResults()[0][0]->getValue(); // returns "South Atlantic"
$response->getResults()[0][1]->getValue(); // returns null
```

```golang
cmd, err := riak.NewTsQueryCommandBuilder()
    .WithQuery("SELECT region, temperature FROM GeoCheckin WHERE time > 1234560 AND time < 1234569 AND region = 'South Atlantic' AND state = 'South Carolina' AND temperature IS NULL")
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd);

if err != nil {
    return err
}

scmd, ok := cmd.(*TsQueryCommand);

scmd.Response.Rows[0][0].GetStringValue() // returns "South Atlantic"
scmd.Response.Rows[0][1] // returns nil
```
