---
title: "Creating Your Riak TS Table"
description: "Creating and Activating Your Riak TS Table"
menu:
  riak_ts-1.5.0:
    name: "Create Your Table"
    identifier: "creating_activating_riakts"
    weight: 302
    parent: "using"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/creating-activating/
---


[csharp]: ../../developing/csharp#query
[describe]: ../querying/describe/
[erlang]: ../../developing/erlang/#query-2
[java]: ../../developing/java#query
[nodejs]: ../../developing/nodejs/#query
[php]: ../../developing/php#query
[python]: ../../developing/python#query
[ruby]: ../../developing/ruby#sql-queries
[planning]: ../planning/
[writing]: ../writingdata/
[Riak bucket properties]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference/#default-bucket-properties


Once you have [planned out your table][planning] you can create it by:

* Executing a CREATE TABLE statement using any Riak TS client, 
* Using `riak-shell`, or
* Running the `riak-admin` command (as root, using `su` or `sudo`).

Throughout this document, we will again be using the example table:

```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
);
```


## `CREATE TABLE` in Client Library

Using one of the Riak TS client libraries, execute the CREATE TABLE statement via that library's query functionality. This will create and activate the table in one step. 

```csharp
string tableName = "GeoCheckin";
string sqlFmt = string.Format(
    @"CREATE TABLE {0} (region varchar not null,
                        state varchar not null,
                        time timestamp not null,
                        weather varchar not null,
                        temperature double,
    PRIMARY KEY((region, state, quantum(time, 15, m)), region, state, time))", tableName);

var cmd = new Query.Builder()
    .WithTable(tableName)
    .WithQuery(sqlFmt)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```erlang
Sql = <<"CREATE TABLE GeoCheckin...">>,
Result = riakc_ts:query(Pid, Sql).
```

```golang
const tsTableDefinition = `
	CREATE TABLE %s (
		region varchar not null,
		state varchar not null,
		time timestamp not null,
		weather varchar not null,
		temperature double,
		uv_index sint64,
		observed boolean not null,
		PRIMARY KEY((region, state, quantum(time, 15, 'm')), region, state, time)
	)`
```

```http
$ curl -XPOST http://127.0.0.1:8098/ts/v1/query --data "CREATE TABLE GeoCheckin (state VARCHAR NOT NULL, city VARCHAR NOT NULL, time TIMESTAMP NOT NULL, weather VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((state, city, QUANTUM(time, 15, 'm')), state, city, time))"

{"success":true}
```

```java
RiakClient client = RiakClient.newClient(10017, "myriakdb.host");

String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "region = 'South Atlantic' and state = 'South Carolina'";

Query query = new Query.Builder(queryText).build();

// With the synchronous execute, any errors encountered will be thrown.
QueryResult queryResult = client.execute(query);

// With the executeAsync method, any errors will be stored for review.
final RiakFuture<QueryResult, String> queryFuture = client.executeAsync(storeCmd);
bool success = queryFuture.isSuccess();
QueryResult result = queryFuture.get();
Throwable error = queryFuture.cause();
```

```nodejs
var Riak = require('basho-riak-client');

//may pass client an array of host:port's
//['192.168.1.1:8087','192.168.1.2:8087']
var client = new Riak.Client(['127.0.0.1:8087']);

var key = [ 'South Carolina', 'South Carolina', now ];

var cb = function (err, rslt) {
    // NB: rslt will be an object with two properties:
    // 'columns' - table columns
    // 'rows' - row matching the Get request
};

var cmd = new Riak.Commands.TS.Get.Builder()
    .withTable('GeoCheckin')
    .withKey(key)
    .withCallback(cb)
    .build();

client.execute(cmd);
```

```php
require __DIR__ . '/../vendor/autoload.php';

use Basho\Riak;
use Basho\Riak\Command;
use Basho\Riak\Node;

$node = (new Node\Builder)
    ->atHost('riak-test')
    ->onPort(8087)
    ->build();

$riak = new Riak([$node], [], new Riak\Api\Pb());


# create table
$table_definition = "
    CREATE TABLE %s (
        region varchar not null,
        state varchar not null,
        time timestamp not null,
        weather varchar not null,
        temperature double,
        PRIMARY KEY((region, state, quantum(time, 15, 'm')), region, state, time)
    )";

$command = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery(sprintf($table_definition, "GeoCheckins"))
    ->build();

if (!$response->isSuccess()) {
    echo $response->getMessage();
    exit;
}
```

```python
def test_query_that_creates_table_using_interpolation(self):
        table = self.randname()
        query = """CREATE TABLE test-{table} (
            geohash varchar not null,
            user varchar not null,
            time timestamp not null,
            weather varchar not null,
            temperature double,
            PRIMARY KEY((geohash, user, quantum(time, 15, m)),
                geohash, user, time))
```

```ruby
  let(:create_table) do
    <<-SQL
CREATE TABLE timeseries-#{random_key} (
    geohash varchar not null,
    user varchar not null,
    time timestamp not null,
    weather varchar not null,
    temperature double,
    PRIMARY KEY(
        (geohash, user, quantum(time, 15, m)),
        geohash, user, time
    )
)
SQL
  end
```


The result of the operation is library-dependent:

* [Java][java]: the `QueryResult` object will be returned without any data for rows or columns.
* [Ruby][ruby]: no exception thrown and result collection is empty.
* [Python][python]: no exception thrown; result object is present with `rows` and `columns` being empty.
* [C#][csharp]: no exception thrown; result object is present with `Value` and `Columns` being empty.
* [Node.js][nodejs]:  no exception thrown; result object is present with `rows` and `columns` being empty.
* [Erlang][erlang]: the returned term will consist of two empty lists `{[],[]}`.
* [PHP][php]: the response object has a boolean `isSuccess()` instance method.


### Using `WITH`

Your data definition language (DDL) may have an optional WITH clause, where any table properties can be specified:

```sql
CREATE TABLE (...) WITH (
    n_val=5, key2 = 'string value2',
    prop_with_quotes='single '' quote here',
    custom_prop = 42.24)
```

Any property with any string or numeric value can be associated with a table, including but not limited to standard [Riak bucket properties]. 

Please note the following when using `WITH`:

* The property values can be of numeric or string types (parseable as `sint64`, `double` or `varchar`, correspondingly). String values should be quoted with a `'`; literal single quote characters appearing in the string should be doubled (and not escaped with a `\`).
* Values from the WITH clause will override those specified outside the query statement.
* The default `n_val` (the number of distinct copies of each record kept in your cluster for safety and availability) is 3. This default cannot be changed; instead, each time a table is created the WITH clause can be used to configure that table's `n_val`.


#### Time Series-Specific Bucket Properties

There are a few default bucket property differences between Riak KV and Riak TS.  These differences are centered around improving performance.
Explanations of these values can be found at [Riak bucket properties].

**Property** | **KV** | **TS**
:------:|:----:|:---:
`allow_mult` | `true` | `false`
`dvv_enabled` | `true` | `false`
`dw` | `quorum` | `one`
`last_write_wins` | `false` | `true`
`r` | `quorum` | `one`
`rw` | `quorum` | `one`


### Verification

You can verify that your table was properly created by executing the [DESCRIBE statement][describe] via the query function of your client library, or by using the [`riak-admin bucket-type status` command](#verify-creation-and-activation).



## `CREATE TABLE` in the riak shell

You can use the riak shell to create a table by running:

```
riak-shell>CREATE TABLE GeoCheckin (id SINT64 NOT NULL, region VARCHAR NOT NULL, state VARCHAR NOT NULL, time  TIMESTAMP NOT NULL, weather  VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((id, QUANTUM(time, 15, 'm')), id, time));
```

Please take care with the following:

* The syntax is sensitive to whitespace and quoting.
* The table and column names are currently constrained to ASCII.


### Verification

You can verify that your table was properly created by executing the [DESCRIBE statement][describe] in the riak shell.


## `CREATE TABLE` using `riak-admin`

> We recommend creating a table using [the riak shell](#create-table-in-riak-shell) or one of our supported [client libraries](#create-table-in-client-library).

To create the example table, first run:

(**Note: Mac OS X users can skip this step**)

```bash
sudo su riak
```

This will put you in a shell as the riak user. Then run:

```bash
riak-admin bucket-type create GeoCheckin '{"props":{"table_def": "CREATE TABLE GeoCheckin (id SINT64 NOT NULL, region VARCHAR NOT NULL, state VARCHAR NOT NULL, time TIMESTAMP NOT NULL, weather VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((id, QUANTUM(time, 15, 'm')), id, time))"}}'
```

Please take care with the following:

* The `bucket-type` name must equal the table name.
* The syntax is very sensitive to whitespace and quoting.
* It is easy to create a very long bucket type name with no corresponding
  TS table if you leave out the space between the bucket type name
  and the opening quote of the JSON properties.
* The table and column names are currently constrained to ASCII.

Also note that if you discover something wrong with the setup of your data definition language (DDL), you will need to create it again and decide whether to scrap the data in the existing table or move it from the old table to the new one.


### Activating Your Table

You activate your table as follows:

```bash
riak-admin bucket-type activate »TABLE NAME«
```

For the example `GeoCheckin` table:

```bash
riak-admin bucket-type activate GeoCheckin
```


### Verify Creation and Activation

You can verify that your table was properly created by looking at the `ddl` section of the `riak-admin bucket-type status` response. For example:

```bash
$ riak-admin bucket-type status GeoCheckin
GeoCheckin is active
...
ddl: {ddl_v2,<<"GeoCheckin">>,
             [{riak_field_v1,<<"id">>,1,sint64,false},
              {riak_field_v1,<<"region">>,2,varchar,false},
              {riak_field_v1,<<"state">>,3,varchar,false},
              {riak_field_v1,<<"time">>,4,timestamp,false},
              {riak_field_v1,<<"weather">>,5,varchar,false},
              {riak_field_v1,<<"temperature">>,6,double,true}],
             {key_v1,[{param_v2,[<<"id">>],undefined},
                      {hash_fn_v1,riak_ql_quanta,quantum,
                                  [{param_v2,[<<"time">>],undefined},15,m],
                                  timestamp}]},
             {key_v1,[{param_v2,[<<"id">>],undefined},
                      {param_v2,[<<"time">>],undefined}]},
             v1}
```


## Editing Your Table

Once created, you cannot edit your Riak TS table. If you discover something wrong with the setup of your Riak TS table, you will need to create it again. You will also need to decide whether to scrap the data in the existing table or move it from the old table to the new one.


## Next Steps

Now that you've created and activated your Riak TS table, you can [write data][writing] to it.
