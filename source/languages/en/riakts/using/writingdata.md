---
title: Writing Data to Riak TS
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

Now that you've [configured][configuring]and [activated][activating] a bucket, you are ready to write data to it.

## Writing Data

Let's use the example bucket from earlier:

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

Riak TS allows you to write multiple rows of data at a time. Simply put the data in a list:

>**Note on client-side validation**:
>Riak Time Series 1.0 does not have client-side insertion validation. The TS server (if enabled), checks if inserted rows and cells are the correct type, order, and nullability. 
>These checks will be available in Riak Time Series 1.1 clients. Until then please take caution when creating data to insert. Ensure each row’s cells match the order/types of the table, and that you do not create a null-value cell for a non-nullable column.

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
submission = Riak::TimeSeries::Submission.new client, "GeoCheckin"
submission.measurements = [["family1", "series1", 1234567, "hot", 23.5], ["family2", "series99", 1234567, "windy", 19.8]]
submission.write!
```

```erlang
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:put(Pid, "GeoCheckin", [[<<"family1">>, <<"series1">>, 1234567, <<"hot">>, 23.5], [<<"family2">>, <<"series99">>, 1234567, <<"windy">>, 19.8]]).
```

```java
RiakClient client = RiakClient.newClient(10017, "myriakdb.host");
List<Row> rows = Arrays.asList(
    new Row(new Cell("family1"), new Cell("series1"), 
            Cell.newTimestamp(1234567), new Cell("hot"), new Cell(23.5)),
    
    new Row(new Cell("family2"), new Cell("series99"),
            Cell.newTimestamp(1234567), new Cell("windy"), new Cell(19.8)));

Store storeCmd = new Store.Builder("GeoCheckin").withRows(rows).build();
client.execute(storeCmd);
```

In the absence of information about which columns are provided, writing data assumes that columns are in the same order they've been declared in the table.

The timestamps should be in UTC microseconds.

If all of the data are correctly written the response is:
`ok` in Erlang, and will not raise an error in Ruby.

If some of the data failed to write, **??** do we really get an `RpbErrorResp`
with a number that failed but no guidance about which ones??!

**??** What happens if it doesn't work? What are some things the user could look at?

## Error conditions

There are two error conditions:

* Writing data to a TS bucket that doesn’t exist
*	Writing data which doesn’t match the specification of the TS bucket
