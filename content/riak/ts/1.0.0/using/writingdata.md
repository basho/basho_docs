---
title: "Writing Data to Riak TS"
description: "Writing Data to Riak TS"
menu:
  riak_ts-1.0.0:
    name: "Write Data"
    identifier: "writing_data_riakts"
    weight: 303
    parent: "using"
project: "riak_ts"
project_version: "1.0.0"
lastmod: 2015-12-15T00:00:00-00:00
sitemap:
  priority: 0.1
toc: true
aliases:
    - /riakts/1.0.0/using/writingdata/
---

[activating]: ../creating-activating/
[planning]: ../planning/
[querying]: ../querying/

Now that you've [planned][planning] and [activated][activating] your Riak TS table, you are ready to write data to it.

## Writing Data

Riak TS allows you to write multiple rows of data at a time. To demonstrate, we'll use the example table from earlier:

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

To write data to your table, put the data in a list:

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

```python
import datetime
from riak.client import RiakClient

# NB: modify 'host' and 'pb_port' to match your installation
client = RiakClient(host='myriakdb.host', pb_port=8087)

fiveMins = datetime.timedelta(0, 300)
ts0 = datetime.datetime(2015, 1, 1, 12, 0, 0)
ts1 = ts0 + fiveMins

table = client.table('GeoCheckin')
rows = [
    ['family1', 'series1', ts0, 'hot', 23.5],
    ['family1', 'series1', ts1, 'windy', 19.8]
]
ts_obj = table.new(rows)
print "Store result:", ts_obj.store()
```

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
submission = Riak::TimeSeries::Submission.new client, "GeoCheckin"
submission.measurements = [["family1", "series1", 1234567, "hot", 23.5], ["family2", "series99", 1234567, "windy", 19.8]]
submission.write!
```

>**Note on validation**:
>
>Riak TS 1.0.0 does not have client-side insertion validation. Please take caution when creating data to insert by ensuring that each row’s cells match the order/types of the table, and that you do not create a null-value cell for a non-nullable column.

If all of the data are correctly written the response is: `ok` in Erlang, and will not raise an error in Ruby.

If some of the data failed to write, an `RpbErrorResp` error occurs with a number of failures. In the event that your write fails, you should:

1. Do a binary search with half the data, then the other half, and etc. to pinpoint the problem; or
2. Write the data one at a time until one fails.

You could also try the original write again. Failures may be transitory when servers are temporarily unable to talk to each other.

### Guidelines

* Batches should not be too large. In our testing, 100 rows per write is a sweet spot, but you should expect different results depending on your hardware and schema.
* Writes will assume that columns are in the same order they've been declared in the table.
* Timestamps should be in Unix epoch/UTC milliseconds.

## Next Steps

Now that you've written data to your tables, you can [query][querying] your data.
