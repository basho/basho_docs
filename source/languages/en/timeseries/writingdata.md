---
title: Writing Data to Riak TS
project: timeseries
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

Now that you've [configured][configuring]and [activated][activating] a bucket, you are ready to write data to it.

##

Let's use the example bucket from earlier: 

``` 
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
 
Riak TS allows you to write multiple rows of data at a time. Simply put the data in a list:

```
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:put(Pid, "GeoCheckin", [[“family1”, “series1”, 1234567, “hot”, 23.5], [“family2”, “series99”, 1234567, “windy”, 19.8]]).
```
 
The timestamps are in microseconds since ....
**??** What other info does a user need about the above example and writing their own? 

 
If the data is correctly written the response is:
ok **??**Is that the response?
 
**??**What happens if it doesn't work? What are some things the user could look at?
 
Error conditions
 
There are two error conditions:
·  	Writing data to a TS bucket that doesn’t exist
·  	Writing data which doesn’t match the DDL**??** specification of the TS bucket
 