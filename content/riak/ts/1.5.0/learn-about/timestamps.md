---
title_supertext: "Learn About"
title: "Timestamps"
description: "Timestamps in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "Timestamps"
    identifier: "timestamps_in_riakts"
    weight: 600
    parent: "about"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/learn-about/timestamps/
---

[ISO 8601]: https://en.wikipedia.org/wiki/ISO_8601
[planning partition]: ../../using/planning#partition-key
[UNIX time]: https://en.wikipedia.org/wiki/Unix_time

Timestamps in Riak TS are a critical data type, nearly always a part of [partition key][planning partition]. They are often leveraged with the quantum function to group data by 15 minute clumps, or 10 second clumps, or 60 day clumps, et cetera, depending on how quickly your time series data come in and how you need to analyze them.

## Structure of Timestamps

### Unix Epoch Time

Timestamps in Riak TS are stored as positive integers representing the
number of UTC milliseconds since January 1st, 1970 (UNIX epoch
time).

As an example: midnight GMT, August 1st 2016, would exist in
the database as the integer `1470009600000`.

You can read more about UNIX epoch time [here][UNIX time].

### ISO 8601

For INSERT and SELECT statements, Riak TS supports [ISO 8601] strings which are converted to UTC milliseconds.

{{% note title="A Note on ISO 8601 Support" %}}
ISO 8601 support is useful for casual interactions via riak shell, but we do NOT recommend using ISO 8601 in production applications. Instead, use the TS-supported client libraries to pass timestamps as integers for better performance and correctness.
{{% /note %}}

#### Reduced Accuracy

Strings that do not include seconds are referred to as *reduced accuracy* representations.

Ambiguity can arise with reduced accuracy representations. For instance, what do the comparisons `time > '2015'` or `time <= '2003-05-01T05:10'`
mean when translated to epoch time value as stored in Riak TS?

To resolve those ambiguities, Riak TS expands the reduced accuracy
value to the lowest date/time which matches the specification. `2015`
becomes `2015-01-01 00:00:00`. `2003-05-01T05:10` becomes
`2003-05-01T05:10:00`.

For inserting data, that's the full story. The expanded date/time
value will be converted to epoch milliseconds and stored in the
database.

For querying data, there are two important exceptions to the simple
expand and convert sequence above.

1\. Strictly greater than (>) and greater-than/equal-to (>=) queries involve
incrementing the original reduced accuracy representation before
expansion.

So, `time > '2015'` becomes in effect, `time >= '2016-01-01
00:00:00'`, and 

`time <= '2003-05-01T05:10'` becomes `time < '2003-05-01T05:11:00'`.

Thus, querying for timestamp values greater than `'1970-12-18 21:00'`
will ignore any values which fall between 9pm and 9:01pm, while using
a fully-specified string `'1970-12-18 21:00:00'` will include them.

2\. Fractional times are not considered reduced accuracy, so selecting for
timestamps greater than `2016-08-03 15:00` will give different
results than `2016-08-01 15.0` (or `2016-08-01 15:00:00`).

#### Time Zone FAQs

> Why can't Riak TS default to the operating system's time zone
> instead of GMT if `riak-admin` is not used to set a time zone?

It is critically important that all servers in the cluster agree on
the time zone to be used for translating time strings into
timestamps. Defaulting to the operating system's time zone is only
safe if all servers in the cluster are guaranteed to be configured
identically.

> Why can't I specify my time zone symbolically so I don't have to
> change my default time zone when DST begins and ends?

Effectively for the same reason as question 1. Politicians love to
mess with DST and time zones, and if one server in the cluster fails
to get the latest time zone file updates, timestamps may be
inconsistent depending on which server handles the translation from
text to timestamp.

> Can I set a different time zone for different tables?

Not yet.

> Why does `riak-shell` display the results from `select` in GMT?

Riak TS does not yet expose the default time zone to clients.

#### Unsupported Formats

ISO 8601 includes three notable syntaxes which are not yet supported
in Riak TS:

* expanded representation (years outside the range
0000-9999),
* time intervals (a range of times), and
* week dates (a week or day expressed as a number of weeks into the year).
