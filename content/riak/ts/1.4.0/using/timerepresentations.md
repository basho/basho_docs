---
title: "Time Representations in Riak TS"
description: "Time Representations in Riak TS"
menu:
  riak_ts-1.4.0:
    name: "Time Representations"
    identifier: "time_representations_riakts"
    weight: 303
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/timerepresentations/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/timerepresentations"
---


[activating]: ../creating-activating/
[planning]: ../planning/
[querying]: ../querying/
[config reference]: /riak/kv/2.1.4/configuring/reference/#the-advanced-config-file
[MDC]: /riak/ts/1.4.0/using/mdc
[riakshell]: ../riakshell
[iso8601]: https://en.wikipedia.org/wiki/ISO_8601

Timestamps in Riak TS are stored as positive integers representing the
number of UTC milliseconds since January 1st, 1970 (UNIX epoch
time). As an example: midnight GMT, August 1st 2016, would exist in
the database as the integer `1470009600000`.

Until Riak TS 1.4, inserts and selects via [riak_shell][riakshell] and
other client libraries required these same integer timestamps. To make
the interface friendlier, 1.4 introduced [ISO 8601][iso8601] support.

Timestamps are still stored as UTC milliseconds, but now users can add
and query using string representations.

This feature is better used for learning how Riak TS works and for
other casual use: production applications should instead use the
client libraries to pass timestamps as integers, for performance and
for correctness.

## ISO 8601

Date/time strings in ISO 8601 are relatively straightforward to read
and write. Some examples, all of which represent the same timestamp
(`1470060351425000`):

* `2016-08-01 08:05:51-06`
* `20160801 080551-0600`
* `20160801 100551-0400`
* `20160801 140551Z`
* `20160801T193551+05:30`

When using ISO 8601 strings for `insert` or `select` statements, be sure
to use apostrophes around the string.

### Reduced Accuracy

Strings that do not include seconds are referred to by the standard as
*reduced accuracy* representations. A couple of examples:

* `2016-08`
* `2016-08-05 15:00Z`

Using reduced accuracy for values in an `insert` will result in a
millisecond UTC time that corresponds to the start of the time period
in question. `2016-08` will map to midnight on August 1st 2016, for
example.

See **Reduced Accuracy** below for an explanation of how reduced
accuracy impacts queries.

### Fractional Times

Fractional times are particularly relevant given the millisecond
accuracy in Riak TS timestamps.

The string `2016-08-01 14:05:51.425Z` would be converted to the Riak
TS millisecond timestamp `1470060351425`.

Fractional times are not considered reduced accuracy, so selecting for
timestamps greater than `2016-08-03 15:00` will give different
results than `2016-08-01 15.0` (or `2016-08-01 15:00:00`).

### Unsupported Formats

ISO 8601 includes three notable syntaxes which are not yet supported
in Riak TS: expanded representation (years outside the range
0000-9999), time intervals (a range of times), and week dates (a week
or day expressed as a number of weeks into the year).

## Time Zones

As of Riak TS 1.4 it is possible to define a cluster-wide default time
zone, expressed as an offset from GMT.

Thus far, its impact is limited to interpreting strings used for
`select` or `insert` *that do not include a time zone specification*.

```
$ riak-admin timezone
not configured
$ riak-admin timezone "-05:00"
Success, timezone is now configured to '-05:00'
$ riak-admin timezone
-05:00
```

It is only necessary to do this on one of your cluster members.

In the absence of such a configuration, the default time zone will be GMT.

### IMPORTANT: Daylight Saving Time

You may have noticed a flaw with the default time zone. Defining a
default time zone as a GMT offset means that when DST begins or ends,
the default time zone must be reconfigured for times to be translated
correctly.

Scheduling a cron job to adjust the time zone is advisable, but you
must be careful: different implementations of cron handle DST
transitions differently.

More generally speaking, for performance reasons as well as
correctness, for production use it is better to use client libraries
that pass timestamps as UTC integers than to use `riak-shell` and ISO
8601 strings.

### Time Zone FAQs

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

Aha! This is a feature we *can* safely offer, and will, just not yet.

> Why does `riak-shell` display the results from `select` in GMT?

Riak TS does not yet expose the default time zone to clients.

## Selecting Data

Writing queries with ISO 8601 is simple as long as you do not use
reduced accuracy time representations.

As long as you specify your time down to the second (or use fractional
times) the logic is simple: the ISO 8601 time string is converted to a
millisecond timestamp and the query uses that value.

```
select weather, temperature from GeoCheckin where time > '2009-11-01 03:15:00+07' and time < '2009-11-01 03:45:00+07' and region = 'South Atlantic' and state = 'South Carolina'
```

The timestamps used inside Riak TS for those comparisons will be
1257020100000 and 1257021900000.

The same query, expressed with fractional time:

```
select weather, temperature from GeoCheckin where time > '2009-11-01 03.25+07' and time < '2009-11-01 03.75+07' and region = 'South Atlantic' and state = 'South Carolina'
```

See **Reduced Accuracy** below for what happens when you don't fully
specify your date/time strings.

## Writing Data

Using ISO 8601 strings in `insert` statements is straightforward; be
certain to include apostrophes around the value.

```sql
insert into GeoCheckin values ('South Atlantic','South Carolina','2015-01-01 12:01:40Z','rain',37.8);
```

See **Reduced Accuracy** for an explanation of what happens if you
try the same insert without any seconds:

```sql
insert into GeoCheckin values ('South Atlantic','South Carolina','2015-01-01 12:01Z','rain',37.8);
```

## Reduced Accuracy

Ambiguity can arise with reduced accuracy representations. What,
precisely, do the comparisons `time > '2015'` or `time <= '2003-05-01T05:10'`
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

Strictly greater than and greater-than/equal-to queries involve
incrementing the original reduced accuracy representation before
expansion.

So, `time > '2015'` becomes, in effect, `time >= '2016-01-01
00:00:00'`.

`time <= '2003-05-01T05:10'` becomes `time < '2003-05-01T05:11:00'`.

Thus, querying for timestamp values greater than `'1970-12-18 21:00'`
will ignore any values which fall between 9pm and 9:01pm, while using
a fully-specified string `'1970-12-18 21:00:00'` will include them.
