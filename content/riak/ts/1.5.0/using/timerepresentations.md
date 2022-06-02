---
title: "Time Representations in Riak TS"
description: "Time Representations in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "Time Representations"
    identifier: "time_representations_riakts"
    weight: 301
    parent: "using"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/timerepresentations/
---


[activating]: ../creating-activating/
[planning]: ../planning/
[querying]: ../querying/
[config reference]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference/#the-advanced-config-file
[MDC]: {{<baseurl>}}riak/ts/1.5.0/using/mdc
[riak shell]: ../riakshell
[ISO 8601]: https://en.wikipedia.org/wiki/ISO_8601
[learn timestamps]: ../../learn-about/timestamps
[learn timestamps epoch]: ../../learn-about/timestamps#unix-epoch-time
[learn timestamps accuracy]: ../../learn-about/timestamps#reduced-accuracy
[learn timestamps timezone]: ../../learn-about/timestamps#time-zone-faqs
[Daylight Saving Time]: https://en.wikipedia.org/wiki/Daylight_saving_time

Timestamps are stored in Riak TS as [epoch milliseconds][learn timestamps epoch], but TS also supports [ISO 8601] string representations. This document will present the basics, but we highly recommend reading [Learn About Timestamps][learn timestamps] for all the details. This is a *time* series database, after all.

{{% note title="A Note on ISO 8601 Support" %}}
ISO 8601 support is useful for casual interactions via riak shell, but we do NOT recommend using ISO 8601 in production applications. Instead, use the TS-supported client libraries to pass timestamps as integers for better performance and correctness.
{{% /note %}}

## ISO 8601 Guidelines

Date/time strings in [ISO 8601] are relatively straightforward to read
and write. Some examples, all of which represent the same timestamp
(`1470060351425000`):

* `2016-08-01 08:05:51-06`
* `20160801 080551-0600`
* `20160801 100551-0400`
* `20160801 140551Z`
* `20160801T193551+05:30`

When using ISO 8601 strings for INSERT or SELECT statements, be sure
to use single quotes around the string.

### Reduced Accuracy

Strings that do not include seconds are referred to by the standard as
*reduced accuracy* representations. A couple examples:

* `2016-08`
* `2016-08-05 15:00Z`

Using reduced accuracy for values in an `INSERT` will result in a
millisecond UTC time that corresponds to the start of the time period
in question. `2016-08` will map to midnight on August 1st 2016, for
example.

See [Learn About Timestamps][learn timestamps accuracy] for the implications of using reduced
accuracy in queries.

### Fractional Times

Fractional times are particularly relevant given the millisecond
accuracy in Riak TS timestamps.

The string `2016-08-01 14:05:51.425Z` would be converted to the Riak
TS millisecond timestamp `1470060351425`.

## Time Zones

It is possible to define a cluster-wide default time
zone, expressed as an offset from GMT.

Its impact is limited to interpreting strings used for
`SELECT` or `INSERT` that do not include a time zone specification.

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

There is a caveat when setting the default time zone. Defining a
default time zone as a GMT offset means that when [Daylight Saving Time] begins or ends,
the default time zone must be reconfigured for times to be translated
correctly.

You can schedule a cron job to adjust the time zone, but be aware that different implementations of cron handle DST
transitions differently.

For more information on time zones, see [Learn About Timestamps][learn timestamps timezone].

### Leap seconds and quantum boundaries

Periodically [leap seconds](https://en.wikipedia.org/wiki/Leap_second)are announced. These are inserted at the end of one day (in UTC).

UNIX treats them as one double-length second. For example, at the end of 1998 a second was added:

```
Date         Time of day   UNIX time
1998-12-31   23:59:58      915148798
1998-12-31   23:59:59      915148799
1998-12-31   23:59:60      915148800     <== Artificial leap second
1999-01-01   00:00:00      915148800
```

Effectively, there is no way in the UNIX time scheme to differentiate an event that occurred during the extra second at the end of 1998 to something that occurred the first second of 1999.

Similarly, Riak TS would treat `915148800` as the start of a new time quantum, and any data points which a client added for that second would be considered to be in the first time quantum in 1999.

The data is not lost, but a query against 1998 time quanta will not produce those data points despite the fact that some of the events flagged as `915148800` technically occurred in 1998.