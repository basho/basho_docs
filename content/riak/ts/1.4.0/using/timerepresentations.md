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
[riak shell]: ../riakshell
[ISO 8601]: https://en.wikipedia.org/wiki/ISO_8601
[learn timestamps accuracy]: riak/ts/1.4.0/learn-about/timestamps#reduced-accuracy
[learn timestamps timezone]: riak/ts/1.4.0/learn-about/timestamps#time-zone-faqs
[Daylight Saving Time]: https://en.wikipedia.org/wiki/Daylight_saving_time



For querying and adding data, however, we support [ISO 8601] string representations. This document will  

{{% note title="NOTE" %}}
We do NOT recommend using this feature in production. Production applications should use the TS-supported client libraries to pass timestamps as integers, as this will ensure better performance and correctness.
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
to use apostrophes around the string.

### Reduced Accuracy

Strings that do not include seconds are referred to by the standard as
*reduced accuracy* representations. A couple examples:

* `2016-08`
* `2016-08-05 15:00Z`

Using reduced accuracy for values in an `INSERT` will result in a
millisecond UTC time that corresponds to the start of the time period
in question. `2016-08` will map to midnight on August 1st 2016, for
example.

See [Learn About Timestamps][learn timestamps accuracy] for an explanation of how reduced
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
in Riak TS:

* expanded representation (years outside the range
0000-9999), 
* time intervals (a range of times), and 
* week dates (a week or day expressed as a number of weeks into the year).


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

There is a flaw with the default time zone. Defining a
default time zone as a GMT offset means that when [Daylight Saving Time] begins or ends,
the default time zone must be reconfigured for times to be translated
correctly.

You can schedule a cron job to adjust the time zone, but be aware that different implementations of cron handle DST
transitions differently.

{{% note title="REMINDER" %}}
We do NOT recommend using ISO 8601 strings in production. Production applications should use the TS-supported client libraries to pass timestamps as integers, as this will ensure better performance and correctness.
{{% /note %}}

For more information on time zones, see [Learn About Timestamps][learn timestamps timezone].