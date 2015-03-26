---
title: Usage and Billing Data
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
index: true
audience: intermediate
keywords: [developer]
---

Like many other object storage systems, Riak CS gathers a variety of
usage statistics and makes them available through its administrative
API.

<div class="note">
<div class="title">Note on terminology</div>
In this and other documents in the Riak CS documentation, the terms
"storage" and "billing" are used interchangeably. The same goes for the
terms "usage" and access.
</div>

## Access Statistics

Access stats are tracked on a per-user basis, as rollups for slices of
time. They are stored just like other Riak CS data, in the `cs.access`
bucket in particular. For information about querying access statistics,
please read [[Querying Access Statistics]].

## Overview

The basic process driving usage and billing data in Riak CS is the
following:

1. Riak CS determines whom, if anyone, should be billed for each access
2. Riak CS send this and some statistical information about the
   accesses to an aggregation subsystem
3. The aggregation subsystem periodically sends its accumulated log to
   be archived
4. The archival subsystem sums all recorded accesses for each user and
   stores a record for each user for the time slice

Log retrieval then involves simply making a request to Riak for all
slice objects for a user in a time period. No access data will be logged
unless the user for the access is known.

### Tracked Statistics

Several statistics are logged automatically is a user is specified for
the request:

* `Count` --- the number of times this operation was used, where each
  request counts as one (1)
* `BytesIn` --- the number of bytes that were included in the request
  body
* `BytesOut` --- the number of bytes that were sent in the response body

For successful requests, each of these stats is logged under the name
given. For unsuccessful requests, they are logged under this name with a
prefix of either `SystemError` for requests that end in response codes
500+, or `UserError` for requests that end in response codes 400-499.
For example, if a user tries to download a nonexistent file, it will be
logged under `UserErrorCount` with the bytes of the message logged under
`UserErrorBytesOut`.

These three metrics are logged for each operation separately. The access
logger determines the operation type by comparing the method, resource
module, and path to a known table. For example, it knows that a `GET` on
the *key* module with the `acl` query parameter in the path is a
`KeyReadACL` operation. A `PUT` to the same resource without the `acl`
query parameter is a `KeyWrite` operation. See [[Querying Access
Statistics]] for a list of all operation types.

### Log Accumulation

As resources finish their processing, the access logger module is called
by Webmachine to log the access. This module implements a server that
finds all of the access notes in the request's log data and stores them
until the current interval ends.

When the current interval ends, the access module transfers ownership of
its accumulated data to the archiver module. The logger module then
resets for logging the next slice's accesses.

#### Interval Duration

The length of the log flushing interval is configured by the application
environment variable `access_log_flush_factor`. The value is expressed
as an integer divisor of the `access_archive_period` setting. That is,
if `access_log_flush_factor` is 5 and `access_archive_period` is 3600
(== 1 hour) seconds, the log will be flushed every 720 seconds (== 12
minutes), which is 5 times per archive period.

The value of `access_log_flush_factor` must be an integer factor of
`access_archive_period`. If the factor does not divide the period
evenly, an error will be printed in the log, and the Riak CS node will
refuse to start.

The default value for `access_log_flush_factor` is 1 (once per archive
period). These settings may be manipulated in the Riak CS `app.config`
file, normally located at `/etc/riak-cs/app.config`.

#### Log Size Trigger for Archival

Archival of the access log will also be triggered if the number of
records waiting to be archived reaches a certain configured level. When
the threshold is reached, all accumulated records are transferred to the
archiver, which writes out a sample with *now* as the end-time.
Accumulation is then restarted with *now* as the start time, and will
continue until either the end of the time interval or until the log
threshold is reached again.

This level is configured by the application environment variable
`access_log_flush_size`. Its default value is `1000000` (one million).

#### Backlog Caveat

If the logger finds itself so far behind that it would need to schedule
its next archival in the past---that is, after sending a log
accumulation for interval N to the archiver, it finds that the end of
interval N+1 has already passed---the logger will drop the backlog in
its message box by exiting and allowing its supervisor process to
restart it. Just before exiting, it will print an error message
describing how far behind it was:

```log
09:56:02.584 [error] Access logger is running 302 seconds behind, skipping 0 log messages to catch up
```

With the default one-hour archive period, this case will only be
encountered when the logger is an entire hour behind. This behavior is
meant as a safety valve to prevent that hour lag from growing due to
memory pressure from the logger processes's message queue.

#### Manually Triggering Archival

When taking a machine out of service, it may be desirable to trigger log
archival before the end of the interval. To do so, use the
`riak-cs-access` script with the command `flush`. It should be installed
on the same path as the `riak-cs` script. For most OS distributions this
will be at `/usr/local/sbin`.

By default, the script will wait up to 50 seconds for the logger to
acknowledge that it has passed its accumulation to the archiver and
another 50 seconds for the archiver to acknowledge that it has finished
archiving all accumulations it has received. To wait longer, use the
`-w` parameter on the command line with an integer number of 5-second
intervals to wait. That is, to wait for 100 seconds for each phase, use:

```bash
riak-cs-access flush -w 20
```

### Archive Retrieval

When a request is received for a user's access stats over some time
period, the objects for all intervals in that time period must be
retrieved.

It is important to note that the archival process does not attempt a
read/modify/write cycle when writing a slice record. The `cs.access`
bucket should have the `allow_mult=true` flag set, and so multiple Riak
CS nodes writing the same slice record for the same user create
siblings. Riak CS attempts to check and set the `allow_mult` bucket
property when it starts up, and will print a warning in the log about
being `unable to configure` or `unable to verify` bucket settings if it
fails.

Siblings should be handled at read time. Sibling resolution should be
nothing more than a set union of all records. The HTTP resource serving
the statistics expects to provide them on a node-accumulated basis, so
it is important to set a **unique Erlang node name for each Riak CS
node**.

## Storage Statistics

Storage statistics are also tracked on a per-user basis, as rollups for
slices of time. They are stored in the same Riak cluster as other Riak
CS data, in the `cs.storage` bucket.

For detailed information about querying storage statistics, please read
[[Querying Storage Statistics]].

### High Level

1. Storage is calculated for all users either
    a.  on a regular schedule or
    b.  when manually triggered with the `riak-cs-storage` script
2. Each user's sum is stored in an object named for the timeslice in
   which the aggregation took place
3. Sums are broken down by bucket

Log retrieval is then simply making a request to Riak for all slice
objects for a user in a particular time period.

#### Prerequisite: Code Paths for MapReduce

The storage calculation system uses MapReduce to sum the files in a
bucket. This means you must tell all of your Riak nodes where to find
Riak CS's compiled files before calculating storage.

See [[Configuring Riak for CS]] for directions on setting this up.

### Scheduling and Manual Triggering

Triggering the storage calculation is a matter of setting up a regular
schedule or manually starting the process via the `riak-cs-storage`
script.

#### Regular Schedules

If you would like to have an Riak CS node calculate the storage used by every
user at the same time (or times) each day, specify a schedule in that node's
Riak CS `riak-cs.conf` file, or in the old-style `advanced.config` or
`app.config` file.

In the `riak_cs` section of the file, add an entry for
`storage_schedule` like this:

```riakcsconf
stats.storage.schedule.1 = "06:00"
```

```advancedconfig
{storage_schedule, "0600"}
```

```appconfig
{storage_schedule, "0600"}
```

The time is given as a string of the form `HH:MM` (omit the `:` in the old-style
`advanced.config`/`app.config` files, so it's simply `HHMM`), representing the
hour and minute GMT to start the calculation process. In this example, the node
would start the storage calculation at 6am GMT every day.

To set up multiple times, simply specify multiple times. For example,
to schedule the calculation to happen at both 6am and 6pm, use:

```riakcsconf
stats.storage.schedule.1 = "06:00"
stats.storage.schedule.2 = "18:00"
```

```advancedconfig
{storage_schedule, ["0600", "1800"]}
```

```appconfig
{storage_schedule, ["0600", "1800"]}
```

<div class="note">
<div class="title">Note on archive periods</div>
When using multiple times in a storage schedule, they must be scheduled
for different archive periods (see details for `storage_archive_period`
in the **Archival** section below). Extra scheduled times in the same
archive period are skipped. This is intended to allow more than one Riak
CS node to calculate storage statistics concurrently, as they will take
notice of users already calculated by other nodes and skip them (see
details in the Manual Triggering section about overriding this
behavior).
</div>

By default, no schedule is specified, so the storage calculation is
never done automatically.

#### Manual Triggering

If you would rather trigger storage calculations manually, simply use
the `batch` command in the `riak-cs-storage` script:

```bash
riak-cs-storage batch
# Response:
# Batch storage calculation started.
```

If there is already a calculation in progress, or if starting the
calculation fails for some other reason, the script will print an error
message saying so.

By default, a manually triggered calculation run will skip users that
have already been calculated in the current archive period (see the
Archival section below for details about `storage_archive_period`). If
you would rather calculate an additional sample for every user in this
period, add the `--recalc` (or `-r` for short) option to the command
line:

```bash
riak-cs-storage batch -r # force recalculation of every user
```

#### Further Control

In-process batch calculations can also be paused or canceled using the
`riak-cs-storage` script.

To pause an in-process batch, use:

```bash
riak-cs-storage pause
# Response:
# The calculation was paused.
```

To resume a paused batch, use:

```bash
riak-cs-storage resume
# Response:
# The calculation was resumed.
```

To cancel an in-process batch (whether *paused* or *active*), use:

```bash
riak-cs-storage cancel
# Response:
# The calculation was canceled.
```

You can also retrieve the current state of the daemon by using the
`status` command. The first line will indicate whether the daemon is
*idle*, *active*, or *paused*, and it will be followed by further
    details based on progress. For example:

```log
A storage calculation is in progress
Schedule: none defined
Last run started at: 20120316T204135Z
Current run started at: 20120316T204203Z
Next run scheduled for: unknown/never
Elapsed time of current run: 3
Users completed in current run: 1
Users left in current run: 4
```

### Results

When the node finishes calculating every user's storage, it will print a
message to the log noting how long the entire process took:

```log
08:33:19.282 [info] Finished storage calculation in 1 seconds.
```

### Process

The calculation process is coordinated by a long-lived finite state
machine process that handles both the scheduling (if a schedule is
defined) and the running of the process.

When a storage calculation starts, the first step is to obtain a list of
known users of the system. Each user's record contains information about
the buckets that the user owns.

For each bucket that a user owns, a MapReduce query is run. The query's
inputs are the list of the keys in the bucket (the input is
`BucketName`, so the keys stay on the server). The query then has two
phases: a map that produces tuples of the form `{1,
ByteSize(File)}`---if *active*; nothing if *inactive*---and a reduce
that sums those tuples element-wise. The result is one tuple whose first
element is the number of files in the bucket and whose second element is
the total number of bytes stored in that file.

Only one bucket is calculated at a time to prevent putting too much load
on the Riak cluster. Only one user is calculated at a time as well to
prevent too large of a temporary list on the Riak CS node.

Once the sum for each of the user's buckets is calculated, a record is
written to the `cs.storage` Riak bucket.

### Archival

Records written to the `cs.storage` bucket are very similar to records
written to the `cs.access` bucket used for logging access statistics.
The value is a JSON object with one field per bucket. The key is a
combination of the user's `key_id` and the timestamp of the time slice
for which the calculation was run.

The period for storage archival is separate from the period for access
archival. The storage archival period is configured by the application
environment variable `storage_archive_period`. The default is 86400 (one
day). This is because storage calculations are expected to be archived
much less frequently than access logs, and so specifying fewer possible
keys to look up later reduces overhead at reporting time.
