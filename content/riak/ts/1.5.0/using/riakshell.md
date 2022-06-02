---
title: "Using riak shell"
description: "Using riak shell"
menu:
  riak_ts-1.5.0:
    name: "riak shell"
    identifier: "riak_shell"
    weight: 309
    parent: "using"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/riakshell/
---

[nodename]: {{<baseurl>}}riak/kv/2.2.0/using/cluster-operations/changing-cluster-info/
[creating]: {{<baseurl>}}riak/ts/1.5.0/using/creating-activating
[writing]: {{<baseurl>}}riak/ts/1.5.0/using/writingdata
[riak shell README]: https://github.com/basho/riak_shell/blob/develop/README.md


You can use riak shell within Riak TS to run SQL and logging commands from one place.


## Capabilities

The following are supported in riak shell: 

* logging
* log replay
* log regression replay
    * you can run replay and regression logs in batch mode by specifying a file of commands to replay
* extensible architecture
* history
* configuration
* specification of alternative configuration files at run time
* SQL mode
* batch mode
* management of connections to remote Riak nodes
* shell management (including cookies)
* debugging

The shell is also trivially extendable for developer use.


## Getting Started

To get started using riak shell:

1\. Upon installing Riak TS from a package (.deb or .rpm), your riak shell should be configured to attach to your local node. You can verify this by running:

```
> sudo riak-shell
```

1a\. You can attach to any node in your cluster from riak shell. To do this, locate your riak_shell.config file. On most systems, it will be in the `/etc/riak` directory with the other Riak TS configuration files. On Mac OS X, the configuration files are in the `~/riak-ts-1.5.0/etc` directory. Open riak_shell.config, and add the nodename and IP addresses you wish to connect to to nodes:

```
[
 {riak_shell, [
              {logging, off},
              {cookie, riak},
              {show_connection_status, false},
              {nodes, [
                       'dev1@127.0.0.1',
                       'yournodename@youripaddress'
                      ]}
             ]}
].
```

2\. Open riak shell (if you have updated riak_shell.config, you will need to navigate back to your Riak TS directory):

```bash
riak-shell
```

You can verify your connection by running `show_connection`:

```
riak-shell>show_connection;
```

You should see a reply like this one:

```
riak_shell is connected to: 'dev1@127.0.0.1' on port 8087
```

## Basic Commands

{{% note %}}
You must always conclude riak shell commands with `;` or your command will not work. Every example in this document ends a command with `;`.
{{% /note %}}

### Connecting and reconnecting

You can connect riak shell to multiple nodes.

{{% note title="Warning" %}}
You cannot run more than one shell per machine. If you try to connect two shells to a single machine, you will receive an error message.
{{% /note %}}

To connect to a specific node, run: `connect '»nodename«'`

```
riak-shell>connect 'dev2@127.0.0.1';
"Trying to connect..."
```

To show all the nodes you are connected to, run `show_nodes`. You will see something like this:

```
riak-shell>show_nodes;
The connected nodes are: ['dev1@127.0.0.1','dev2@127.0.0.1']
```

You can reconnect riak shell by running: `reconnect`. This will try to connect you to one of the nodes listed in your riak_shell.config. Each node will be tried until it succeeds or runs out of nodes to try.

You can toggle a connection prompt to show the connect status in the prompt:

```
riak-shell>connection_prompt on;
Connection Prompt turned on
riak-shell>connection_prompt off;
Connection Prompt turned off
```


### History and repeating past commands

You can see the log of your activity in riak shell by running `show_history`

```
riak-shell>show_history;
The history contains:
- 1: show_connection;
- 2: connect dev2@127.0.0.1;
- 3: show_connection;
- 4: help connection reconnect;
- 5: reconnect;
- 6: show_connection;
- 7: help history h;
- 8: h 9;
```

You can re-run a past command by running `h »number of command to re-run«`

For instance:

```
riak-shell>h 6;
rerun (6)> show_connection;
riak-shell is connected to: 'dev2@127.0.0.1' on port 8097
```


### SQL

`riak-shell` now has SQL help built in. To get a list of all supported SQL statement use the `help sql;` command:
```
riak-shell>help sql;
The following SQL help commands are supported:
CREATE   - using CREATE TABLE statements
DELETE   - deleting data with DELETE FROM
DESCRIBE - examining table structures
EXPLAIN  - understanding SELECT query execution paths
INSERT   - inserting data with INSERT INTO statements
SELECT   - querying data
SHOW     - listing tables

SELECT can be used with ORDER BY, GROUP BY and LIMIT clauses. It supports arithmetic on column values and has a variety of aggregation functions: COUNT, SUM, MEAN, AVG, MAX, MIN, STDDEV, STDDEV_SAMP and STDDEV_POP

To get more help type `help SQL SELECT` (replacing SELECT with another statement as appropriate)
```

You can get more details on each type of statement by appending the statement name to the help command `help sql insert;`:
```
riak-shell>help sql insert;
You can use the INSERT INTO statement to insert data into a Time Series table.
There are two formats that the INSERT INTO statement can use.

(this example uses the table definition from 'help SQL CREATE')

An example of the first format is shown below:

(1)>INSERT INTO mytable VALUES ('keyvalue', '2016-11-30 19:30:00', 123, 12.3, false);
Using this format you have to provide values for all columns - including those that can contain nulls.

An example of the second format is shown below:

(2)>INSERT INTO mytable (keyfield, timefield, otherfield1, otherfield2) VALUES ('keyvalue', '2016-11-30 19:30:00', 123, 12.3);
In both of these formats multiple rows of data can be specified

(3)>INSERT INTO mytable VALUES ('keyvalue', '2016-11-30 19:30:00', 123, 12.3, false), ('newvalue', '2016-11-30 19:31:04' 456, 45.6, true);
For more details please go to /riak/ts
```


You can use riak shell to [create a table][creating]:

```
riak-shell>CREATE TABLE GeoCheckin (state VARCHAR NOT NULL, time TIMESTAMP NOT NULL, weather  VARCHAR NOT NULL, temperature DOUBLE, rawdata BLOB, PRIMARY KEY ((state, QUANTUM(time, 15, 'm')), state, time));
```

Then, you can see the table in riak shell:

```
riak-shell>describe GeoCheckin;
+-----------+---------+--------+-------------+---------+--------+----+----------+
|  Column   |  Type   |Nullable|Partition Key|Local Key|Interval|Unit|Sort Order|
+-----------+---------+--------+-------------+---------+--------+----+----------+
|   state   | varchar | false  |      1      |    1    |        |    |          |
|   time    |timestamp| false  |      2      |    2    |   15   | m  |          |
|  weather  | varchar | false  |             |         |        |    |          |
|temperature| double  |  true  |             |         |        |    |          |
|  rawdata  |  blob   |  true  |             |         |        |    |          |
+-----------+---------+--------+-------------+---------+--------+----+----------+
```

You can insert data:

```
riak-shell>INSERT INTO GeoCheckin VALUES ('SC', '2017-01-01T15:00:00', 'sunny', 43.2, 0x3af6240c1000035dbc), ('SC', '2017-01-01T16:00:00', 'cloudy', 41.5, 0x3af557bc4000042dbc), ('SC', '2017-01-01T17:00:00', 'windy', 33.0, 0x3af002ee10000a2dbc);
```

See [Writing Data][writing] for more details.

You can select specific columns:

```
riak-shell(4)>SELECT time, weather, temperature, rawdata from GeoCheckin WHERE state = 'SC' and time >= '2017-01-01' and time < '2017-01-02';
+--------------------+-------+-----------+--------------------+
|        time        |weather|temperature|      rawdata       |
+--------------------+-------+-----------+--------------------+
|2017-01-01T15:00:00Z| sunny |   43.2    |0x3af6240c1000035dbc|
|2017-01-01T16:00:00Z|cloudy |   41.5    |0x3af557bc4000042dbc|
|2017-01-01T17:00:00Z| windy |   33.0    |0x3af002ee10000a2dbc|
+--------------------+-------+-----------+--------------------+
```

{{% note %}}
SQL commands in riak shell may span multiple lines.
{{% /note %}}

#### Comments

Riak shell understands (and thus ignores) SQL comments. Both simple
comments (single line comments prefaced with `--`) and bracketed
comments (single- or multi-line comments using `/*` and `*/`) are
supported.

As of TS 1.5.0 (the first release to support comments), there is a
compatibility bug with the SQL specification: nested bracketed
comments are not properly supported, so the comment will terminate at
the first `*/` sequence.

### Logging

You can specify the name of your logfile by running: `logfile "»mylogname«"`.

```
riak-shell>logfile "mylogfile";
Log file changed to "mylogfile"
```

You can turn logging on or off by running either `log on` or `log off`.

```
riak-shell>log on;
Logging turned on.
riak-shell>log off;
Logging turned off.
```

Logging is off by default. The above command will allow you to turn logging on or off for the duration of your time using riak shell. To change the default state, you must edit the riak_shell.config [configuration file](#configuration).

You can check whether logging is currently on or off by running `show_log_status`:

```
riak-shell>show_log_status;
Logging : on
Date Log : off
Logfile : "/users/myusername/riakts/riak-ts-1.5.0/bin/../log/riak_shell/riak_shell"
Current Date: "2016_02_02-00:26:19"
```

{{% note %}}
If you have temporarily turned logging on/off, the output of `show_log_status` may differ from the output of `show_config`.
{{% /note %}}

If you would like your logfile to have a timestamp, run `date_log`.

You can replay the current logfile regardless of whether logging is turned on. To replay your logfile, run `replay_log`.

```
riak-shell>replay_log;

Replaying "mylogfile.log"
replay (1)> describe GeoCheckin;
+-----------+---------+--------+-------------+---------+--------+----+----------+
|  Column   |  Type   |Nullable|Partition Key|Local Key|Interval|Unit|Sort Order|
+-----------+---------+--------+-------------+---------+--------+----+----------+
|   state   | varchar | false  |      1      |    1    |        |    |          |
|   time    |timestamp| false  |      2      |    2    |   15   | m  |          |
|  weather  | varchar | false  |             |         |        |    |          |
|temperature| double  |  true  |             |         |        |    |          |
|  rawdata  |  blob   |  true  |             |         |        |    |          |
+-----------+---------+--------+-------------+---------+--------+----+----------+


replay (2)> SELECT time, weather, temperature, rawdata from GeoCheckin WHERE state = 'SC' and time >= '2017-01-01' and time < '2017-01-02';
+--------------------+-------+-----------+--------------------+
|        time        |weather|temperature|      rawdata       |
+--------------------+-------+-----------+--------------------+
|2017-01-01T15:00:00Z| sunny |   43.2    |0x3af6240c1000035dbc|
|2017-01-01T16:00:00Z|cloudy |   41.5    |0x3af557bc4000042dbc|
|2017-01-01T17:00:00Z| windy |   33.0    |0x3af002ee10000a2dbc|
+--------------------+-------+-----------+--------------------+
```

To play a specific logfile, run `replay_log »filename.log«`.

You can also run a regression test on your logfile by running `regression_log "»path to logfile«"`. The `regression_log` command will check the output of the log to see if it matches the previous run.

```
riak-shell>regression_log "../log/riak_shell.log";

Regression Testing "../log/riak_shell.log"
No Regression Errors.
```


### Help

You get help on riak shell functions with the `help` command:

```
riak-shell> help;
```

You can get more specific help by calling `help` with the extension name and function name like `help shell quit;`.


## Configuration

You can configure riak shell from the riak_shell.config file. You can find the file in your Riak TS directory. 

The following things can be configured:

* `logging` (on | off) - Defaults to 'off'; determines whether or not to enable logging.
* `date_log` (on | off) - Defaults to 'off'; determines whether or not to add timestamp information to the logs.
* `logfile` ("../some/dir/mylogfile.log") - Defaults to '../log/riak_shell.log'; sets the name and location of the logfile.
* `cookie` - No default; any atom representing the Erlang cookie that riak shell uses to connect to Riak.
* `show_connection_status` (true | false) - Defaults to 'false'; sets whether to show the green tick or red cross in the command line.
* `nodes` ([nodenames]) - No defaults; a list of nodes to try and connect to on startup or 'reconnect;'.


## Command Line Flags

There are 4 different configurations, two of which trigger batch mode.

By default riak shell swallows error messages, this makes it hard to develop new extensions. You can run it in debug mode as shown below:

```bash
riak-shell -d
```

You can pass in a different config file than `../etc/riak/riak_shell.config`:

```bash
riak-shell -c ../path/to/my.config
```

You can run a riak shell replay log for batch/scripting:

```bash
riak-shell -f ../path/to/my.log
```

You can run a riak shell regression log for batch/scripting:

```bash
riak-shell -r ../path/to/my.log
```


## Extending Riak Shell

See the [riak shell README] for information on extending or developing on riak shell.
