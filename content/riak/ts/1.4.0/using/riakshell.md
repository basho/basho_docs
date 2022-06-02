---
title: "Using riak shell"
description: "Using riak shell"
menu:
  riak_ts-1.4.0:
    name: "riak shell"
    identifier: "riak_shell"
    weight: 309
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/riakshell/
---

[nodename]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/changing-cluster-info/
[creating]: {{<baseurl>}}riak/ts/1.4.0/using/creating-activating
[writing]: {{<baseurl>}}riak/ts/1.4.0/using/writingdata
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

1a\. You can attach to any node in your cluster from riak shell. To do this, locate your riak_shell.config file. On most systems, it will be in the `/etc/riak` directory with the other Riak TS configuration files. On Mac OS X, the configuration files are in the `~/riak-ts-1.4.0/etc` directory. Open riak_shell.config, and add the nodename and IP addresses you wish to connect to to nodes:

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

You can use riak shell to [create a table][creating]:

```
riak-shell>CREATE TABLE GeoCheckin (region VARCHAR NOT NULL, state VARCHAR NOT NULL, time  TIMESTAMP NOT NULL, weather  VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((region, state, QUANTUM(time, 15, 'm')), region, state, time));
```

Then, you can see the table in riak shell:

```
riak-shell>describe GeoCheckin;
+-----------+---------+-------+-----------+---------+
|  Column   |  Type   |Is Null|Primary Key|Local Key|
+-----------+---------+-------+-----------+---------+
| region    | varchar | false |     1     |    1    |
| state     | varchar | false |     2     |    2    |
|   time    |timestamp| false |     3     |    3    |
|  weather  | varchar | false |           |         |
|temperature| double  | true  |           |         |
+-----------+---------+-------+-----------+---------+
```

You can also select specific data points from your table:

```
riak-shell(28)>SELECT time, weather, temperature FROM GeoCheckin WHERE myfamily='family1' AND myseries='series1' AND time > 0 AND time < 1000;
+------------------------+-------+--------------------------+
|          time          |weather|       temperature        |
+------------------------+-------+--------------------------+
|1970-01-01T00:00:00.001Z| snow  |2.51999999999999992895e+01|
|1970-01-01T00:00:00.002Z| rain  |2.45000000000000000000e+01|
|1970-01-01T00:00:00.003Z| rain  |2.30000000000000000000e+01|
|1970-01-01T00:00:00.004Z| sunny |2.86000000000000014211e+01|
|1970-01-01T00:00:00.005Z| sunny |2.46999999999999992895e+01|
|1970-01-01T00:00:00.006Z|cloudy |3.27890000000000014779e+01|
|1970-01-01T00:00:00.007Z|cloudy |2.78999999999999985789e+01|
|1970-01-01T00:00:00.008Z|  fog  |3.48999999999999985789e+01|
|1970-01-01T00:00:00.009Z|  fog  |2.86999999999999992895e+01|
|1970-01-01T00:00:00.01Z | hail  |3.81000000000000014211e+01|
+------------------------+-------+--------------------------+
```

You can add data via a simple SQL INSERT statement, too:

```sql
riak-shell>INSERT INTO GeoCheckin (region, state, time, weather, temperature) VALUES ('South Atlantic','South Carolina',1420113600000,'snow',25.2);
```

See [Writing Data][writing] for more details.

{{% note %}}
SQL commands in riak shell may span multiple lines.
{{% /note %}}


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
Logfile : "/users/myusername/riakts/riak-ts-1.4.0/bin/../log/riak_shell/riak_shell"
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

+-----------+---------+-------+-----------+---------+
|  Column   |  Type   |Is Null|Primary Key|Local Key|
+-----------+---------+-------+-----------+---------+
| region    | varchar | false |     1     |    1    |
| state     | varchar | false |     2     |    2    |
|   time    |timestamp| false |     3     |    3    |
|  weather  | varchar | false |           |         |
|temperature| double  | true  |           |         |
+-----------+---------+-------+-----------+---------+

replay (2)> select time, weather, temperature from GeoCheckin where region='South Atlantic' and state='South Carolina' and time > 0 and time < 1000;

+------------------------+-------+--------------------------+
|          time          |weather|       temperature        |
+------------------------+-------+--------------------------+
|1970-01-01T00:00:00.001Z| snow  |2.51999999999999992895e+01|
|1970-01-01T00:00:00.002Z| rain  |2.45000000000000000000e+01|
|1970-01-01T00:00:00.003Z| rain  |2.30000000000000000000e+01|
|1970-01-01T00:00:00.004Z| sunny |2.86000000000000014211e+01|
|1970-01-01T00:00:00.005Z| sunny |2.46999999999999992895e+01|
|1970-01-01T00:00:00.006Z|cloudy |3.27890000000000014779e+01|
|1970-01-01T00:00:00.007Z|cloudy |2.78999999999999985789e+01|
|1970-01-01T00:00:00.008Z|  fog  |3.48999999999999985789e+01|
|1970-01-01T00:00:00.009Z|  fog  |2.86999999999999992895e+01|
|1970-01-01T00:00:00.01Z | hail  |3.81000000000000014211e+01|
+------------------------+-------+--------------------------+
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