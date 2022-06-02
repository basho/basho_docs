---
title: "Using riak shell"
description: "Using riak shell"
menu:
  riak_ts-1.3.0:
    name: "riak shell"
    identifier: "riak_shell"
    weight: 309
    parent: "using"
project: "riak_ts"
project_version: "1.3.0"
toc: true
version_history:
  in: "1.2.0+"
aliases:
    - /riakts/1.3.0/using/riakshell/
---

[nodename]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/changing-cluster-info/
[creating]: {{<baseurl>}}riak/ts/1.3.0/using/creating-activating
[writing]: {{<baseurl>}}riak/ts/1.3.0/using/writingdata


You can use riak shell within Riak TS to run SQL and logging commands from one place.


## Capabilities

While riak shell is in the early stages of development, the following are well-supported: 

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

1\. Locate your riak_shell.config configuration file. It will be in the `/etc/riak` directory with the other Riak TS configuration files. On Mac OS X, the configuration files are in the `riak-ts-1.3.0/etc` directory.

2\. Open the configuration file, riak_shell.config, and add your nodename and IP address to `nodes`:

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

3\. If you change the IP address in any of the configuration files, you will need to perform the steps in the [Rename Single Node Clusters][nodename] before proceeding.

4\. Navigate back to your Riak TS directory, and open riak shell:

```bash
./riak-shell
```

You can verify your connection by running `show_connection`. You should see a reply like this one:

```
riak_shell is connected to: 'dev1@127.0.0.1' on port 8087
```


## Basic Commands

### Connecting and reconnecting

You can connect riak shell to multiple nodes.

>**Warning**
>
>You cannot run more than one shell per machine. If you try to connect two shells to a single machine, you will receive an error message.

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

You can see the log of your activity in riak shell by running `>show_history;`

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
riak-shell>select time, weather, temperature from GeoCheckin where region='South Atlantic' and state='South Carolina' and time > 0 and time < 1000;
+----+----------------+---------------------------+
|time|    weather     |        temperature        |
+----+----------------+---------------------------+
| 1  |    z«êPò¹    |4.19111744258298777600e+18 |
| 2  |  ^OOgz^Blu7)  |6.07861409217513676800e+18 |
| 3  |      ÔÖã       |6.84034338181623808000e+17 |
| 4  |       ^G        |-5.55785206740398080000e+16|
| 5  |   ¸LËäà«d    |-3.62555783091625574400e+18|
| 6  |    ^AE^S¥­     |1.11236574770119680000e+18 |
| 7  |    ïö?ï^Fv     |5.51455556936744140800e+18 |
| 8  | ^FtFVÅë=+#^Y5  |2.44525777392835584000e+17 |
| 9  |ðÁÖ·©Ü^GV^^^DkU|6.90864738609726668800e+18 |
| 10 | QÝZa^QËfQ  |5.08590022245487001600e+18 |
+----+----------------+---------------------------+
```

You can add data via a simple SQL `INSERT` statement, too:

```sql
riak-shell>INSERT INTO GeoCheckin (region, state, time, weather, temperature) VALUES ('South Atlantic','South Carolina',1420113600000,'snow',25.2);
```

See [Writing Data][writing] for more details.

>**Note:** SQL commands in riak shell may span multiple lines.


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

Logging is off by default. The above command will allow you to turn logging on or off for the duration of your time using riak shell. To change the default state, you must edit the `riak_shell.config` [configuration file](#configuration).

You can check whether logging is currently on or off by running `show_log_status`:

```
riak-shell>show_log_status;
Logging : on
Date Log : off
Logfile : "/users/myusername/riakts/riak-ts-1.3.0/bin/../log/riak_shell/riak_shell"
Current Date: "2016_02_02-00:26:19"
```

>**Note:** If you have temporarily turned logging on/off, the output of `show_log_status` may differ from the output of `show_config`.

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

+----+----------------+---------------------------+
|time|    weather     |        temperature        |
+----+----------------+---------------------------+
| 1  |    z«êPò¹    |4.19111744258298777600e+18 |
| 2  |  ^OOgz^Blu7)  |6.07861409217513676800e+18 |
| 3  |      ÔÖã       |6.84034338181623808000e+17 |
| 4  |       ^G        |-5.55785206740398080000e+16|
| 5  |   ¸LËäà«d    |-3.62555783091625574400e+18|
| 6  |    ^AE^S¥­     |1.11236574770119680000e+18 |
| 7  |    ïö?ï^Fv     |5.51455556936744140800e+18 |
| 8  | ^FtFVÅë=+#^Y5  |2.44525777392835584000e+17 |
| 9  |ðÁÖ·©Ü^GV^^^DkU|6.90864738609726668800e+18 |
| 10 | QÝZa^QËfQ  |5.08590022245487001600e+18 |
+----+----------------+---------------------------+
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

* logging (on | off) - Defaults to 'off'; determines whether or not to enable logging.
* date_log (on | off) - Defaults to 'off'; determines whether or not to add timestamp information to the logs.
* logfile ("../some/dir/mylogfile.log") - Defaults to '../log/riak_shell.log'; sets the name and location of the logfile.
* cookie - No default; any atom representing the Erlang cookie that riak shell uses to connect to Riak.
* show_connection_status (true | false) - Defaults to 'false'; sets whether to show the green tick or red cross in the command line.
* nodes ([nodenames]) - No defaults; a list of nodes to try and connect to on startup or 'reconnect;'.


## Command Line Flags

There are 4 different configurations, two of which trigger batch mode.

By default riak shell swallows error messages, this makes it hard to develop new extensions. You can run it in debug mode as shown below:

``` 
./riak-shell -d
```

You can pass in a different config file than `../etc/riak/riak_shell.config`:

```
./riak-shell -c ../path/to/my.config
```

You can run a riak shell replay log for batch/scripting:

```
./riak-shell -f ../path/to/my.log
```

You can run a riak shell regression log for batch/scripting:

```
./riak-shell -r ../path/to/my.log
```


## Extending Riak Shell

riak shell uses a magic architecture with convention.

Riak modules with names like `mymodule_EXT.erl` are considered to be riak shell extension modules.

All exported functions with an arity >= 1 are automatically exposed in riak shell mode, with some exceptions.

Exported functions with the following names will be silently ignored:

* `module_info/0`
* `module_info/1`
* `help/1`
* `'riak-admin'/N`

Functions that share a name with the first keyword of supported SQL statements will likewise be ignored:

* `create/N`
* `describe/N`
* `select/N`

As additional SQL statements are supported, adding them to the macro `IMPLEMENTED_SQL_STATEMENTS` in `riakshell.hrl` will automatically make them available to riak shell and exclude them from extensions.

To add a function which appears to the user like:

```
riak-shell> frobulator bish bash bosh;
```

You implement a function with the following signature:

```
frobulator(#state{} = State, _Arg1, _Arg2, N) when is_integer(N) ->
    Result = "some string that is the result of the fn",
    {Result, State};
frobulator(S, _Arg1, Arg2, N) ->
    ErrMsg = io_lib:format("The third parameter '~p' should be an integer",
        [N]),
   {ErrMsg, S#state{cmd_error = true}}.
```

Your function may modify the state record if appropriate. All the shell functions are implemented as extensions.

This example shows you how to handle errors - return an error message and a state record with the `cmd_error` flag set to 'true'.

To be a good citizen you should add a clause to the help function like:

```
-help(frobulator) ->
    "This is how you use my function";
```

If you have a function with the same name that appears in two EXT modules riak shell will not start. It will not check if the arities match. You may have the same function with different arities in the same module - but there is only one help call.

As a convenience, there is a module called `debug_EXT.erl`. This module implements a function which reloads and reregisters all extensions `riak-shell>load`, and can hot-load changes into the shell (it won't work on first-creation of a new EXT module, only on reloading). The only EXT that debug doesn't load is `debug_EXT`, so please do not add functions to it.

riak shell suppresses error messages that would otherwise be written to the console. For instance, if the remote Riak node goes down the protocol buffer connection is torn down. This makes debugging painful. You can stop this behavior by starting riak shell in debug mode. You can do this by starting it from the shell with the `-d` flag:

```
cd ~/riakshell/bin
./riak-shell -d
```


### Architecture Notes

This shell has a simpler architecture than conventional Erlang/LFE/Elixir REPLS.

Although there are no `-spec()` annotations this is actually an example of spec-first development.