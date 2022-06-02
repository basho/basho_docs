---
title: "Benchmarking"
description: ""
project: "riak_kv"
project_version: "2.0.9"
menu:
  riak_kv-2.0.9:
    name: "Benchmarking"
    identifier: "performance_benchmarking"
    weight: 100
    parent: "managing_performance"
toc: true
aliases:
  - /riak/2.0.9/ops/building/benchmarking
  - /riak/kv/2.0.9/ops/building/benchmarking
---

Basho Bench is a benchmarking tool created to conduct accurate and
repeatable performance tests and stress tests, and to produce
performance graphs.

Basho Bench exposes a pluggable driver interface and has been extended
to serve as a benchmarking tool against a variety of projects. New
drivers can be written in Erlang and are generally less than 200 lines
of code.

## Installation

You will need:

1. One or more load-generating machines on which to install
    ```basho_bench```.  Especially when testing larger clusters, a
    single machine cannot generate enough load to properly exercise
    the cluster.  Do not run the ```basho_bench``` instances on the
    Riak nodes themselves, since the load generation will compete with
    Riak for resources.
2. The [R statistics language](http://www.r-project.org/) must be
    installed (somewhere available to you) if you wish to generate
    graphs (see the [Generating Benchmark Graphs](#generating-benchmark-graphs) section, below).

### Download ```basho_bench```

You can download the pre-built packages below, or build it from source.

* **Ubuntu 14.04 LTS:**
    [basho-bench_0.10.0.53-1_amd64.deb](http://ps-tools.s3.amazonaws.com/basho-bench_0.10.0.53.g0e15158-ubuntu14.04LTS-1_amd64.deb)
* **CentOS 7:**
    [basho-bench-0.10.0.53-1.el7.centos.x86_64.rpm](http://ps-tools.s3.amazonaws.com/basho-bench-0.10.0.53.g0e15158-1.el7.centos.x86_64.rpm)

### Building from Source

#### Prerequisites

* Erlang must be installed. See [Installing Erlang]({{<baseurl>}}riak/kv/2.0.9/setup/installing/source/erlang) for instructions
    and versioning requirements. Note: Unless you're an experienced
    Erlang developer, we recommend that you use Ubuntu 14.04 LTS (and
    not CentOS), when building ```basho_bench``` from source.  Later
    versions of CentOS (6 and 7) have difficulty with installing and
    enabling certain parts of the ```erlang-crypto``` package, which
    is required by ```basho_bench```.
* Install ```git``` (to check out the ```basho_bench``` code)

#### Compiling

```bash
git clone git://github.com/basho/basho_bench.git
cd basho_bench
make
```

## Usage

Run the `basho_bench` script, pass in the config file and the
directory to generate the results into:

```bash
basho_bench --results-dir <results dir> <config file>
```

If you've installed ```basho_bench``` from a pre-built package, you
must specify full paths for the test results directory and config
file. (Also, don't use the common ```~/``` shell notation, specify the
user's home directory explicitly)

```bash
basho_bench --results-dir /home/username/bench_results/ /etc/basho_bench/riakc_pb.config 
```

The example above will generate results in
```/home/username/bench_results/current/```.

If you built ```basho_bench``` from source, you can get away with
relative paths (and the results directory will be created in the
current directory):

```bash
./basho_bench myconfig.config
```

This will generate results in `tests/current/`. You will need to
create a configuration file. The recommended approach is to start from
a file in the `examples` directory and modify settings using the
[Configuration](#configuration) section below for
reference.

## Generating Benchmark Graphs

The output of from running the `basho_bench` script can be used to
create graphs showing the following:

* Throughput --- Operations per second over the duration of the test.
* Latency at 99th percentile, 99.9th percentile and max latency for
    the selected operations.
* Median latency, mean latency, and 95th percentile latency for the
    selected operations.

### Prerequisites

The R statistics language is needed to generate graphs. Note: If
necessary, R can be installed on a different machine than the one
running basho_bench, and the performance data can be copied (via
rsync, for example) from the load testing machine to the one that will
be generating and viewing the graphs (such as a desktop).

#### Installing R on Ubuntu

```
sudo apt-get install r-base
```

#### Installing R on Other Platforms

-   [More information](http://www.r-project.org/)
-   [Download R](http://cran.r-project.org/mirrors.html)

Follow the instructions for your platform to install R.

### Generating Graphs

If you have installed ```basho_bench``` from a pre-built package, and
you also have R installed on the same machine, you can generate the
current result graph with the following:

```bash
Rscript --vanilla /usr/lib/basho_bench/lib/basho_bench*/priv/summary.r -i /home/username/bench_results/current/
```

This will create a results file in
```/home/username/bench_results/summary.png```.

If you have built ```basho_bench``` from source, you can just use
```make```.  To generate a benchmark graph against the current
results, run:

```bash
make results
```

This will create a results file in `tests/current/summary.png`.

You can also run this manually:

```bash
priv/summary.r -i tests/current
```

### Troubleshooting Graph Generation

For additional help, see the [Troubleshooting Graph Generation](https://github.com/basho/basho_bench#troubleshooting-graph-generation)
section of the ```basho_bench/README```.

## How does it work?

When Basho Bench starts (`basho_bench.erl`), it reads the
configuration (`basho_bench_config.erl`), creates a new results
directory, and then sets up the test (`basho_bench_app.erl` and
`basho_bench_sup.erl`).

During test setup, Basho Bench creates the following:

* One **stats process** (`basho_bench_stats.erl`). This process
    receives notifications when an operation completes, plus the
    elapsed time of the operation, and stores it in a histogram. At
    regular intervals, the histograms are dumped to `summary.csv` as
    well as operation-specific latency CSVs (e.g. `put_latencies.csv`
    for the PUT operation).
* N **workers**, where N is specified by the [concurrent](#concurrent) configuration setting
    (`basho_bench_worker.erl`). The worker process wraps a driver
    module, specified by the [driver](#driver)
    configuration setting. The driver is randomly invoked using the
    distribution of operations as specified by the [operations](#operations) configuration setting. The rate at which the
    driver invokes operations is governed by the [mode](#mode) setting.

Once these processes have been created and initialized, Basho Bench
sends a run command to all worker processes, causing them to begin the
test. Each worker is initialized with a common seed value for random
number generation to ensure that the generated workload is reproducible
at a later date.

During the test, the workers repeatedly call `driver:run/4`, passing in
the next operation to run, a keygen function, a valuegen function, and
the last state of the driver. The worker process times the operation,
and reports this to the stats process when the operation has completed.

Finally, once the test has been run for the duration specified in the
config file, all workers and stats processes are terminated and the
benchmark ends. The measured latency and throughput of the test can be
found in `./tests/current/`. Previous results are in timestamped
directories of the form `./tests/YYYYMMDD-HHMMSS/`.

## Configuration

Basho Bench ships with a number of sample configuration files, available
in the `/examples` directory.

### Global Config Settings

#### mode

The `mode` setting controls the rate at which workers invoke the
`{driver:run/4}` function with a new operation. There are two possible
values:

* `{max}` --- generate as many ops per second as possible
* `{rate, N}` --- generate N ops per second, with exponentially distributed interarrival times

Note that this setting is applied to each driver independently. For
example, if `{rate, 5}` is used with 3 concurrent workers, Basho Bench
will be generating 15 (i.e. 5 * 3) operations per second.

```erlang
% Run at max, i.e.: as quickly as possible
{mode, max}

% Run 15 operations per second per worker
{mode, {rate, 15}}
```

#### concurrent

The number of concurrent worker processes. The default is 3 worker
processes. This determines the number of concurrent clients running
requests on API under test.

```erlang
% Run 10 concurrent processes
{concurrent, 10}
```

#### duration

The duration of the test, in minutes. The default is 5 minutes.

```erlang
% Run the test for one hour
{duration, 60}
```

#### operations

The possible operations that the driver will run, plus their "weight,"
or likelihood of being run. The default is `[{get,4},{put,4},{delete,
1}]`, which means that out of every 9 operations, GET will be called
four times, PUT will be called four times, and DELETE will be called
once, on average.

```erlang
{operations, [{get, 4}, {put, 1}]}.
```

Operations are defined on a **per-driver** basis. Not all drivers will
implement the GET/PUT operations discussed above. Consult the driver
source to determine the valid operations. If you're testing the HTTP
interface, for example, the corresponding operations are GET and
UPDATE, respectively.

If a driver does not support a specified operation (`asdfput` in this
example), you may see errors like this:

```log
DEBUG:Driver basho_bench_driver_null crashed: {function_clause,
                                          [{{{basho_bench_driver_null,run,
                                            [asdfput,
                                             #Fun<basho_bench_keygen.4.4674>,
                                             #Fun<basho_bench_valgen.0.1334>,
                                             undefined]}}},
                                           {{{basho_bench_worker,
                                            worker_next_op,1}}},
                                           {{{basho_bench_worker,
                                            max_worker_run_loop,1}}}]}
```

#### driver

The module name of the driver that Basho Bench will use to generate
load. A driver may simply invoke code in-process (such as when
measuring the performance of DETS) or may open network connections and
generate load on a remote system (such as when testing a Riak
server/cluster).

Available drivers include:

* `basho_bench_driver_http_raw` --- Uses Riak's HTTP interface to
    get/update/insert data on a Riak server
* `basho_bench_driver_riakc_pb` --- Uses Riak's Protocol Buffers
    interface to get/put/update/delete data on a Riak serve
* `basho_bench_driver_riakclient` --- Uses Riak's Distributed Erlang
    interface to get/put/update/delete data on a Riak server
* `basho_bench_driver_bitcask` --- Directly invokes the Bitcask API
* `basho_bench_driver_dets` --- Directly invokes the DETS API

On invocation of the `driver:run/4` method, the driver may return one of
the following results:

* `{ok, NewState}` --- operation completed successfully
* `{error, Reason, NewState}` --- operation failed but the driver can
    continue processing (i.e. recoverable error)
* `{stop, Reason}` --- operation failed; driver can't/won't continue
    processing
* `{'EXIT', Reason}` --- operation failed; driver crashed

#### code_paths

Some drivers need additional Erlang code in order to run. Specify the
paths to this code using the `code_paths` configuration setting.

#### key_generator

The generator function to use for creating keys. Generators are defined
in `basho_bench_keygen.erl`. Available generators include:

* `{sequential_int, MaxKey}` --- generates integers from 0..`MaxKey`
    in order and then stops the system. Note that each instance of
    this keygen is specific to a worker.
* `{partitioned_sequential_int, MaxKey}` --- the same as
    `{sequential_int}`, but splits the keyspace evenly among the
    worker processes. This is useful for pre-loading a large dataset.
* `{partitioned_sequential_int, StartKey, NumKeys}` --- the same as
    `partitioned_sequential_int`, but starting at the defined
    `StartKey` and going up to `StartKey + NumKeys`.
* `{uniform_int, MaxKey}` --- selects an integer from uniform
    distribution of 0..`MaxKey`, i.e. all integers are equally probable.
* `{pareto_int, MaxKey}` --- selects an integer from a Pareto
    distribution, such that 20% of the available keys get selected 80%
    of the time. Note that the current implementation of this
    generator _may_ yield values larger than `MaxKey` due to the
    mathematical properties of the Pareto distribution.
* `{truncated_pareto_int, MaxKey}` --- the same as `{pareto_int}`, but
    will _not> yield values above `MaxKey`.
* `{function, Module, Function, Args}` --- specifies an external
    function that should return a key generator function. The worker
    `Id` will be prepended to `Args` when the function is called.
* `{int_to_bin, Generator}` --- takes any of the above `_int`
    generators and converts the number to a 32-bit binary. This is
    needed for some drivers that require a binary key.
* `{int_to_str, Generator}` --- takes any of the above `_int`
    generators and converts the number to a string. This is needed for
    some drivers that require a string key.

The default key generator is `{uniform_int, 100000}`.

Examples:

```erlang
% Use a randomly selected integer between 1 and 10,000
{key_generator, {uniform_int, 10000}}.

% Use a randomly selected integer between 1 and 10,000, as binary.
{key_generator, {int_to_bin, {uniform_int, 10000}}}.

% Use a pareto distributed integer between 1 and 10,000; values < 2000
% will be returned 80% of the time.
{key_generator, {pareto_int, 10000}}.
```

#### value_generator

The generator function to use for creating values. Generators are
defined in `basho_bench_valgen.erl`. Available generators include:

* `{fixed_bin, Size}` --- generates a random binary of `Size`
    bytes. Every binary is the same size, but varies in content.
* `{exponential_bin, MinSize, Mean}` --- generates a random binary
    which has an exponentially distributed size. Most values will be
    approximately `MinSize` + `Mean` bytes in size, with a long tail
    of larger values.
* `{uniform_bin, MinSize, MaxSize}` --- generates a random binary
    which has an evenly distributed size between `MinSize` and
    `MaxSize`.
* `{function, Module, Function, Args}` --- specifies an external
    function that should return a value generator function. The worker
    `Id` will be prepended to `Args` when the function is called.

The default value generator is `{value_generator, {fixed_bin, 100}}`.

Examples:

```erlang
% Generate a fixed size random binary of 512 bytes
{value_generator, {fixed_bin, 512}}.

% Generate a random binary whose size is exponentially distributed
% starting at 1000 bytes and a mean of 2000 bytes
{value_generator, {exponential_bin, 1000, 2000}}.
```

#### rng_seed

The initial random seed to use. This is explicitly seeded, rather than
seeded from the current time, so that a test can be run in a
predictable, repeatable fashion.

Default is `{rng_seed, {42, 23, 12}}`.

```erlang
% Seed to {12, 34, 56}
{rng_seed, {12, 34, 56}.
```

#### log_level

The `log_level` setting determines which messages Basho Bench will log
to the console and to disk.

The default level is `debug`.

| Valid levels
|:------------
| `debug`
| `info`
| `warning`
| `error`

#### report_interval

How often, in seconds, the stats process should write histogram data
to disk. The default is 10 seconds.

#### test_dir

The directory in which result data is written. The default is `/tests`.

### basho_bench_driver_riakclient Settings

These configuration settings apply to the
`basho_bench_driver_riakclient` driver.

#### riakclient_nodes

List of Riak nodes to use for testing.

```erlang
{riakclient_nodes, ['riak1@127.0.0.1', 'riak2@127.0.0.1']}.
```

#### riakclient_cookie

The Erlang cookie to use to connect to Riak clients. The default is `riak`.

```erlang
{riakclient_cookie, riak}.
```

#### riakclient_mynode

The name of the local node. This is passed into
[net_kernel:start/1](http://erlang.org/doc/man/net_kernel.html).

```erlang
{riakclient_mynode, ['basho_bench@127.0.0.1', longnames]}.
```

#### riakclient_replies

This value is used for R-values during a get operation, and W-values
during a put operation.

```erlang
% Expect 1 reply.
{riakclient_replies, 1}.
```

#### riakclient_bucket

The Riak bucket to use for reading and writing values. The Default is
`<<"test">>`.

```erlang
% Use the "bench" bucket.
{riakclient_bucket, <<"bench">>}.
```

### basho_bench_driver_riakc_pb Settings

#### riakc_pb_ips

A list of IP addresses to connect the workers to. A random IP will be
chosen for each worker.

The default is `{riakc_pb_ips, [{127,0,0,1}]}`

```erlang
% Connect to a cluster of 3 machines
{riakc_pb_ips, [{10,0,0,1},{10,0,0,2},{10,0,0,3}]}
```

#### riakc_pb_port

The port on which to connect to the PBC interface.

The default is `{riakc_pb_port, 8087}`

#### riakc_pb_bucket

The bucket to use for testing.

The default is `{riakc_pb_bucket, <<"test">>}`

### basho_bench_driver_http_raw Settings

#### http_raw_ips

A list of IP addresses to connect the workers to. Each worker makes
requests to each IP in a round-robin fashion.

The default is `{http_raw_ips, ["127.0.0.1"]}`

```erlang
% Connect to a cluster of machines in the 10.x network
{http_raw_ips, ["10.0.0.1", "10.0.0.2", "10.0.0.3"]}.
```

#### http_raw_port

Select the default port to connect to for the HTTP server.

The default is `{http_raw_port, 8098}`.

```erlang
% Connect on port 8090
{http_raw_port, 8090}.
```

#### http_raw_path

The base path to use for accessing Riak, usually `"/riak/<bucket>"`.

The default is `{http_raw_path, "/riak/test"}`.

```erlang
% Place test data in another_bucket
{http_raw_path, "/riak/another_bucket"}.
```

#### http_raw_params

Additional parameters to add to the end of the URL. This can be used
to set the `r`/`w`/`dw`/`rw` parameters as desired.

The default is `{http_raw_params, ""}`.

```erlang
% Set R=1, W=1 for testing a system with n_val set to 1
{http_raw_params, "?r=1&w=1"}.
```

#### http_raw_disconnect_frequency

How often, in seconds or number of operations, the HTTP clients
(workers) should forcibly disconnect from the server.

The default is `{http_raw_disconnect_frequency, infinity}` (which
means that Basho Bench should never forcibly disconnect).

```erlang
% Disconnect after 60 seconds
{http_raw_disconnect_frequency, 60}.

% Disconnect after 200 operations
{http_raw_disconnect_frequency, {ops, 200}}.
```

## Custom Driver

A custom driver must expose the following callbacks.

```erlang
% Create the worker
% ID is an integer
new(ID) -> {ok, State} or {error, Reason}.

% Run an operation
run(Op, KeyGen, ValueGen, State) -> {ok, NewState} or {error, Reason, NewState}.
```

See the [existing
drivers](https://github.com/basho/basho_bench/tree/master/src) for
more details.
