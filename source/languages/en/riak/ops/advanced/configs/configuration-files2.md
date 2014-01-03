    ```erlang
    {default_bucket_props, [
        {n_val,3},
        {allow_mult,false},
        {last_write_wins,false},
        {precommit, []},
        {postcommit, []},
        {chash_keyfun, {riak_core_util, chash_std_keyfun}},
        {linkfun, {modfun, riak_kv_wm_link_walker, mapreduce_linkfun}}
        ]}
    ```



### riak_kv settings










































#### -name
Name of the Erlang node. (default: `riak@127.0.0.1`)

The default value, `riak@127.0.0.1`, will work for running Riak locally, but for distributed (multi-node) use, the portion of the name after the "@" should be changed to the IP address of the machine on which the node is running.

If you have properly-configured DNS, the short-form of this name can be used (for example: `riak`). The name of the node will then be `riak@Host.Domain`.

#### -setcookie
Cookie of the Erlang node. (default: `riak`)

Erlang nodes grant or deny access based on the sharing of a previously-shared cookie. You should use the same cookie for every node in your Riak cluster, but it should be a not-easily-guessed string unique to your deployment, to prevent non-authorized access.

#### -heart
Enable "heart" node monitoring. (default: `disabled`)

Heart will restart nodes automatically, should they crash. However, heart is so good at restarting nodes that it can be difficult to prevent it from doing so. Enable heart once you are sure that you wish to have the node restarted automatically on failure.

#### +K
Enable kernel polling. (default: `true`)

#### +A
Number of threads in the async thread pool. (default: `64`)

#### -pa
Adds the specified directories to the beginning of the code path,
similar to
[`code:add_pathsa/1`](http://www.erlang.org/doc/man/code.html#add_pathsa-1). As
an alternative to `-pa`, if several directories are to be prepended to
the code and the directories have a common parent directory, that
parent directory could be specified in the `ERL_LIBS` environment
variable.

#### -env
Set host environment variables for Erlang.

#### -smp
Enables Erlang's SMP support. (default: `enable`)

#### +zdbbl
Configures the buffer size for outbound messages between nodes. This
is commented out by default because the ideal value varies
significantly depending on available system memory, typical object
size, and amount of traffic to the database. (default: `1024` unless
configured in `vm.args`, `32768` is the commented-out value)

Systems with lots of memory and under a heavy traffic load should
consider increasing our default value; systems under lighter load but
storing large objects may wish to lower it. [[Basho Bench]] is highly
recommended to help determine the best values for this (and other
tuning parameters) in your environment.

#### +P
Defines the Erlang process limit. Under the versions of Erlang
supported by Riak through 1.4.x, the limit is very low, and thus using
this to raise the limit is very important. (default: `256000`)

**Note**: For anyone concerned about configuring such a high value, be
aware that Erlang processes are not the same as system processes. All
of these processes will exist solely inside a single system process,
the Erlang beam.

#### +sfwi
If using an
[appropriately patched Erlang VM](https://gist.github.com/evanmcc/a599f4c6374338ed672e)
(such as one downloaded directly from Basho) this will control the
interval (in milliseconds) at which a supervisor thread wakes to check
run queues for work to be executed. (default: `500`)

#### +W
Determines whether warning messages sent to Erlang's `error_logger`
are treated as errors, warnings, or informational. (default: `w` for
warnings)

#### -env ERL_LIBS
Alternate method to add directories to the code path (see `-pa` above)

#### -env ERL_MAX_PORTS

Maximum number of concurrent ports/sockets. (default: `64000`)

**Note**: As with processes, Erlang ports and system ports are similar
but distinct.

#### -env ERL_FULLSWEEP_AFTER

Run garbage collection more often. (default: `0`)

#### -env ERL_CRASH_DUMP

Set the location of crash dumps. (default: `./log/erl_crash.dump`)

## Rebar Overlays

If you are going to be rebuilding Riak often, you will want to edit the
`vm.args` and `app.config` files in the `rel/files` directory. These files are
used whenever a new release is generated using "make rel" or "rebar generate".
Each time a release is generated any existing release must first be destroyed.
Changes made to release files (`rel/riak/etc/vm.args`,
`rel/riak/etc/app.config`, etc.) would be lost when the release is destroyed.
