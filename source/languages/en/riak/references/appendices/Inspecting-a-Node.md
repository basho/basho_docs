---
title: Inspecting a Riak Node
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [operator, status, riaknostic]
---

When inspection of a Riak node to gather metrics on performance or
potential issues is desired, a number of tools are available to help,
and are either included with Riak itself or made available through the
Riak community.

This guide provides starting points and details on some of the available
tools for inspecting a Riak node.

riak-admin status
-----------------

`riak-admin status` is a subcommand of the `riak-admin` command that is
included with every installation of Riak. The `status` subcommand
provides data related to current operating status for a node. The output
of `riak-admin status` is categorized and detailed below.

Please note, for some counters such as node_get_fsm_objsize a minimum of
5 transactions is required for statistics to be generated.

### One-minute

One-minute Counters are data points delineating the number of times a
particular activity has occurred within the last minute on this node.

Sample one minute counters:

-   **node\_gets** Number of GETs coordinated by this node, including
    GETs to non-local vnodes on this node within the last minute
-   **node\_gets\_total** Number of GET operations coordinated by vnodes
    on this node since node was started
-   **node\_puts** Number of PUTs coordinated by this node, including
    PUTs to non-local vnodes on this node within the last minute
-   **node\_puts\_total** Number of PUT operations coordinated by vnodes
    on this node since node was started
-   **vnode gets** Number of GET operations coordinated by vnodes on
    this node within the last minute
-   **vnode\_gets\_total** Number of GET operations coordinated by
    vnodes on this node since node was started
-   **vnode\_puts** Number of PUT operations coordinated by vnodes on
    this node within the last minute
-   **vnode\_puts\_total** Number of PUT operations coordinated by
    vnodes on this node since node was started
-   **read\_repairs** Number of read repair operations this this node
    has coordinated in the last minute
-   **read\_repairs\_total**: Number of read repair operations this this
    node has coordinated since node was started

### FSM\_Time

FSM\_Time Counters represent the amount of time in microseconds required
to traverse the GET or PUT Finite State Machine code, offering a picture
of general node health. From your application's perspective, FSM\_Time
effectively represents experienced latency. Mean, Median, and 95th-,
99th-, and 100th-percentile (Max) counters are displayed. These are
one-minute stats.

Sample finite state machine time counters:

-   **node\_get\_fsm\_time\_mean**: Mean time between reception of
    client GET request and subsequent response to client
-   **node\_get\_fsm\_time\_median**: Median time between reception of
    client GET request and subsequent response to client
-   **node\_get\_fsm\_time\_95**: 95th percentile time between reception
    of client GET request and subsequent response to client
-   **node\_get\_fsm\_time\_99** 99th percentile time between reception
    of client GET request and subsequent response to client
-   **node\_get\_fsm\_time\_100** 100th percentile time between
    reception of client GET request and subsequent response to client
-   **node\_put\_fsm\_time\_mean**: Mean time between reception of
    client PUT request and subsequent response to client
-   **node\_put\_fsm\_time\_median**: Median time between reception of
    client PUT request and subsequent response to client
-   **node\_put\_fsm\_time\_95**: 95th percentile time between reception
    of client PUT request and subsequent response to client
-   **node\_put\_fsm\_time\_99**: 99th percentile time between reception
    of client PUT request and subsequent response to client
-   **node\_put\_fsm\_time\_100**: 100th percentile time between
    reception of client PUT request and subsequent response to client

### GET\_FSM\_Siblings

GET\_FSM\_Sibling Stats offer a count of the number of siblings
encountered by this node on the occasion of a GET request. These are
one-minute stats.

Sample finite state machine sibling counters:

-   **node\_get\_fsm\_siblings\_mean**: Mean number of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node\_get\_fsm\_siblings\_median**: Median number of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node\_get\_fsm\_siblings\_95**: 95th percentile of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node\_get\_fsm\_siblings\_99**: 99th percentile of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node\_get\_fsm\_siblings\_100**: 100th percentile of siblings
    encountered during all GET operations by this node within the last
    minute

### GET\_FSM\_Objsize

GET\_FSM\_Objsize is a window on the sizes of objects flowing through
this node's GET\_FSM. The size of an object is obtained by summing the
length of the bucket name, key, the serialized vector clock, the value,
and the serialized metadata of each sibling. GET\_FSM\_Objsize and
GET\_FSM\_Siblings are inextricably linked. These are one-minute stats.

Sample finite state machine object size counters:

-   **node\_get\_fsm\_objsize\_mean**: Mean object size encountered by
    this node within the last minute
-   **node\_get\_fsm\_objsize\_median**: Median object size encountered
    by this node within the last minute
-   **node\_get\_fsm\_objsize\_95**: 95th percentile object size
    encountered by this node within the last minute
-   **node\_get\_fsm\_objsize\_99**: 99th percentile object size
    encountered by this node within the last minute
-   **node\_get\_fsm\_objsize\_100** 100th percentile object size
    encountered by this node within the last minute

### Totals

Total Counters are data points that represent the total number of times
a particular activity has occurred since this node was started.

Sample total counters:

-   **vnode\_gets\_total** Number of GETs coordinated by local vnodes
    since node startup
-   **vnode\_puts\_total** Number of PUTS coordinated by local vnodes
    since node startup
-   **node\_gets\_total** Number of GETs coordinated by this node since
    startup, including GETs to non-local vnodes
-   **node\_puts\_total** Number of PUTs coordinated by this node since
    startup, including PUTs to non-local vnodes
-   **read\_repairs\_total** Number of Read Repairs this node has
    coordinated since startup
-   **coord\_redirs\_total** Number of requests this node has redirected
    to other nodes for coordination since startup

### CPU and Memory

CPU statistics are taken directly from Erlang’s cpu\\\_sup module.
Documentation for which can be found at [ErlDocs:
cpu\_sup](http://erldocs.com/R14B04/os_mon/cpu_sup.html).

-   **cpu\_nprocs**: Number of operating system processes
-   **cpu\_avg1**: The average number of active processes for the last 1
    minute (equivalent to top(1) command’s load average when divided by
    256()
-   **cpu\_avg5**: The average number of active processes for the last 5
    minutes (equivalent to top(1) command’s load average when divided by
    256()
-   **cpu\_avg15**: The average number of active processes for the last
    15 minutes (equivalent to top(1) command’s load average when divided
    by 256()

Memory statistics are taken directly from the Erlang virtual machine.
Documentation for which can be found at [ErlDocs:
Memory](http://erldocs.com/R14B04/erts/erlang.html?i=0&search=erlang:memory#memory/0).

-   **memory\_total**: Total allocated memory (sum of processes and
    system)
-   **memory\_processes**: Total amount of memory allocated for Erlang
    processes
-   **memory\_processes\_used**: Total amount of memory used by Erlang
    processes
-   **memory\_system**: Total allocated memory that is not directly
    related to an Erlang process
-   **memory\_atom**: Total amount of memory currently allocated for
    atom storage
-   **memory\_atom\_used**: Total amount of memory currently used for
    atom storage
-   **memory\_binary**: Total amount of memory used for binaries
-   **memory\_code**: Total amount of memory allocated for Erlang code
-   **memory\_ets**: Total memory allocated for Erlang Term Storage
-   **mem\_total**: Total available system memory
-   **mem\_allocated**: Total memory allocated for this node

### Miscellaneous Information

Miscellaneous Information stats are data points that provide details
particular to this node.

Sample miscellaneous information statistics:

-   **nodename** The name this node uses to identify itself
-   **ring\_num\_partitions** The configured number of partitions in the
    ring
-   **ring\_ownership**: List of all nodes in the ring and their
    associated partition ownership
-   **ring\_members**: List of nodes which are members of the ring
-   **connected\_nodes** A list of the nodes that this node is aware of
    at this time
-   **ignored\_gossip\_total**: Total number of ignored gossip messages
    since node was started
-   **handoff\_timeouts**: Number of handoff timeouts encountered by
    this node
-   **coord\_redirs\_total**: Number of requests this node has
    redirected to other nodes for coordination since startup
-   **precommit\_fail**: Number of pre commit hook failures
-   **postcommit\_fail**: Number of post commit hook failures
-   **sys\_driver\_version**: String representing the Erlang driver
    version in use by the runtime system
-   **sys\_global\_heaps\_size**: Current size of the shared global heap
-   **sys\_heap\_type**: String representing the heap type in use (one
    of private, shared, hybrid)
-   **sys\_logical\_processors**: Number of logical processors available
    on the system

{{#1.2.0+}}
### Pipeline Metrics

The following metrics from from riak_pipe are generated during MapReduce operations.

- **pipeline_active**: The number of pipelines active in the last 60 seconds
- **pipeline_create_count**: The total number of pipelines created since the node was started
- **pipeline_create_error_count**: The total number of pipeline creation errors since the node was started
- **pipeline_create_error_one**: The number of pipelines created in the last 60 seconds
- **pipeline_create_one**: The number of pipeline creation errors in the last 60 seconds
{{/1.2.0+}}

### Application and Subsystem Versions

The specific version of each Erlang application and subsystem which
makes up a Riak node is present in `riak-admin status` output.

-   **sys\_driver\_version**: String representing the Erlang driver
    version in use by the runtime system
-   **sys\_otp\_release**: Erlang OTP release version in use on the node
-   **sys\_system\_version**: Detailed Erlang version information
-   **ssl\_version**: Version of secure sockets layer (SSL) application
    in use
-   **public\_key\_version**: Version of public key application in use
-   **runtime\_tools\_version**: Version of runtime tools application in
    use
-   **basho\_stats\_version**: Version of Basho stats application in use
-   **riak\_search\_version**: Version of Riak Search application in use
-   **riak\_kv\_version**: Version of Riak KV application in use
-   **bitcask\_version**: Version of Bitcask backend application in use
-   **luke\_version**: Version of Luke application in use {{<1.3.0}}
-   **erlang\_js\_version**: Version of Erlang JS application in use
-   **mochiweb\_version**: Version of MochiWeb application in use
-   **inets\_version**: Version of Inets application in use
-   **riak\_pipe\_version**: Version of Riak Pipe application in use
-   **merge\_index\_version**: Version of Merge Index application in use
-   **cluster\_info\_version**: Version of Cluster Information
    application in use
-   **basho\_metrics\_version**: Version of Basho Metrics application in
    use
-   **riak\_control\_version**: Version of Riak Control application in
    use
-   **riak\_core\_version**: Version of Riak Core application in use
-   **lager\_version**: Version of Lager application in use
-   **riak\_sysmon\_version**: Version of Riak System Monitor
    application in use
-   **webmachine\_version**: Version of Webmachine application in use
-   **crypto\_version**: Version of Cryptography application in use
-   **os\_mon\_version**: Version of Operating System Monitor
    application in use
-   **sasl\_version**: Version of SASL application in use
-   **stdlib\_version**: Version of Standard Library application in use
-   **kernel\_version**: Version of Kernel application in use

{{#1.2.0+}}
### Riak Search Statistics

The following statistics related to Riak Search message queues are available.

- **riak_search_vnodeq_max**: Maximum number of unprocessed messages all
  virtual node (vnode) message queues in the Riak Search subsystem have
  received on this node in the last minute
- **riak_search_vnodeq_mean**: Mean number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node in the last minute
- **riak_search_vnodeq_median**: Median number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node in the last minute
- **riak_search_vnodeq_min**: Minimum number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node in the last minute
- **riak_search_vnodeq_total**: Total number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node since it was started
- **riak_search_vnodes_running**: Total number of vnodes currently running
  in the Riak Search subsystem

Note that under ideal operation and with the exception of
`riak_search_vnodes_running` these statistics should contain low values
(e.g., 0-10). Presence of higher values could be indicative of an issue.
{{/1.2.0+}}

Riaknostic
----------

[Riaknostic](http://riaknostic.basho.com/) is a small suite of
diagnostic checks that can be run against a Riak node to discover common
problems, and recommend how to resolve them. These checks are derived
from the experience of the Basho Client Services Team as well as
numerous public discussions on the mailing list, `#riak` IRC channel,
and other online media.

Riaknostic is open source, developed by Basho Technologies and members
of the Riak community, and the code is available in the [Riaknostic
Github repository](https://github.com/basho/riaknostic).

{{#1.3.0+}}
As of version 1.3 of Riak, Riaknostic is installed with Riak by default.
{{/1.3.0+}}

{{#<1.3.0}}
Getting started with Riaknostic is easy, and instructions for
installation and use are provided on the Riaknostic website. Once
downloaded and installed, Riaknostic adds a `diag` subcommand to the
`riak-admin` command.
{{/<1.3.0}}

Executing `riak-admin diag` will provide
information on any node problems as detected by Riaknostic, and also
recommendations for resolution of the problems. Riaknostic can be
extremely handy, and is strongly recommended as a first step when
inspecting a problematic node.


Related Resources
-----------------

-   [Configuration and Management: Command Line Tools:
    riak-admin](http://docs.basho.com/riak/1.2.0/references/Command-Line-Tools---riak-admin/)
-   [Riaknostic](http://riaknostic.basho.com/)
-   [[HTTP API status|HTTP Status]]
