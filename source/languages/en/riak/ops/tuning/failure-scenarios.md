---
title: Common Failure Scenarios
project: riak
version: 2.0.0+
document: cookbook
audience: intermediate
keywords: [operator, failure, troubleshooting]
---

This document walks you through a number of failure scenarios that you
may encounter, pointing to relevant documentation and suggesting ways of
addressing the problem.

If you don't a specific problem addressed in this document, it may also
be worthwhile to check out our [[troubleshooting checklist]]. This guide
provides general information about how to begin to address a wide
variety of issues in Riak.

## High Latency

If your cluster is experiencing latency issues, we recommend consulting
our [[Latency Reduction Checklist]].

## Node Has Crashed Unexpectedly

For reasons that are not immediately clear, a node in your cluster has
crashed, i.e. is not responsive or has gone offline. There are a few
potential causes of this:

* The node is out of memory
* Data corruption
* An expensive and/or long-running query is in progress

## Node Won't Start

If you're attempting to start a node (using the `[[riak start|riak
Command Line#start]]` command) and the operation keeps timing out or
throwing errors, there's a good chance that there's either an error in
your [[configuration files]] or a pre-condition for running Riak in your
environment that is not being met.

* Check for configuration errors by running the `[[riak chkconfig|riak
    Command Line#chkconfig]]` command. If the output is `config is OK`,
    then your configuration files are syntactically correct. Please
    note, though, that there may be other configuration issues involved
    that are not syntactic. More information can be found in [[Checking
    Your Configuration|Configuration
    Files#Checking-Your-Configuration]].
* Riak tends to require a lot of open files. Some operating systems, in
    particular Mac OS X, have an open files limit that is below what
    Riak requires. If this is a problem in your OS, Riak will provide a
    warning when you attempt to start the node. See our [[Open Files
    Limit]] documentation for more information.

## Node Won't Stop

If you're attempting to stop a node (using the `[[riak stop|riak Command
Line#stop]]` command) and you get `ok` as a response, then the operation
was successful. If not, then the stop operation has probably failed.
This problem usually arises when there is some set of work that the node
is scheduled to perform that has not yet been completed. This might
involve one of the following:

* **Replication queue**
* **Unfinished compactions**

If all else fails and it's imperative that the node be stopped, you can
always resort to the
`[kill](http://www.cyberciti.biz/faq/unix-kill-command-examples/)`
command, though we do not advise this.

## Disk is Full

If a Riak node's disk is full or getting perilously close to it, this
can cause a wide variety of problems and should be avoided at all costs.
See [[Cluster Capacity Planning]] for information on how to avoid this
possibility.

If, however, you need to reclaim disk space immediately, we recommend
beginning in these two places:

* [[Bitcask]] hintfiles --- These files are used by Bitcask to build an
    in-memory representation of the current key space. If you are using
    Bitcask, they can be deleted if necessary; if you are not using
    Bitcask, disregard this suggestion. You can find them in the node's
    `/data` directory, in the `/bitcask` subdirectory. Within the
    `/bitcask` directory you'll find a subdirectory for each
    [[vnode|Riak Glossary#vnodes]] data partition within the node. Each
    partition contains a `*.bitcask.hint` file, where the `*` is an
    assigned number. Those are the files that you must remove. One
    drawback to eliminating Bitcask hintfiles is that Bitcask will
    likely take longer to start up when the node is restarted. If disk
    space has become a serious issue, however, then deleting hintfiles
    is probably worth the risk.
* Active anti-entropy \(AAE) files --- These files are used by
    Riak's [[active anti-entropy]] subsystem. By default, they reside in
    each node's `/data` directory, in the `/anti_entropy` subdirectory
