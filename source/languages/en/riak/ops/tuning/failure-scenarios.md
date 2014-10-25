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

If you have a node that won't seem to start, there's a good chance that
there is a either an error in your [[configuration files]] or there's a
pre-condition for running Riak in your environment that is not being
met.

* Check for configuration errors by running the `[[riak chkconfig|riak
    Command Line#chkconfig]]` command. If the output is `config is OK`,
    then your configuration files are syntactically correct. Please
    note, though, that there may be other configuration issues involved
    that are not syntactic. More information can be found in [[Checking
    Your Configuration|Configuration
    Files#Checking-Your-Configuration]].

