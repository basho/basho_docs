---
title: "Multi Data Center Replication: Scheduling Full Sync"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl]
---

## Scheduling Full Synchronization
With the pause and resume commands it is possible to limit full synchronizations to off-peak times. First, disable `fullsync_interval` and set `fullsync_on_connect` to false. Then using cron or similar execute the commands below at the start of the sync window:

    #! /bin/sh
    ## Resume from where we left off
    riak-repl resume-fullsync
    ## Start fullsync if nothing is running
    riak-repl start-fullsync

At the end of the sync window:

    #! /bin/sh
    ## Stop fullsync until start of next sync window
    riak-repl pause-fullsync
