<div class="info"><div class="title">Riak Enterprise Only</div>This documentation applies only to Riak Enterprise, Basho's commercial extension to <a href="http://wiki.basho.com/Riak.html">Riak</a>. To learn more about the differences between Riak and Riak Enterprise, <a href="http://basho.com/products/riak-overview/">read here</a>.  To talk to us about using Riak Enterprise,  <a href="http://info.basho.com/Wiki_Contact.html" target="_blank">let us know</a>.</div>

## Scheduling Full Synchronization
With the pause and resume commands it is possible to limit full synchronizations to off-peak times. First, disable fullsync_interval and set fullsync_on_connect to false. Then using cron or similar execute the commands below at the start of the sync window:

    #! /bin/sh
    ## Resume from where we left off
    riak-repl resume-fullsync
    ## Start fullsync if nothing is running
    riak-repl start-fullsync

At the end of the sync window:

    #! /bin/sh
    ## Stop fullsync until start of next sync window
    riak-repl pause-fullsync
