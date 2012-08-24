At some point, for various reasons, you might need to replace a node in your cluster (which is different from [[recovering a failed node|Recovering a failed node]]). Here is the recommended way to go about doing that:

* Tar up your data directory on the node in question (which should be found at `riak/data`).
* Download and install Riak on the new node you wish to bring into the cluster. Don't start it just yet. 
* Copy the data from the node you're decommissioning to the new machine and untar it. Also make sure to copy the existing ring file to the new node.
* Run [[reip|Command-Line Tools#reip]] on the new node.
* Stop the old node using [[riak stop|Command-Line Tools#stop]].
* Start the new machine with [[riak start|Command-Line Tools#start]]. 

<div class="info">
You'll need to make sure that no other ring changes occur between the time when you start the new node and the ring settles with the new IP info.

The ring is considered settled when the new node reports `true` using the 
[[ringready|Command-Line Tools#ringready]] command.
</div>
