---
title: Command Line Tools
project: riak
version: 0.10.0+
document: reference
toc: true
index: true
audience: beginner
keywords: [command-line]
moved: {
    '1.4.0-': '/cookbooks/Repairing-KV-Indexes'
}
---

This is a description of the Riak command-line tools and their subcommands.
These tools are located in the `bin` directory of an embedded node, and the
path when installed from a package (usually `/usr/sbin` or `/usr/local/sbin`).

This is a description of the Riak command-line tools and their subcommands.
These tools are located in the `bin` directory of an embedded node, and the
path when installed from a package (usually `/usr/sbin` or `/usr/local/sbin`).

## riak

`[[riak|riak Command Line]]` is the primary script for controlling the
Riak node process. The following subcommands are available:

* [[start|riak Command Line#start]]
* [[stop|riak Command Line#stop]]
* [[restart|riak Command Line#restart]]
* [[reboot|riak Command Line#reboot]]
* [[ping|riak Command Line#ping]]
* [[console|riak Command Line#console]]
* [[attach|riak Command Line#attach]]
* [[chkconfig|riak Command Line#chkconfig]]

## riak-admin

`[[riak-admin|riak-admin Command Line]]` performs operations not related
to node liveness, including node membership, backup, and basic status reporting. The node must be running for most of these commands to work.
The following subcommands are supported:

* [[cluster|riak-admin Command Line#cluster]]
  * [[cluster join|riak-admin Command Line#cluster join]]
  * [[cluster leave|riak-admin Command Line#cluster leave]]
  * [[cluster force-remove|riak-admin Command Line#cluster force-remove]]
  * [[cluster replace|riak-admin Command Line#cluster replace]]
  * [[cluster force-replace|riak-admin Command Line#cluster force-replace]]
* [[join|riak-admin Command Line#join]]
* [[leave|riak-admin Command Line#leave]]
* [[backup|riak-admin Command Line#backup]]
* [[restore|riak-admin Command Line#restore]]
* [[test|riak-admin Command Line#test]]
* [[status|riak-admin Command Line#status]]
* [[reip|riak-admin Command Line#reip]]
* [[js-reload|riak-admin Command Line#js-reload]]
* [[wait-for-service|riak-admin Command Line#wait-for-service]]
* [[services|riak-admin Command Line#services]]
* [[ringready|riak-admin Command Line#ringready]]
* [[transfers|riak-admin Command Line#transfers]]
* [[force-remove|riak-admin Command Line#force-remove]]
* [[down|riak-admin Command Line#down]]
* [[cluster-info|riak-admin Command Line#cluster-info]]
* [[member-status|riak-admin Command Line#member-status]]
* [[ring-status|riak-admin Command Line#ring-status]]
* [[vnode-status|riak-admin Command Line#vnode-status]]
