---
title: Command Line Tools
project: riak
version: 0.10.0+
document: reference
toc: true
index: true
audience: beginner
keywords: [command-line]
---

This is a description of the Riak command-line tools and their subcommands.
These tools are located in the `bin` directory of an embedded node, and the path
when installed from a package (usually `/usr/sbin` or `/usr/local/sbin`).

This is a description of the Riak command-line tools and their subcommands.
These tools are located in the `bin` directory of an embedded node, and the path
when installed from a package (usually `/usr/sbin` or `/usr/local/sbin`).

## riak

`[[riak|Command Line Tools - riak]]` is the primary script for controlling the
Riak node process. The following subcommands are avaiable:

* [[start|Command Line Tools - riak#start]]
* [[stop|Command Line Tools - riak#stop]]
* [[restart|Command Line Tools - riak#restart]]
* [[reboot|Command Line Tools - riak#reboot]]
* [[ping|Command Line Tools - riak#ping]]
* [[console|Command Line Tools - riak#console]]
* [[attach|Command Line Tools - riak#attach]]
* [[chkconfig|Command Line Tools - riak#chkconfig]]

## riak-admin

`[[riak-admin|Command Line Tools - riak-admin]]` performs operations not related
to node-liveness, including node membership, backup, and basic status reporting.
The node must be running for most of these commands to work. The following
subcommands are supported:

* [[cluster|Command Line Tools - riak-admin#cluster]]
  * [[cluster join|Command Line Tools - riak-admin#cluster join]]
  * [[cluster leave|Command Line Tools - riak-admin#cluster leave]]
  * [[cluster force-remove|Command Line Tools - riak-admin#cluster force-remove]]
  * [[cluster replace|Command Line Tools - riak-admin#cluster replace]]
  * [[cluster force-replace|Command Line Tools - riak-admin#cluster force-replace]]
* [[join|Command Line Tools - riak-admin#join]]
* [[leave|Command Line Tools - riak-admin#leave]]
* [[backup|Command Line Tools - riak-admin#backup]]
* [[restore|Command Line Tools - riak-admin#restore]]
* [[test|Command Line Tools - riak-admin#test]]
* [[status|Command Line Tools - riak-admin#status]]
* [[reip|Command Line Tools - riak-admin#reip]]
* [[js-reload|Command Line Tools - riak-admin#js-reload]]
* [[wait-for-service|Command Line Tools - riak-admin#wait-for-service]]
* [[services|Command Line Tools - riak-admin#services]]
* [[ringready|Command Line Tools - riak-admin#ringready]]
* [[transfers|Command Line Tools - riak-admin#transfers]]
* [[force-remove|Command Line Tools - riak-admin#force-remove]]
* [[down|Command Line Tools - riak-admin#down]]
* [[cluster-info|Command Line Tools - riak-admin#cluster-info]]
* [[member-status|Command Line Tools - riak-admin#member-status]]
* [[ring-status|Command Line Tools - riak-admin#ring-status]]
* [[vnode-status|Command Line Tools - riak-admin#vnode-status]]

## search-cmd

`[[search-cmd|Command Line Tools - search-cmd]]` is used to interact with the
Search functionality included with Riak. The following subcommands are
supported:

* [[set-schema|Command Line Tools - search-cmd#set-schema]]
* [[show-schema|Command Line Tools - search-cmd#show-schema]]
* [[clear-schema-cache|Command Line Tools - search-cmd#clear-schema-cache]]
* [[search|Command Line Tools - search-cmd#search]]
* [[search-doc|Command Line Tools - search-cmd#search-doc]]
* [[explain|Command Line Tools - search-cmd#explain]]
* [[index|Command Line Tools - search-cmd#index]]
* [[delete|Command Line Tools - search-cmd#delete]]
* [[solr|Command Line Tools - search-cmd#solr]]
* [[install|Command Line Tools - search-cmd#install]]
* [[uninstall|Command Line Tools - search-cmd#uninstall]]
* [[test|Command Line Tools - search-cmd#test]]