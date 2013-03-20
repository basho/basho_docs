---
title: Riak CS Using Chef
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing, chef]
---

With Chef, you can use a recipe to automatically install Riak CS on all the nodes in your system. A recipe contains a collection of resources for configuring a system, which are executed in order.

To learn more about configuring a Chef server and its clients, use the following links to access the OpsCode Wiki. OpsCode also offers a hosted Chef Platform that you can use.

* [[http://wiki.opscode.com]]
* [[http://www.opscode.com/hosted-chef]]

<div class="note">This section assumes that you have a working knowledge of Chef.</div>

## Cookbooks for Riak CS

This section describes at a high level the two Chef Cookbooks that you should use to properly install a Riak CS system.

### Riak Cookbook

**riak::default recipe**

The Riak Cookbook has been available since Riak release 0.14 and has been used by Basho and many Basho customers to install the software. This cookbook automatically downloads and installs specified versions of Riak. The Cookbook also generates the Erlang configuration file for Riak.

**riak::autoconf recipe**

The Riak Cookbook has an extra recipe for automatic configuration.

<div class="note"><div class="title">Note</div> This automatic configuration recipe works only with a full Chef Stack, not chef-solo.</div>

For more information, see the Riak Cookbook's [[README|https://github.com/basho/riak-chef-cookbook]] file.

### Riak CS Cookbook
The Riak CS Cookbook installs both Riak CS and Stanchion. The Riak CS Cookbook contains two recipes.

**RiakCS::default recipe**

The default recipe downloads and installs the version of Riak CS specified. The version can be specified or overridden in package.rb or in a node attribute. This Cookbook also generates the Riak CS Erlang configuration file.

**RiakCS::stanchion recipe**

Stanchion should be installed on only one node in a Riak CS system, so the stanchion recipe should be applied only to one Riak CS node.

<div class="note"><div class="title">Note</div>Running Stanchion on more than one node can lead to problems if Riak CS nodes are configured to communicate using multiple Stanchion nodes. In this situation, the uniqueness of bucket names and user email addresses might not be enforced, which, in turn, could lead to unexpected behavior.</div>

For more information, see the Riak CS Cookbook's [[repository|https://github.com/basho/riak-cs-chef-cookbook]].
