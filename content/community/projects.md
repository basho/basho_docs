---
title: "Community Projects"
description: ""
menu:
  community:
    name: "Community Projects"
    identifier: "community_projects"
    weight: 400
    parent: "community_overview"
toc: true
aliases:
  - /riak/2.1.3/dev/advanced/community-projects/
  - /riak/2.1.1/dev/advanced/community-projects/
  - /riak/2.0.6/dev/advanced/community-projects/
  - /riak/2.0.5/dev/advanced/community-projects/
  - /riak/2.0.4/dev/advanced/community-projects/
  - /riak/2.0.3/dev/advanced/community-projects/
  - /riak/2.0.2/dev/advanced/community-projects/
  - /riak/2.0.1/dev/advanced/community-projects/
  - /riak/2.0.0/dev/advanced/community-projects/
---

Here are all projects that have come about out of community contribution. [Let us know]({{<communityemail>}}) if you choose to maintain one of them!

## Install and Configure

Here are the many recommended ways to get Riak KV started in your environment.

* All pre-built packages at Basho are managed on [PackageCloud](https://packagecloud.io/basho/)
* Spin up [Riak KV on AWS using the latest AMI](https://aws.amazon.com/marketplace/pp/B00YFZ60X2/ref=sp_mpg_product_title?ie=UTF8&sr=0-2), pre-configured and optimized for use on AWS
* [Riak KV Chef Recipe](https://github.com/basho-labs/riak-chef-cookbook) provisions using [Chef](http://www.chef.io)
* [Riak KV Puppet Module](https://github.com/basho-labs/puppet-riak) provisions using [Puppet Labs](http://www.puppetlabs.com)
* [Riak KV Ansible Role](https://github.com/basho-labs/ansible-riak) provisions using [Ansible](http://www.ansible.com)
* [Cloudsoft-Riak](https://github.com/cloudsoft/amp-basho) is a tested Riak blueprints for developing and deploying using Cloudsoft
* [Riak Mesos Framework](https://github.com/basho-labs/riak-mesos) is an experimental beta project that deploys Riak KV on top of Mesosphere's DCOS. See how it works with [this demo](http://basho-labs.github.io/riak-mesos/).

### Local Development

There are a number of tools to get Riak running on your local system.

* [The Riak App](https://github.com/basho-labs/riak-app) is an OS X application to run Riak KV on a Mac
* [Vagrant-riak-meta](https://github.com/basho-labs/vagrant-riak-meta) is a collection of Vagrantfiles to spin up different versions of Riak on  your laptop using Hashicorp's [Vagrant](http://vagrantup.com)

## Sample Applications

This a collection of sample applications built on Riak and Riak Core.

### Riak

* [Zombie Riak](https://github.com/basho-labs/vagrant-zombie-riak) is an implementation of inverted-indexes to fight off the zombie apocalypse
* [Dynamiq](https://github.com/tapjoy/dynamiq) is a message queue powered by Riak KV and written in Golang. [Read more](http://basho.com/posts/technical/built-on-riak-dynamiq-by-tapjoy/) about it
* [Notification](https://github.com/smoketurner/notification/) is an HTTP-based notification web service, based on Yammer's Streamie service, written in Java on top of Riak KV
* [Pyoko](https://github.com/zetaops/pyoko) is a Django-esque ORM for Riak KV written in python

### Riak Core

_Riak Core (or `riak_core` as it's written in the code) is the distributed systems framework that underpins Riak. For more general information on Riak Core, [start with this blog post](http://blog.basho.com/2011/04/12/Where-To-Start-With-Riak-Core/)._

* [NKcluster](https://github.com/Nekso/nkcluster) is a framework for creating clusters of Erlang nodes of any size, and distributing and managing jobs
* [Basho Didgeridoo](https://github.com/cmeiklejohn/BashoDidgeridoo) is an application that uses Riak Core to play distributed music

## Monitoring and Management

Here are the **most active** contributions from the community.

* [Riak Explorer](https://github.com/basho-labs/riak_explorer) is an open source tool that provides a convenient method to browse Bucket Types, Buckets, Keys, view and edit Riak Objects.
* The [Boundary Riak plugin](https://github.com/boundary/boundary-plugin-riak) collects information on Riak clusters for consumption in [Boundary](https://www.boundary.com/)
* [Datadog Riak Plugin](http://docs.datadoghq.com/integrations/riak/) collects Riak metrics for the [Datadog](http://www.datadoghq.com/) monitoring service
* [Riak Zabbix](https://github.com/basho-labs/riak-zabbix) is a template and setup guide to monitor Riak KV
* [Nagios Plugin](https://github.com/basho-labs/riak_nagios) maintained by the Basho community
* [Advanced Nagios Plugins Collection](https://github.com/harisekhon/nagios-plugins) contains many additional Nagios plugins for monitoring Riak
* [New Relic Plugin](https://github.com/basho/riak_newrelic) serves node statistics of a Riak Node to the New Relic APM System
* [Yokozuna Monitor](https://github.com/basho-labs/ruby-yz-monitor) is a ruby application to monitor your Riak Search activity.
* [riak-statsd in golang](https://github.com/jjmalina/riak-statsd) which monitors Riak KV and pushes to statsd
* [Gmond Python Modules for Riak](https://github.com/ganglia/gmond_python_modules) is a Ganglia Module for connecting to Riak KV
* [Riak Key List Utility](https://github.com/basho-labs/riak-key-list-util) is a console utility script for per-vnode key counting, siblings logging and more

## Backup Tools

Here are some known **most active** contributions from the community.

* [riak-data-migrator](https://github.com/basho-labs/riak-data-migrator)for migrating data from one or more buckets in a Riak KV store into another Riak cluster by exporting all data from buckets
* [riak-extl](https://github.com/basho-labs/riak-extl) is a data migrator written in Elixir
* [riak-tools](https://github.com/sqor/riak-tools) is an easy way to backup Riak KV clusters running on AWS

## Other Tools and Projects

Some projects have lost its maintainer with time. Here are all projects that haven't seen commits in quite a while and may be helpful as reference. [Let us know]({{<communityemail>}}) if you choose to maintain one of them!

* [Brackup](http://code.google.com/p/brackup/) --- A modern net-based backup system that supports de-duplication, intelligent chunking, and [GPG](http://en.wikipedia.org/wiki/GNU_Privacy_Guard)-based-encryption
* [riak_mapreduce_utils](http://github.com/whitenode/riak_mapreduce_utils) --- A library of mapreduce utility functions developed in Erlang
* [riakbloom](http://github.com/whitenode/riakbloom) --- A solution allowing Bloom filters to be created and used in MapReduce jobs
* [Qi4j Riak EntityStore](http://qi4j.org/extension-es-riak.html) --- Qi4j EntityStore service backed by a Riak bucket
* [ldapjs-riak](https://github.com/mcavage/node-ldapjs-riak) --- A Riak backend for [ldapjs](http://ldapjs.org)
* [otto](https://github.com/ncode/otto) --- S3 Clone built on top of Cyclone with support for Riak
* [Riaktivity](https://github.com/roidrage/riaktivity) --- A Ruby library for storing timelines in Riak
* [Timak](https://github.com/bretthoerner/timak) --- A Python library for storing timelines (activity streams) in Riak
* [Statebox_Riak](https://github.com/mochi/statebox_riak) --- Convenience library that makes it easier to use [Statebox](https://github.com/mochi/statebox) with Riak. There is a great blog post from the Mochi Team about how this is used in production [here](http://labs.mochimedia.com/archive/2011/05/08/statebox/).
* [bitcask-ruby](https://github.com/aphyr/bitcask-ruby) --- An interface to the Bitcask storage system
* [Riak BTree Backend](https://github.com/krestenkrab/riak_btree_backend) --- Backend for Riak/KV based on couch_btree*
* [Riak Link Index](https://github.com/krestenkrab/riak_link_index) --- Simple Indexer for Riak based on Links
* [rack-rekon](https://github.com/seomoz/rack-rekon) --- A Rack application to serve [Rekon](https://github.com/adamhunter/rekon/)
* [ring-session-riak](https://github.com/ossareh/ring-session-riak) --- A Riak implementation for Ring Session
* [Riak to CSV Export](https://github.com/bradfordw/riak_csv) --- A simple way to export your Riak buckets to CSV files
* [Couch to Riak](http://github.com/mattsta/couchdb/tree/couch_file-to-riak)
* [Chimera](http://github.com/benmyles/chimera) --- An object mapper for Riak and Redis
* [Riak_Redis Backend](http://github.com/cstar/riak_redis_backend)
* [Riak Homebrew Formula](http://github.com/roidrage/homebrew)
* [Riak-fuse --- A FUSE Driver for Riak](http://github.com/johnthethird/riak-fuse)
* [riakfuse](http://github.com/crucially/riakfuse) --- A distributed filesystem that uses Riak as its backend store
* [ebot](http://www.redaelli.org/matteo-blog/projects/ebot/) --- A scalable web crawler that supports Riak as a backend
* [riak-jscouch](https://github.com/jimpick/riak-jscouch) --- JSCouch examples done with Riak
* [riak_tokyo_cabinet](http://github.com/jebu/riak_tokyo_cabinet) --- A Tokyo Cabinet backend for Riak
* [Logstash Riak Output](http://logstash.net/docs/1.1.9/outputs/riak) --- An output plugin for Logstash
* [Fluentd plugin for Riak](http://github.com/kuenishi/fluent-plugin-riak) --- An output plugin for [Fluentd](http://fluentd.org)
* [Riak/RabbitMQ Commit Hook](https://github.com/jbrisbin/riak-rabbitmq-commit-hooks) --- A post-commit hook that sends entries into a RabbitMQ broker using the Erlang AMQP client
* [riak-exchange](https://github.com/jbrisbin/riak-exchange) --- Custom RabbitMQ exchange type for sticking messages in Riak
* [rabbit_riak_queue](https://github.com/jbrisbin/rabbit_riak_queue) --- Riak-backed RabbitMQ persistent queue implementation
* [msg_store_bitcask_index](https://github.com/videlalvaro/msg_store_bitcask_index) --- RabbitMQ message store index with Bitcask Backend
* [RabbitMQ riak_core Vnode Dispatcher](https://github.com/jbrisbin/rabbitmq-riak_core-vnode-dispatcher) --- An example of how to dispatch a web request into a riak_core Vnode
* [Lager AMQP Backend](https://github.com/jbrisbin/lager_amqp_backend) --- AMQP RabbitMQ Lager backend
* [Rekon](https://github.com/basho/rekon) --- Riak Node Data Browser
* [Nagios Plugins for Riak](https://github.com/xb95/nagios-plugins) by xb95
* [Scalarium-Riak](https://github.com/roidrage/scalarium-riak) --- Riak Cookbooks for Scalarium Platform
* [Riak Chef Recipe](https://github.com/basho/riak-chef-cookbook) --- Vanilla Chef Recipe for installing and configuring Riak
* [RiakAWS](http://github.com/roder/riakaws) --- A simple way to deploy a Riak cluster in the Amazon Cloud
* [Cloudsoft-Riak](https://github.com/cloudsoft/amp-basho) --- Tested and optimized Riak blueprints for developing and deploying applications faster.
* [Using Nginx as a front-end for Riak](http://rigelgroupllc.com/wp/blog/using-nginx-as-a-front-end-for-riak)
* [Sample HA Proxy Configuration for Protocol Buffers Interface](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-May/004387.html) (courtesy of Scott M. Likens)
* [Sample HA Proxy Configuration for Protocol Buffers Interface](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-May/004388.html) (courtesy of Bob Feldbauer)
* [Storing Apache Logs in Riak via Fluentd](http://docs.fluentd.org/articles/apache-to-riak)
* [yakriak](http://github.com/seancribbs/yakriak) --- Riak-powered Ajax-polling chatroom
* [riaktant](https://github.com/basho/riaktant) --- A full-blown NodejS app that stores and makes syslog messages searchable in Riak Search
* [selusuh](https://github.com/OJ/selusuh) --- Riak application that presents JSON slide decks (thanks, [OJ](http://twitter.com/thecolonial)!)
* [Rekon](https://github.com/adamhunter/rekon) --- A Riak data browser, built as a totally self-contained Riak application
* [Slideblast](https://github.com/rustyio/SlideBlast) --- Share and control slide presentation for the web
* [riak_php_app](http://github.com/schofield/riak_php_app) --- A small PHP app that shows some basic usage of the Riak PHP library
* [riak-url-shortener](http://github.com/seancribbs/riak-url-shortener) --- A small Ruby app (with Sinatra) that creates short URLs and stores them in Riak
* [wriaki](https://github.com/basho-labs/wriaki) --- A wiki app backed by Riak
* [riagi](https://github.com/basho-labs/riagi) --- A simple imgur.com clone built using Riak, Django, and Riak Search
* [riak-session-manager](https://github.com/jbrisbin/riak-session-manager) --- A Riak-backed Tomcat Session Manager
* [riak_id](https://github.com/seancribbs/riak_id) --- A clone of Twitter's Snowflake, built on riak_core
* [riak_zab](https://github.com/jtuple/riak_zab) --- An implementation of the Zookeeper protocol on top of Riak Core
* [Misultin `riak_core` Vnode Dispatcher](https://github.com/jbrisbin/misultin-riak-core-vnode-dispatcher) --- An example of how to dispatch a web request into a `riak_core` vnode
* [ecnty](https://github.com/benmmurphy/ecnty) --- Partitioned counter based on Riak Core
* [`rebar_riak_core`](https://github.com/websterclay/rebar_riak_core) --- Rebar templates for generating `riak_core` applications
* [riak_node (for Munin)](https://github.com/munin-monitoring/contrib/blob/master/plugins/riak/riak_node) --- Munin plugin for monitoring GET and PUT traffic
* [riak_memory (for Munin)](https://github.com/munin-monitoring/contrib/blob/master/plugins/riak/riak_memory) --- Munin plugin for monitoring memory allocation
* [Riak-Console](https://github.com/lucaspiller/riak-console) --- An interactive command line interface to Riak
* [riak-admin](https://github.com/pentium10/riak-admin) --- Admin panel written in PHP (supports delete bucket via keys stream)
* [riak_stats](https://gist.github.com/4064937) --- A shell script to ship `riak-admin` statistics to [Librato](https://metrics.librato.com)
* [riak-manage](https://github.com/basho-labs/riak-manage) is a toolset is a project to manage Riak KV clusters
