---
title: Community Projects
project: riak
version: 0.10.0+
document: reference
toc: true
index: true
audience: intermediate
keywords: [client, drivers]
moved: {
  '1.4.0-': '/references/Community-Developed-Libraries-and-Projects'
}
---

## Monitoring, Management, and GUI Tools

* [[riak_node (for Munin)|https://github.com/munin-monitoring/contrib/blob/master/plugins/riak/riak_node]] - Munin plugin for monitoring GET and PUT traffic
* [[riak_memory (for Munin)|https://github.com/munin-monitoring/contrib/blob/master/plugins/riak/riak_memory]] - Munin plugin for monitoring memory allocation
* [[Nagios Plugins for Riak|https://github.com/xb95/nagios-plugins]]  
* [[Riak-Console|https://github.com/lucaspiller/riak-console]] - An interactive command line interface to Riak 
* [[Rekon|https://github.com/basho/rekon]] - Riak Node Data Browser 
* [[Gmond Python Modules for Riak|http://github.com/jnewland/gmond_python_modules/tree/master/riak]] - Ganglia Module for connecting to Riak
* [[riak-admin|http://bitbucket.org/harmen/riak-admin/]] - A Java program with GUI to browse and update a Riak database
* [[Riak Admin|http://github.com/frank06/riak-admin]] - A Futon-like web interface for Riak
* [[riak-session-manager|https://github.com/jbrisbin/riak-session-manager]] - A Riak-backed Tomcat Session Manager
* [[app-karyn|https://github.com/tempire/app-karyn]] - Simple command line utility for working with Riak objects
* [[Briak|http://github.com/johnthethird/Briak]] - A Sinatra-based web front-end browser for Riak
* [[riak_stats|https://gist.github.com/4064937]] - A shell script to ship riak-admin statistics to [[Librato|https://metrics.librato.com/]]
* [[riak_graphite_stats|https://gist.github.com/4064990]] - A shell script to ship riak-admin statistics to [[Graphite|http://graphite.wikidot.com/]]
        
## Backup Tools

* [[Brackup|http://code.google.com/p/brackup/]] - A modern net-based backup system that supports de-duplication, intelligent chunking, and gpg-based-encryption

## riak_core

* [[Misultin riak_core Vnode Dispatcher|https://github.com/jbrisbin/misultin-riak-core-vnode-dispatcher]] - An example of how to dispatch a web request into a riak_core Vnode
* [[ecnty|https://github.com/benmmurphy/ecnty]] - Partitioned Counter Based on Riak Core 
* [[rebar_riak_core|https://github.com/websterclay/rebar_riak_core]] - Rebar templates for generating riak_core applications
* [[Try Try Try|https://github.com/rzezeski/try-try-try/]] - Ryan Zezeski's working blog that explores many aspects of riak_core (this is an amazing resource)
* [[riak_zab|https://github.com/jtuple/riak_zab]] - an extension for riak_core that provides totally ordered atomic broadcast capabilities 
* [[riak_zab_example|https://github.com/jtuple/riak_zab_example]] - an example application that allows you to build a multi-node cluster using riak_zab


## Riak and RabbitMQ 

* [[Riak/RabbitMQ Commit Hook|https://github.com/jbrisbin/riak-rabbitmq-commit-hooks]] - A Post-Commit hook that sends entries into a RabbitMQ broker using the Erlang AMQP client
* [[riak-exchange|https://github.com/jbrisbin/riak-exchange]] - Custom RabbitMQ exchange type for sticking messages in Riak 
* [[rabbit_riak_queue|https://github.com/jbrisbin/rabbit_riak_queue]] - Riak-backed RabbitMQ persistent queue implementation
* [[msg_store_bitcask_index|https://github.com/videlalvaro/msg_store_bitcask_index]] - RabbitMQ message store index with Bitcask Backend
* [[RabbitMQ riak_core Vnode Dispatcher|https://github.com/jbrisbin/rabbitmq-riak_core-vnode-dispatcher]] - An example of how to dispatch a web request into a riak_core Vnode

## Lager 

* [[Lager AMQP Backend|https://github.com/jbrisbin/lager_amqp_backend]] - AMQP RabbitMQ Lager backend


## Recipes, Cookbooks, and Configurations

* [[Scalarium-Riak|https://github.com/roidrage/scalarium-riak]] - Riak Cookbooks for Scalarium Platform
* [[Riak Chef Recipe|https://github.com/basho/riak-chef-cookbook]] - Vanilla Chef Recipe for installing and configuring Riak
* [[Custom Chef Recipe for running Riak on the Engine Yard AppCloud|https://github.com/engineyard/ey-cloud-recipes/tree/master/cookbooks/riak]]
* [[RiakAWS|http://github.com/roder/riakaws]] - A simple way to deploy a Riak cluster in the Amazon Cloud
* [[Using Nginx as a front-end for Riak|http://rigelgroupllc.com/wp/blog/using-nginx-as-a-front-end-for-riak]]
* [Sample HA Proxy Configuration for Protocol Buffers Interface](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-May/004387.html)  (courtesy of Scott M. Likens)
* [Sample HA Proxy Configuration for Protocol Buffers Interface](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-May/004388.html)  (courtesy of Bob Feldbauer)


## Other Tools and Projects

* [[riak_mapreduce_utils|http://github.com/whitenode/riak_mapreduce_utils]] - A library of mapreduce utility functions developed in Erlang
* [[riakbloom|http://github.com/whitenode/riakbloom]] - A solution allowing Bloom filters to be created and used in mapreduce jobs
* [[Qi4j Riak EntityStore|http://qi4j.org/extension-es-riak.html]] - Qi4j EntityStore service backed by a Riak bucket
* [[ldapjs-riak|https://github.com/mcavage/node-ldapjs-riak]] - A Riak backend for [[ldapjs|http://ldapjs.org]]
* [[otto|https://github.com/ncode/otto]] - S3 Clone built on top of Cyclone with support for Riak
* [[Riaktivity|https://github.com/roidrage/riaktivity]] - A Ruby library for storing timelines in Riak
* [[Timak|https://github.com/bretthoerner/timak]] - A Python library for storing timelines (activity streams) in Riak
* [[Statebox_Riak|https://github.com/mochi/statebox_riak ]] - Convenience library that makes it easier to use [[Statebox|https://github.com/mochi/statebox]] with Riak. (There is a great blog post from the Mochi Team about how this is used in production [[here|http://labs.mochimedia.com/archive/2011/05/08/statebox/]].  )
* [[bitcask-ruby|https://github.com/aphyr/bitcask-ruby]] - An interface to the Bitcask storage system
* [[Riak BTree Backend|https://github.com/krestenkrab/riak_btree_backend]] - Backend for Riak/KV based on couch_btree*
* [[Riak Link Index|https://github.com/krestenkrab/riak_link_index]] - Simple Indexer for Riak based on Links
* [[rack-rekon|https://github.com/seomoz/rack-rekon]] - A Rack application to serve [[Rekon|https://github.com/adamhunter/rekon/]]
* [[ring-session-riak|https://github.com/ossareh/ring-session-riak]] - A Riak implementation for Ring Session
* [[Riak to CSV Export|https://github.com/bradfordw/riak_csv]] - A simple way to export your Riak buckets to CSV files
* [[Couch to Riak|http://github.com/mattsta/couchdb/tree/couch_file-to-riak]]
* [[Chimera|http://github.com/benmyles/chimera]] - An object mapper for Riak and Redis
* [[Riak_Redis Backend|http://github.com/cstar/riak_redis_backend]]
* [[Riak Homebrew Formula|http://github.com/roidrage/homebrew]]
* [[Riak-fuse - A FUSE Driver for Riak|http://github.com/johnthethird/riak-fuse]]
* [[riakfuse|http://github.com/crucially/riakfuse]] - A distributed filesystem that uses riak as its backend store
* [[ebot|http://www.redaelli.org/matteo-blog/projects/ebot/]] - A scalable Web Crawler that supports Riak as a backend
* [[riak-jscouch|https://github.com/jimpick/riak-jscouch]] - JSCouch examples done with Riak
* [[riak_tokyo_cabinet|http://github.com/jebu/riak_tokyo_cabinet]] - A Tokyo Cabinet back end for Riak
* [[Logstash Riak Output|http://logstash.net/docs/1.1.9/outputs/riak]] - An output plugin for Logstash


## Sample Applications

This a collection of sample applications built on Riak and Riak Core.

### Riak 

* [[yakriak|http://github.com/seancribbs/yakriak]] - Riak-powered Ajax-polling chatroom
* [[riaktant|https://github.com/basho/riaktant]] - A full- blown node.js app that stores and makes syslog messages searchable in Riak Search
* [[selusuh|https://github.com/OJ/selusuh]] - Riak application which presents JSON slide decks (thanks, [OJ](http://twitter.com/thecolonial)!)
* [[Rekon|https://github.com/adamhunter/rekon]] - A Riak data browser, built as totally self-contained Riak application
* [[Slideblast|https://github.com/rustyio/SlideBlast]] - share and control slide presentation for the web
* [[riak_php_app|http://github.com/schofield/riak_php_app]] - a small PHP app that shows some basic usage of the Riak PHP library
* [[riak-url-shortener|http://github.com/seancribbs/riak-url-shortener]] - a small Ruby app (with Sinatra) that creates short URLs and stores them in Riak
* [[wriaki|https://github.com/basho/wriaki]] - a wiki app backed by Riak
* [[riagi|https://github.com/basho/riagi]] - A simple imgur.com clone built using Riak, Django, and Riak Search

### Riak Core

_Riak Core (or "riak_core" as it's written in the code) is the distributed systems framework that underpins Riak. For more general information on Riak Core, [start with this blog post](http://blog.basho.com/2011/04/12/Where-To-Start-With-Riak-Core/)._

* [[riak_id|https://github.com/seancribbs/riak_id]] - A clone of Twitter's Snowflake, built on riak_core
* [[basho_banjo|https://github.com/rustyio/BashoBanjo]] - An application that uses Riak Core to play distributed music
* [[riak_zab|https://github.com/jtuple/riak_zab]] - An implementation of the Zookeeper
protocol on top of Riak Core
* [[try-try-try|https://github.com/rzezeski/try-try-try]] - Ryan Zezeski’s working blog that takes an in depth look at various aspects of riak_core (and walks you through building an application called "RTS") 
