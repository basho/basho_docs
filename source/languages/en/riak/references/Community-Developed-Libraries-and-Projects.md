---
title: Community Developed Libraries and Projects
project: riak
version: 0.10.0+
document: reference
toc: true
index: true
audience: intermediate
keywords: [client, drivers]
---

The Riak Community is developing at a break-neck pace, and the number of community-contributed libraries and drivers is growing right along side it. Here is a list of projects that may suit your programming needs or curiosities. If you know of something that needs to be added or are developing something that you wish to see added to this list, please fork the [Riak Docs repo on GitHub](https://github.com/basho/basho_docs) and send us a pull request.

<div class="info">
All of these projects and libraries are at various stages of completeness and may not suit your application's needs based on their level of maturity and activity.
</div>

## Client Libraries and Frameworks

*C/C++*

* [[riak-cpp|https://github.com/ajtack/riak-cpp]] - A C++ Riak client library for use with C++11 compilers
* [[Riak C Driver|https://github.com/fenek/riak-c-driver]] - A library to communicate with Riak using cURL and Protocol Buffers
* [[Riack|https://github.com/trifork/riack]] - A simple C client library
* [[Riack++|https://github.com/TriKaspar/riack_cpp]] - A C++ wrapper around riack

*Clojure*

* [[knockbox|https://github.com/reiddraper/knockbox]] - An eventual-consistency toolbox for Clojure 
* [[Welle|http://clojureriak.info]] - An expressive Clojure client with batteries included
* [[clj-riak|http://github.com/mmcgrana/clj-riak]] - Clojure bindings to the Riak Protocol Buffers API
* [[sumo|https://github.com/reiddraper/sumo]] - A Protocol Buffer specific client for Riak with K/V, 2i, and MapReduce support

*ColdFusion*

* [[Riak-Cache-Extension|https://github.com/getrailo/Riak-Cache-Extension]] - A Riak-backed cache extension for Railo/ColdFusion

*Common Lisp*

* [[cl-riak (1)|https://github.com/whee/cl-riak]] 
* [[cl-riak (2)|https://github.com/eriknomitch/cl-riak]]

*Dart*

* [[riak-dart|http://code.google.com/p/riak-dart/]] - HTTP client for Riak written in Dart. 

*Django*

* [[django-riak-sessions|https://github.com/flashingpumpkin/django-riak-sessions]] - Riak-based Session Backend for Django 
* [[Django Riak Engine|https://github.com/oubiwann/django-riak-engine]] - A Riak backend for Django

*Go* 

* [[goriakpbc|https://github.com/tpjg/goriakpbc]] - A golang riak client inspired by the Ruby riak-client from Basho and riakpbc from mrb
* [[riakpbc|https://github.com/mrb/riakpbc]] - A Riak Protocol Buffer Client in Go
* [[Shoebox|https://github.com/mrb/shoebox]] - A proof of concept Go project that uses [[riakpbc|https://github.com/mrb/riakpbc]]
* [[riak.go|http://github.com/c141charlie/riak.go]] - A Riak Client for Go

*Grails*

* [[Grails ORM for Riak|http://www.grails.org/plugin/riak]]

*Griffon*

* [[Riak Plugin for Griffon|http://docs.codehaus.org/display/GRIFFON/Riak+Plugin]]

*Groovy*

* [[spring-riak|https://github.com/jbrisbin/spring-riak]] - Riak support from Groovy and/or Java

*Erlang*

* [[Riak PBC Pool|https://github.com/snoopaloop/Riak-PBC-Pool]] - Riak Protocol Buffer Client pool application
* [[Pooly|https://github.com/aberman/pooly]] - Riak Process Pool 
* [[riakpool|https://github.com/dweldon/riakpool]] - Application for maintaining a dynamic pool of Protocol Buffer client connections to a Riak database
* [[pooler|https://github.com/seth/pooler]] - An OTP Process Pool Application
* [[krc|https://github.com/klarna/krc]] - A simple wrapper around the official Riak client for Erlang

*Haskell*

* [[Riak Haskell Client|https://github.com/bos/riak-haskell-client]] - A fast Haskell client library from the team at MailRank.

*Java*

* [[Riak-Java-PB-Client|http://github.com/krestenkrab/riak-java-pb-client]] - Java Client Library for Riak based on the Protocol Buffers API
* [[Asynchronous Riak Java Client|https://github.com/jbrisbin/riak-async-java-client]] - Asynchronous, NIO-based Protocol Buffers client for Riak

*Lisp Flavored Erlang*

* [[Gutenberg|https://github.com/dysinger/gutenberg/]] - Riak MapReduce Examples Written in LFE

*.NET*

* CorrugatedIron ([[project page|http://corrugatediron.org/]] | [[source|https://github.com/DistributedNonsense/CorrugatedIron]] | [[Nuget package|http://www.nuget.org/List/Packages/CorrugatedIron]])
* [[Hebo|http://github.com/bubbafat/hebo]] - An experimental Riak client
* [[Data.RiakClient|http://github.com/garethstokes/Data.RiakClient]] - A Riak client with Protocol Buffer support

*Node.js*

* [[node_riak|https://github.com/mranney/node_riak]] - Voxer's production Node.js client for Riak. 
* [[nodiak|https://github.com/Coradine/nodiak]] - Supports bulk get/save/delete, sibling auto-resolution, MapReduce chaining, Search, and 2i's.
* [[resourceful-riak|https://github.com/admazely/resourceful-riak]] - A Riak engine to the [[resourceful|https://github.com/flatiron/resourceful/]] model framework from [[flatiron|https://github.com/flatiron/]].
* [[Connect-Riak|https://github.com/frank06/connect-riak]] - Riak Session Store for Connect backed by [[Riak-js|http://riakjs.org/]]
* [[Riak-js|http://riakjs.com]] - Node.js client for Riak with support for HTTP and Protocol Buffers
* [[Riakjs-model|https://github.com/dandean/riakjs-model]] - a model abstraction around riak-js
* [[Node-Riak|http://github.com/orlandov/node-riak]] - A wrapper around Node's HTTP facilities for communicating with Riak
* [[Nori|https://github.com/sgonyea/nori]] - Experimental Riak HTTP Library for Node.js modeled after Ripple
* [[OrionNodeRiak|http://github.com/mauritslamers/OrionNodeRiak]] - Node-based server and database-frontend for Sproutcore
* [[Chinood|https://npmjs.org/package/chinood]] - Object data mapper for Riak built on Nodiak
* [[SimpleRiak|https://npmjs.org/package/simpleriak]] - A very simple Riak HTTP client

*OCaml*

* [[Riak OCaml Client|http://metadave.github.com/riak-ocaml-client/]] - Riak OCaml Client

*Perl*

* [[Net::Riak|http://search.cpan.org/~franckc/Net-Riak/]] - A Perl interface to Riak
* [[AnyEvent-Riak adapter|http://github.com/franckcuny/anyevent-riak]] - Non-blocking Riak adapter using anyevent
* [[riak-tiny|https://github.com/tempire/riak-tiny]] - Perl interface to Riak without Moose

*PHP*

* [[Ripple-PHP|https://github.com/KevBurnsJr/ripple-php]] - A port of Ripple to PHP
* [[riiak|https://bitbucket.org/intel352/riiak]] - A Riak PHP client library for the [[Yii Framework|http://www.yiiframework.com/]]
* [[riak-php|https://github.com/marksteele/riak-php]] - A Riak PHP with support for Protocol Buffers
* [[RiakBundle|https://github.com/remialvado/RiakBundle]] - [[Symfony|http://symfony.com]] Bundle designed to ease interaction with Riak

*Play* 

* [[Riak Module for The Play Framework|http://www.playframework.org/modules/riak-head/home]]

*Python*

* [[Riakasaurus|https://github.com/calston/riakasaurus]] - A Riak client library for Twisted (based on txriak) 
* [[RiakKit|http://shuhaowu.com/riakkit]] - A small Python ORM that sits on top of riak-python-client similar to mongokit and couchdbkit
* [[riakalchemy|https://github.com/Linux2Go/riakalchemy]] - Object mapper for Riak written in Python
* [[riak_crdt|https://github.com/ericmoritz/riak_crdt]] - A CRDT (Conflict-Free Replicated Data Type) loader for Riak using the [[crdt API|https://github.com/ericmoritz/crdt]]
* [[txriak|https://launchpad.net/txriak]]- a Twisted module for communicating with Riak via the HTTP interface
* [[txriakidx|https://github.com/williamsjj/txriakidx]] - Riak client for Twisted Python that implements transparent indexes

*Racket*

* [[Racket-Riak|https://gist.github.com/shofetim/riak.rkt]] - Racket API to Riak

*Ruby*

* [[Shogun|https://github.com/krainboltgreene/shogun]] - A light weight, powerful Ruby web application framework with first class support for Riak.
* [[Risky|https://github.com/aphyr/risky]] - A lightweight Ruby ORM for Riak 
* [[riak_sessions|http://github.com/igorgue/riak_sessions]] - Riak-backed session storage for Rack
* [[Riaktor|http://github.com/benmyles/riaktor]] - Ruby client and object mapper for Riak
* [[dm-riak-adapter|http://github.com/mikeric/dm-riak-adapter]] - DataMapper adapter for Riak
* [[Riak PB Client|https://github.com/sgonyea/riak-pbclient]] - Riak Protocol Buffer Client in Ruby
* [[Devise-Ripple|http://github.com/frank06/devise-ripple]] - An ORM strategy to use Devise with Riak
* [[ripple-anaf|http://github.com/bkaney/ripple-anaf]] - Accepts nested attributes support for Ripple
* [[Pabst|https://github.com/sgonyea/pabst]] - Cross-platform Ruby extension for Protocol Buffers written in both Objective-C and Objective-C++

*Scala*

* [[Riakka|http://github.com/timperrett/riakka]] - Scala library for talking to Riak
* [[Ryu|http://github.com/softprops/ryu]] - A Tornado Whirlwind Kick Scala client for the Riak raw HTTP interface

*Smalltalk*

* [[Phriak|http://www.squeaksource.com/Phriak/]] - a Riak client for Pharo Smalltalk based on Runar Jordan's EpigentRiakInterface
* [[EpigentRiakInterface|http://www.squeaksource.com/EpigentRiakInterface/]] - A Pharo Smalltalk interface to Riak. (There is also a blog post with some additional info about the client [[here|http://blog.epigent.com/2011/03/riak-interface-for-pharo-smalltalk.html]].)

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

