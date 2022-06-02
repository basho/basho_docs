---
title: "Client Libraries"
description: ""
project: "riak_kv"
project_version: "2.2.1"
menu:
  riak_kv-2.2.1:
    name: "Client Libraries"
    identifier: "developing_client_libraries"
    weight: 106
    parent: "developing"
toc: true
aliases:
  - /riak/2.2.1/dev/using/libraries
  - /riak/kv/2.2.1/dev/using/libraries
---

## Basho-Supported Libraries

Basho officially supports a number of open-source client libraries for a
variety of programming languages and environments.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
Java | [riak-java-client](https://github.com/basho/riak-java-client) | [javadoc](http://basho.github.com/riak-java-client), [wiki](https://github.com/basho/riak-java-client/wiki) | [Maven Central](http://search.maven.org/?#search%7Cgav%7C1%7Cg%3A%22com.basho.riak%22%20AND%20a%3A%22riak-client%22) |
Ruby | [riak-ruby-client](https://github.com/basho/riak-ruby-client) | [GitHub Pages](http://basho.github.io/riak-ruby-client/) | [RubyGems](https://rubygems.org/gems/riak-client)
Python | [riak-python-client](https://github.com/basho/riak-python-client) | [sphinx](http://basho.github.com/riak-python-client) | [PyPI](http://pypi.python.org/pypi?:action=display&name=riak#downloads)
C# | [riak-dotnet-client](https://github.com/basho/riak-dotnet-client) | [api docs](http://basho.github.io/riak-dotnet-client-api/), [wiki](https://github.com/basho/riak-dotnet-client/wiki) | [NuGet package](http://www.nuget.org/List/Packages/RiakClient), [GitHub Releases](https://github.com/basho/riak-dotnet-client/releases)
Node.js | [riak-nodejs-client](https://github.com/basho/riak-nodejs-client) | [api docs](http://basho.github.com/riak-nodejs-client/), [wiki](https://github.com/basho/riak-nodejs-client/wiki) | [NPM](https://www.npmjs.com/package/basho-riak-client), [GitHub Releases](https://github.com/basho/riak-nodejs-client/releases)
PHP | [riak-php-client](https://github.com/basho/riak-php-client) | [apigen](http://basho.github.io/riak-php-client)
Erlang | [riak-erlang-client (riakc)](https://github.com/basho/riak-erlang-client) | [edoc](http://basho.github.com/riak-erlang-client/) | [GitHub](https://github.com/basho/riak-erlang-client)
Go | [riak-go-client](https://github.com/basho/riak-go-client) | [GoDoc](https://godoc.org/github.com/basho/riak-go-client) | [GitHub](https://github.com/basho/riak-go-client)

**Note**: All official clients use the integrated issue tracker on
GitHub for bug reporting.

In addition to the official clients, Basho provides some unofficial
client libraries, listed below. There are also many client libraries and
related [community projects]({{<baseurl>}}community/projects/).


## Community Libraries

The Riak Community is developing at a break-neck pace, and the number of
community-contributed libraries and drivers is growing right along side
it. Here is a list of projects that may suit your programming needs or
curiosities. If you know of something that needs to be added or are
developing something that you wish to see added to this list, please
fork the [Riak Docs repo on GitHub](https://github.com/basho/basho_docs)
and send us a pull request.

{{% note title="Note on community-produced libraries" %}}
All of these projects and libraries are at various stages of completeness and
may not suit your application's needs based on their level of maturity and
activity.
{{% /note %}}

### Client Libraries and Frameworks

#### C/C++

* [riak-cpp](https://github.com/ajtack/riak-cpp) --- A C++ Riak client
  library for use with C++11 compilers
* [Riak C Driver](https://github.com/fenek/riak-c-driver) --- A library
  to communicate with Riak using cURL and Protocol Buffers
* [Riack](https://github.com/trifork/riack) --- A simple C client
  library
* [Riack++](https://github.com/TriKaspar/riack_cpp) --- A C++ wrapper
  around riack

#### Clojure

* [knockbox](https://github.com/reiddraper/knockbox) --- An eventual
  consistency toolbox for Clojure
* [Welle](http://clojureriak.info) --- An expressive Clojure client with
  batteries included
* [clj-riak](http://github.com/mmcgrana/clj-riak) --- Clojure bindings
  to the Riak Protocol Buffers API
* [sumo](https://github.com/reiddraper/sumo) --- A Protocol
  Buffer-specific client for Riak with KV, 2i, and MapReduce support
* [kria](https://github.com/bluemont/kria) --- Riak 2.0 Asynchronous
  (NIO.2) Clojure client. Callback driven, low level, Protocol Buffer
  API, Java 7.

#### ColdFusion

* [Riak-Cache-Extension](https://github.com/getrailo/Riak-Cache-Extension)
  --- A Riak-backed cache extension for Railo/ColdFusion

#### Common Lisp

* [cl-riak (1)](https://github.com/whee/cl-riak)
* [cl-riak (2)](https://github.com/eriknomitch/cl-riak)

#### Dart

* [riak-dart](https://github.com/agilord/riak_dart_client) --- HTTP
  client for Riak written in Dart

#### Django (Python)

* [django-riak-sessions](https://github.com/flashingpumpkin/django-riak-sessions)
  --- Riak-based Session Backend for Django
* [Django Riak Engine](https://github.com/oubiwann/django-riak-engine)
  --- A Riak backend for Django

#### Erlang

* [Uriak Pool](https://github.com/unisontech/uriak_pool) --- Erlang
  connection pool library from the team at
  [Unison](http://www.unison.com)
* [Riak PBC Pool](https://github.com/snoopaloop/Riak-PBC-Pool) --- Riak
  Protocol Buffer Client pool application
* [Pooly](https://github.com/aberman/pooly) --- Riak Process Pool
* [riakpool](https://github.com/dweldon/riakpool) --- Application for
  maintaining a dynamic pool of Protocol Buffer client connections to a
  Riak database
* [pooler](https://github.com/seth/pooler) --- An OTP Process Pool
  Application
* [krc](https://github.com/klarna/krc) --- A simple wrapper around the
  official Riak client for Erlang
* [riakc_pool](https://github.com/brb/riakc_pool) --- A really simple
  Riak client process pool based on poolboy

#### Go

* [riaken](https://github.com/riaken) --- A fast and extendable Riak
  Protocol Buffer Client
* [goriakpbc](https://github.com/tpjg/goriakpbc) --- A Golang Riak
  client inspired by the Ruby riak-client from Basho and riakpbc from mrb
* [riakpbc](https://github.com/mrb/riakpbc) --- A Riak Protocol Buffer
  client in Go
* [goriak](https://github.com/zegl/goriak) --- Go language driver for Riak KV

#### Grails

* [Grails ORM for Riak](http://www.grails.org/plugin/riak)

#### Griffon

* [Riak Plugin for
  Griffon](http://docs.codehaus.org/display/GRIFFON/Riak+Plugin)

#### Groovy

* [spring-riak](https://github.com/jbrisbin/spring-riak) --- Riak
  support from Groovy and/or Java

#### Haskell

* [Riak Haskell Client](https://github.com/markhibberd/riak-haskell-client)
  --- A fast Haskell client library from the team at MailRank.

#### Java

* [Riak-Java-PB-Client](http://github.com/krestenkrab/riak-java-pb-client)
  --- Java Client Library for Riak based on the Protocol Buffers API
* [Asynchronous Riak Java Client](https://github.com/jbrisbin/riak-async-java-client)
  --- Asynchronous, NIO-based Protocol Buffers client for Riak
* [Riak Module for the Play
  Framework](http://www.playframework.org/modules/riak-head/home)

#### Lisp-flavored Erlang

* [Gutenberg](https://github.com/dysinger/gutenberg/) --- Riak MapReduce
  examples written in LFE

#### Node.js

* [zukai](https://github.com/natural/zukai) --- Riak ODM for Node.js
  from Troy Melhase
* [riak-pb](https://github.com/CrowdProcess/riak-pb) --- Riak Protocol
  Buffers client for Node.js from the team at
  [CrowdProcess](http://crowdprocess.com)
* [node_riak](https://github.com/mranney/node_riak) --- Voxer's
  production Node.js client for Riak.
* [riakpbc](https://github.com/nlf/riakpbc) --- A simple Riak Protocol
  Buffer client library for Node.js
* [nodiak](https://npmjs.org/package/nodiak) --- Supports bulk
  get/save/delete, sibling auto-resolution, MapReduce chaining, Search,
  and 2i's
* [resourceful-riak](https://github.com/admazely/resourceful-riak) --- A
  Riak engine to the
  [resourceful](https://github.com/flatiron/resourceful/) model
  framework from [flatiron](https://github.com/flatiron/)
* [Connect-Riak](https://github.com/frank06/connect-riak) --- Riak
  session store for Connect backed by [Riak-js](http://riakjs.org/)
* [Riak-js](http://riakjs.com) --- Node.js client for Riak with support
  for HTTP and Protocol Buffers
* [Riakjs-model](https://github.com/dandean/riakjs-model) --- a model
  abstraction around riak-js
* [Node-Riak](http://github.com/orlandov/node-riak) --- A wrapper around
  Node's HTTP facilities for communicating with Riak
* [riak-dc](https://github.com/janearc/riak-dc) --- A very thin, very small
  http-based interface to Riak using promises intended to be used for small
  tools like command-line applications; aims to have the "most-synchronous-
  like" interface.
* [Nori](https://github.com/sgonyea/nori) --- Experimental Riak HTTP
  library for Node.js modeled after Ripple
* [OrionNodeRiak](http://github.com/mauritslamers/OrionNodeRiak) ---
  Node-based server and database-frontend for Sproutcore
* [Chinood](https://npmjs.org/package/chinood) --- Object data mapper
  for Riak built on Nodiak
* [SimpleRiak](https://npmjs.org/package/simpleriak) --- A very simple
  Riak HTTP client

#### OCaml

* [Riak OCaml Client](http://metadave.github.com/riak-ocaml-client/) ---
  Riak OCaml client
* [OCaml Riakc](https://github.com/orbitz/ocaml-riakc) --- A Protocol
  Buffers client for Riak

#### Perl

* [Net::Riak](http://search.cpan.org/~franckc/Net-Riak/) --- A Perl
  interface to Riak
* [AnyEvent-Riak adapter](http://github.com/franckcuny/anyevent-riak)
  --- Non-blocking Riak adapter using anyevent
* [riak-tiny](https://github.com/tempire/riak-tiny) --- Perl interface
  to Riak without Moose
* [Riak::Light](https://metacpan.org/module/Riak::Light) --- Fast and
  lightweight Perl client for Riak (PBC only)

#### PHP

* [riak-client](https://github.com/php-riak/riak-client) --- A Riak
  2.0-compliant PHP client with support for Protocol Buffers by [Fabio
  Silva](https://github.com/FabioBatSilva)
* [Ripple-PHP](https://github.com/KevBurnsJr/ripple-php) --- A port of
  Ripple to PHP
* [riiak](https://bitbucket.org/intel352/riiak) --- A Riak PHP client
  library for the [Yii Framework](http://www.yiiframework.com/)
* [riak-php](https://github.com/marksteele/riak-php) --- A Riak PHP
  client with support for Protocol Buffers
* [RiakBundle](https://github.com/remialvado/RiakBundle) ---
  [Symfony](http://symfony.com) Bundle designed to ease interaction
  with Riak
* [php_riak](https://github.com/TriKaspar/php_riak) --- A PHP extension
  written in C, Both Riak client and PHP session module

#### Python

* [Aioriak](https://github.com/rambler-digital-solutions/aioriak) 
  --- Asyncio PBC Riak 2.0+ client library. (Based on official Basho 
  python client)
* [Riakasaurus](https://github.com/calston/riakasaurus) --- A Riak
  client library for Twisted (based on txriak)
* [RiakKit](http://shuhaowu.com/riakkit) --- A small Python ORM that
  sits on top of riak-python-client, similar to mongokit and couchdbkit
* [riakalchemy](https://github.com/Linux2Go/riakalchemy) --- Object
  mapper for Riak written in Python
* [riak_crdt](https://github.com/ericmoritz/riak_crdt) --- A CRDT
  (Conflict-Free Replicated Data Type) loader for Riak using the [CRDT
  API](https://github.com/ericmoritz/crdt)
* [txriak](https://launchpad.net/txriak) --- A Twisted module for
  communicating with Riak via the HTTP interface
* [txriakidx](https://github.com/williamsjj/txriakidx) --- Riak client
  for Twisted Python that implements transparent indexes

#### Racket

* [riak.rkt](https://github.com/shofetim/riak.rkt) --- Racket API to
  Riak
* [Racket Riak](https://github.com/dkvasnicka/racket-riak) --- Racket
  1.3.x API to Riak

#### Ruby

* [Risky](https://github.com/aphyr/risky) --- A lightweight Ruby ORM for
  Riak
* [riak_sessions](http://github.com/igorgue/riak_sessions) ---
  Riak-backed session storage for Rack
* [Riaktor](http://github.com/benmyles/riaktor) --- Ruby client and
  object mapper for Riak
* [dm-riak-adapter](http://github.com/mikeric/dm-riak-adapter) ---
  DataMapper adapter for Riak
* [Riak PB Client](https://github.com/sgonyea/riak-pbclient) --- Riak
  Protocol Buffer Client in Ruby
* [Devise-Ripple](http://github.com/frank06/devise-ripple) --- An ORM
  strategy to use Devise with Riak
* [ripple-anaf](http://github.com/bkaney/ripple-anaf) --- Accepts nested
  attributes support for Ripple
* [Pabst](https://github.com/sgonyea/pabst) --- Cross-platform Ruby
  extension for Protocol Buffers written in both Objective-C and
  Objective-C++

#### Scala

* [Riakka](http://github.com/timperrett/riakka) --- Scala library for
  talking to Riak
* [Ryu](http://github.com/softprops/ryu) --- A Tornado Whirlwind Kick
  Scala client for the Riak raw HTTP interface
* [Raiku](https://github.com/gideondk/Raiku) --- An Akka IO- and
  Sentinel-driven Riak Scala client

#### Smalltalk

* [Phriak](http://www.squeaksource.com/Phriak/) --- A Riak client for
  Pharo Smalltalk based on Runar Jordahl's EpigentRiakInterface
* [EpigentRiakInterface](http://www.squeaksource.com/EpigentRiakInterface/)
  --- A Pharo Smalltalk interface to Riak. There is also a blog post
  with some additional info about the client
  [here](http://blog.epigent.com/2011/03/riak-interface-for-pharo-smalltalk.html).
