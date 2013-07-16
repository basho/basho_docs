---
title: Client Libraries
project: riak
version: 0.10.0+
document: reference
toc: true
index: true
audience: intermediate
keywords: [client, drivers]
moved: {
  '1.4.0-': '/references/Client-Libraries'
}
---

## Basho Supported Libraries

Basho officially supports a number of open-source client libraries for various programming languages and environments.

| Language | Source                                                   | Documentation | Download      |
|----------|----------------------------------------------------------|---------------|---------------|
| Erlang   | [riak-erlang-client (riakc)](https://github.com/basho/riak-erlang-client)<br>[riak-erlang-http-client (rhc)](https://github.com/basho/riak-erlang-http-client) | [edoc](http://basho.github.com/riak-erlang-client/)          |               |
| Java     | [riak-java-client](https://github.com/basho/riak-java-client)                                         | [javadoc](http://basho.github.com/riak-java-client), [wiki](https://github.com/basho/riak-java-client/wiki) | [Maven Central](http://search.maven.org/?#search%7Cgav%7C1%7Cg%3A%22com.basho.riak%22%20AND%20a%3A%22riak-client%22) |
| PHP      | [riak-php-client](https://github.com/basho/riak-php-client)                                          | [doxygen](http://basho.github.com/riak-php-client)       |               |
| Python   | [riak-python-client](https://github.com/basho/riak-python-client)                                       | [sphinx](http://basho.github.com/riak-python-client)        | [PyPI](http://pypi.python.org/pypi?:action=display&name=riak#downloads)          |
| Ruby     | [riak-ruby-client](https://github.com/basho/riak-ruby-client)                                         | [rdoc](http://rdoc.info/gems/riak-client/frames), [wiki](https://github.com/basho/riak-ruby-client/wiki)    | [RubyGems](https://rubygems.org/gems/riak-client)      |


All official clients use the integrated issue tracker on Github for bug reporting.

In addition to the official clients, Basho provides some unofficial
client libraries, listed below. There are also many client libraries
and related projects [[community projects]].

| Language            | Source                 |
|---------------------|------------------------|
| C/C++               | [riak-cxx-client](https://github.com/basho/riak-cxx-client)        |
| Javascript (jQuery) | [riak-javascript-client](https://github.com/basho/riak-javascript-client) |


*** Feature Matrix
   Below is a series of tables that compares the functionality of our
   official client libraries with the features exposed by Riak's API,
   and also compares features that are desirable in well-developed
   clients. We have developed this matrix in an effort to ensure
   feature-parity across the different clients.

   Legend:
   - =✓= - has feature
   - =✗= - lacks feature
   - =text= - partially supports feature
   - (blank) - unknown

### HTTP

| Bucket Operations          | Erlang (rhc)           | Java | PHP                    | Python  | Ruby |
|----------------------------|------------------------|------|------------------------|---------|------|
| List buckets               | ✓                      | ✓    | ✓                      | ✓       | ✓    |
| List keys                  | ✓                      | ✓    | ✓                      | ✓       | ✓    |
| Get Bucket Properties      | partial                | ✓    | ✓                      | ✓       | ✓    |
| Set Bucket Properties      | partial                | ✓    | ✓                      | ✓       | ✓    |

| Object/Key Operations      | Erlang (rhc)           | Java | PHP                    | Python  | Ruby |
|----------------------------|------------------------|------|------------------------|---------|------|
| Fetch Object (get)         | ✓                      | ✓    | ✓                      | ✓       | ✓    |
| Fetch w/quorums            | no PR                  | ✓    | no PR                  | ✓       | ✓    |
| Store Object (put)         | ✓                      | ✓    | ✓                      | ✓       | ✓    |
| Store w/quorums            | no PW                  | ✓    | no PW                  | ✓       | ✓    |
| Delete Object              | ✓                      | ✓    | ✓                      | ✓       | ✓    |

| Query Operations           | Erlang (rhc)           | Java | PHP                    | Python  | Ruby |
|----------------------------|------------------------|------|------------------------|---------|------|
| Link Walking               | ✗                      | ✓    | ✗                      | ✗       | ✓    |
| MapReduce                  | ✓                      | ✓    | ✓                      | ✓       | ✓    |
| Secondary Indexes          | ✗                      | ✓    | ✓                      | ✓       | ✓    |
| Search                     | emulated via MapReduce | ✓    | emulated via MapReduce | ✓       | ✓    |

| Server Operations          | Erlang (rhc)           | Java | PHP                    | Python  | Ruby |
|----------------------------|------------------------|------|------------------------|---------|------|
| Ping                       | ✓                      | ✓    | ✓                      | ✓       | ✓    |
| Status                     | partial                | ✓    | ✗                      | partial | ✓    |
| List Resources             | ✗                      | ✗    | ✗                      | partial | ✓    |

### Protocol Buffers

*Note: The PHP client does not support Protocol Buffers and so is
excluded from this matrix.*

| Bucket Operations                    | Erlang (riakc) | Java | Python  | Ruby |
|--------------------------------------|----------------|------|---------|------|
| List buckets                         | ✓              | ✓    | ✓       | ✓    |
| List keys                            | ✓              | ✓    | ✓       | ✓    |
| Get Bucket Properties                | ✓              | ✓    | ✓       | ✓    |
| Set Bucket Properties                | ✓              | ✓    | ✓       | ✓    |

| Object/Key Operations                | Erlang (riakc) | Java | Python  | Ruby |
|--------------------------------------|----------------|------|---------|------|
| Fetch Object (get)                   | ✓              | ✓    | ✓       | ✓    |
| Fetch w/quorums                      | ✓              | ✓    | ✓       | ✓    |
| Store Object (put)                   | ✓              | ✓    | ✓       | ✓    |
| Store w/quorums                      | ✓              | ✓    | ✓       | ✓    |
| Delete Object                        | ✓              | ✓    | ✓       | ✓    |

| Query Operations                     | Erlang (riakc) | Java | Python  | Ruby |
|--------------------------------------|----------------|------|---------|------|
| MapReduce                            | ✓              | ✓    | ✓       | ✓    |
| Secondary Indexes (emulated, native) | ✓✗             | ✓✗   | ✓✓      | ✓✓   |
| Search (emulated, native)            | ✓✗             | ✓✗   | ✓✓      | ✓✓   |

| Server Operations                    | Erlang (riakc) | Java | Python  | Ruby |
|--------------------------------------|----------------|------|---------|------|
| Ping                                 | ✓              | ✓    | ✓       | ✓    |
| Server Info                          | ✓              | ✗    | partial | ✓    |
| Get Client ID                        | ✓              | ✓    | ✓       | ✓    |
| Set Client ID                        | ✓              | ✓    | ✓       | ✓    |

### Additional features

| Protocols                              | Erlang                    | Java | PHP     | Python  | Ruby          |
|----------------------------------------|---------------------------|------|---------|---------|---------------|
| Cluster connections/pools              | ✗                         | ✓    | ✗       | partial | ✓             |
| Retry failures (on other nodes)        | ✗                         | ✓    | ✗       | ✗       | ✓ ✓           |
| Failure-sensitive node selection       | ✗                         | ✗    | ✗       | ✗       | ✓             |
| Automatic protocol selection           | ✗                         | ✗    | ✗       | ✗       | ✓             |

| Media-Type Handling                    | Erlang                    | Java | PHP     | Python  | Ruby          |
|----------------------------------------|---------------------------|------|---------|---------|---------------|
| Use arbitrary media types              | ✓                         | ✓    | ✓       | ✓       | ✓             |
| JSON (de-)serialization                | ✗                         | ✓    | ✓       | ✓       | ✓             |
| Other included (de-)serializers        | Erlang Binary Term Format | ✗    | ✗       | ✗       | YAML, Marshal |
| Custom (de-)serializers                | ✗                         | ✓    | ✗       | ✓       | ✓             |

| Eventual Consistency                   | Erlang                    | Java | PHP     | Python  | Ruby          |
|----------------------------------------|---------------------------|------|---------|---------|---------------|
| Exposes siblings                       | ✓                         | ✓    | ✓       | ✓       | ✓             |
| Sibling resolution policies/strategies | ✗                         | ✓    | ✗       | ✗       | ✓             |
| Mutators (encapsulating change ops)    | ✗                         | ✓    | ✗       | ✗       | ✗             |

| Domain Types/Object Mapping            | Erlang                    | Java | PHP     | Python* | Ruby*         |
|----------------------------------------|---------------------------|------|---------|---------|---------------|
| Abstract domain types w/reification    | ✗                         | ✓    | partial | ✓       | ✓             |
| Embedded/nested domain types           | ✗                         | ✓    |         | ✓       | ✓             |
| Domain-level sibling resolution        | ✗                         | ✓    | ✗       | ✗       | ✓             |
| Secondary index integration            | ✗                         | ✓    | partial | ✓       | ✓             |
| Search integration                     | ✗                         | ✓    | ✗       | ✓       | ✗             |


Various [[community projects]] provide support for domain types and
object mapping in Python and Ruby. The values in the table above
represent the aggregate features of the projects listed below:

- *Ruby*: [ripple](https://github.com/basho/ripple), [risky](https://github.com/aphyr/risky), and [curator](https://github.com/braintree/curator).
- *Python*: [riakkit](https://github.com/shuhaowu/riakkit), [riakalchemy](https://github.com/Linux2Go/riakalchemy), and [django-riak-engine](https://github.com/oubiwann/django-riak-engine).

## Community Libraries

The Riak Community is developing at a break-neck pace, and the number of community-contributed libraries and drivers is growing right along side it. Here is a list of projects that may suit your programming needs or curiosities. If you know of something that needs to be added or are developing something that you wish to see added to this list, please fork the [Riak Docs repo on GitHub](https://github.com/basho/basho_docs) and send us a pull request.

<div class="info">
All of these projects and libraries are at various stages of completeness and may not suit your application's needs based on their level of maturity and activity.
</div>

### Client Libraries and Frameworks

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

* [Uriak Pool](https://github.com/unisontech/uriak_pool) - Erlang connection pool library from the team at [[Unison|http://www.unison.com]]
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

* [zukai](https://github.com/natural/zukai) - Riak ODM for Node.js from Troy Melhase 
* [riak-pb](https://github.com/CrowdProcess/riak-pb) - Riak Protocol Buffers Client for Node.js from the team at [CrowdProcess](http://crowdprocess.com)
* [[node_riak|https://github.com/mranney/node_riak]] - Voxer's production Node.js client for Riak. 
* [[nodiak|https://npmjs.org/package/nodiak]] - Supports bulk get/save/delete, sibling auto-resolution, MapReduce chaining, Search, and 2i's.
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
* [OCaml Riakc](https://github.com/orbitz/ocaml-riakc) - ocaml-riakc

*Perl*

* [[Net::Riak|http://search.cpan.org/~franckc/Net-Riak/]] - A Perl interface to Riak
* [[AnyEvent-Riak adapter|http://github.com/franckcuny/anyevent-riak]] - Non-blocking Riak adapter using anyevent
* [[riak-tiny|https://github.com/tempire/riak-tiny]] - Perl interface to Riak without Moose
* [[Riak::Light|https://metacpan.org/module/Riak::Light]] - Fast and lightweight Perl client for Riak (PBC only)

*PHP*

* [[Ripple-PHP|https://github.com/KevBurnsJr/ripple-php]] - A port of Ripple to PHP
* [[riiak|https://bitbucket.org/intel352/riiak]] - A Riak PHP client library for the [[Yii Framework|http://www.yiiframework.com/]]
* [[riak-php|https://github.com/marksteele/riak-php]] - A Riak PHP with support for Protocol Buffers
* [[RiakBundle|https://github.com/remialvado/RiakBundle]] - [[Symfony|http://symfony.com]] Bundle designed to ease interaction with Riak
* [[php_riak|https://github.com/TriKaspar/php_riak]] - A PHP Extension written in C, Both Riak client and PHP session module

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

* [[riak.rkt|https://github.com/shofetim/riak.rkt]] - Racket API to Riak
* [[Racket Riak|https://github.com/dkvasnicka/racket-riak]] - Racket 1.3.x API to Riak

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
