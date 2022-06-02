---
title: "Developing with Riak TS"
description: "Developing with Riak TS"
menu:
  riak_ts-1.3.1:
    name: "Develop"
    identifier: "develop"
    weight: 400
    pre: lambda
project: "riak_ts"
project_version: "1.3.1"
toc: true
aliases:
    - /riakts/1.3.1/developing/developing/
---


[erlang]: {{<baseurl>}}riak/ts/1.3.1/developing/erlang
[go]: {{<baseurl>}}riak/ts/1.3.1/developing/golang
[http]: {{<baseurl>}}riak/ts/1.3.1/developing/http
[java]: {{<baseurl>}}riak/ts/1.3.1/developing/java
[ruby]: {{<baseurl>}}riak/ts/1.3.1/developing/ruby
[python]: {{<baseurl>}}riak/ts/1.3.1/developing/python
[csharp]: {{<baseurl>}}riak/ts/1.3.1/developing/csharp
[nodejs]: {{<baseurl>}}riak/ts/1.3.1/developing/nodejs
[erlang]: {{<baseurl>}}riak/ts/1.3.1/developing/erlang
[php]: {{<baseurl>}}riak/ts/1.3.1/developing/php


You can access Riak TS data over HTTP through the [API][http].

TS also exposes protobufs APIs. We build and support the following clients:

* [C#][csharp]
* [Erlang][erlang]
* [Go][go]
* [Java][java]
* [Node.js][nodejs]
* [PHP][php]
* [Python][python]
* [Ruby][ruby]

You can find more information on each officially supported, open-source client libraries at the links below.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
Java | [riak-java-client](https://github.com/basho/riak-java-client) | [javadoc](http://basho.github.com/riak-java-client), [wiki](https://github.com/basho/riak-java-client/wiki) | [Maven Central](http://search.maven.org/?#search%7Cgav%7C1%7Cg%3A%22com.basho.riak%22%20AND%20a%3A%22riak-client%22) |
Ruby | [riak-ruby-client](https://github.com/basho/riak-ruby-client) | [GitHub Pages](http://basho.github.io/riak-ruby-client/) | [RubyGems](https://rubygems.org/gems/riak-client)
Python | [riak-python-client](https://github.com/basho/riak-python-client) | [sphinx](http://basho.github.com/riak-python-client) | [PyPI](http://pypi.python.org/pypi?:action=display&name=riak#downloads)
C# | [riak-dotnet-client](https://github.com/basho/riak-dotnet-client) | [api docs](http://basho.github.io/riak-dotnet-client-api/), [wiki](https://github.com/basho/riak-dotnet-client/wiki) | [NuGet package](http://www.nuget.org/List/Packages/RiakClient), [GitHub Releases](https://github.com/basho/riak-dotnet-client/releases)
Node.js | [riak-nodejs-client](https://github.com/basho/riak-nodejs-client) | [api docs](http://basho.github.com/riak-nodejs-client/), [wiki](https://github.com/basho/riak-nodejs-client/wiki) | [NPM](https://www.npmjs.com/package/basho-riak-client), [GitHub Releases](https://github.com/basho/riak-nodejs-client/releases)
PHP | [riak-php-client](https://github.com/basho/riak-php-client) | [apigen](http://basho.github.io/riak-php-client)
Erlang | [riak-erlang-client (riakc)](https://github.com/basho/riak-erlang-client) | [edoc](http://basho.github.com/riak-erlang-client/) | [GitHub](https://github.com/basho/riak-erlang-client)
Go | [riak-go-client](https://github.com/basho/riak-go-client) | [GoDoc](https://godoc.org/github.com/basho/riak-go-client) | 
