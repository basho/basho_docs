---
title: Taste of Riak Prerequisites
project: riak
document: guide
toc: false
audience: beginner
keywords: [developers, client]
---

Welcome!  In just a few minutes we will show you how easy it is to begin developing against Riak.  To keep these examples simple and fast we will be using a single Riak node, but you are welcome to use a full cluster if you'd like to.  
(For production deployments, Basho [recommends a minimum of five nodes](http://basho.com/why-your-riak-cluster-should-have-at-least-five-nodes/).) 

###Installing Riak
Installing Riak is really easy.  Download the package for your system from the [[Downloads]] page, and install it. — Go ahead, we’ll wait for you here.

###Start Riak
Now that we have Riak installed, we need to start it.  From your command line, this is done with:

```bash
riak start
```

Your single node instance of Riak is now installed and running.  

###Choose Your Programming Language

Basho officially supports a number of open-source client libraries for various programming languages and environments.  Please select the language you'd like to proceed with.

-	[[Erlang|Taste of Riak - Erlang Flavor]]
-	[[Java|Taste of Riak - Java Flavor]]
-	[[PHP|Taste of Riak - PHP Flavor]]
-	[[Python|Taste of Riak - Python Flavor]]
-	[[Ruby|Taste of Riak - Ruby Flavor]]




