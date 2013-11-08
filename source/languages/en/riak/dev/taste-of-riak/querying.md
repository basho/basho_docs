---
title: "Taste of Riak: Querying"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search]
---

Now that we've had a taste of the CRUD interface for Riak, let's look into a few ways to lay out and query our data - Secondary Indexes and Key/Value.

###Configuration Changes
Before we experiment with these methods, we will have to change our Riak instance's configuration a little bit.  

To do this we will have to find Riak's `app.config` file, which can usually be found at `/etc/riak/app.config`.  
If you are running SmartOS it will be at `/opt/local/etc/riak/app.config`, and if you used HomeBrew to install Riak on OSX it will be at `/usr/local/Cellar/riak/**VERSION**/libexec/etc/app.config`.  

Open the `app.config` file in your favorite text editor.
  

####Using the LevelDB Backend for 2i

Search for the string `{riak_kv`, this will bring you to the configuration section for Riak's Key/Value component.
Change the line `{storage_backend, riak_kv_bitcask_backend},` to `{storage_backend, riak_kv_eleveldb_backend},`.  
This will make Riak use LevelDB for it's backend instead of Bitcask.  LevelDB supports Secondary Indexes (2i), which we will be using for our examples.


Save your app.config, and restart riak by executing `riak restart` from the command line.  
Run `riak ping` after the restart to ensure that the node is up and running.

_**Note: If you are running a cluster instead of a single node, you will have to make these changes on each node.**_

###Choose Your Programming Language
Please select the language you'd like to proceed with.

<ul class="planguages">
<li><a href="/dev/taste-of-riak/querying-java/"><img src="/images/plangs/java.jpg" alt="Java"></a></li>
<li><a href="/dev/taste-of-riak/querying-erlang/"><img src="/images/plangs/erlang.jpg" alt="Erlang"></a></li>
<li><a href="/dev/taste-of-riak/querying-ruby/"><img src="/images/plangs/ruby.jpg" alt="Ruby"></a></li>
<li><a href="/dev/taste-of-riak/querying-php/"><img src="/images/plangs/php.png" alt="PHP"></a></li>
<li><a href="/dev/taste-of-riak/querying-python/"><img src="/images/plangs/python.png" alt="Python"></a></li>
</ul>

