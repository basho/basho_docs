---
title: "Taste of Riak: Querying"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search]
---

Now that we've had a taste of the CRUD interface for Riak, let's look
into a few ways to lay out and query our data: [[secondary indexes|Using
Secondary Indexes]] and [[key/value operations|The Basics]].

## Configuration Changes

Before we experiment with these methods, we will have to change our Riak
instance's configuration a little bit.

To do this we will have to find Riak's `riak.conf` file, which can
usually be found at `/etc/riak/riak.conf.config`. If you are running
SmartOS it will be at `/opt/local/etc/riak/riak.conf`, and if you used
Homebrew to install Riak on OSX it will be at
`/usr/local/Cellar/riak/**VERSION**/libexec/etc/riak.conf`.

Open the `riak.conf` file in your favorite text editor.

### Using the LevelDB Backend for 2i

Search for the `storage_backend` setting and change it from `bitcask` to
`leveldb` (because only [[LevelDB]] supports secondary indexes, a
feature that we'll use in our examples in this tutorial).

Save your `riak.conf`, and restart riak by executing `riak stop`
followed by `riak start` from the command line. Run `riak ping` after
the restart to ensure that the node is up and running.

**Note**: If you are running a cluster instead of a single node, you
will have to make these changes on each node.

### Choose Your Programming Language

Please select the language with which you'd like to proceed:

<ul class="planguages">
<li><a href="/dev/taste-of-riak/querying-java/"><img src="/images/plangs/java.jpg" alt="Java"></a></li>
<li><a href="/dev/taste-of-riak/querying-erlang/"><img src="/images/plangs/erlang.jpg" alt="Erlang"></a></li>
<li><a href="/dev/taste-of-riak/querying-ruby/"><img src="/images/plangs/ruby.jpg" alt="Ruby"></a></li>
<li><a href="/dev/taste-of-riak/querying-php/"><img src="/images/plangs/php.png" alt="PHP"></a></li>
<li><a href="/dev/taste-of-riak/querying-python/"><img src="/images/plangs/python.png" alt="Python"></a></li>
<li><a href="/dev/taste-of-riak/querying-csharp/"><img src="/images/plangs/csharp.png" alt="CSharp"></a></li>
</ul>

