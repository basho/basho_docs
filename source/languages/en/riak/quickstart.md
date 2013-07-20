---
title: Five Minute Install
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, 2i]
moved: {
  '1.4.0-': '/tutorials/fast-track/Building-a-Development-Environment'
}
---

Let's install Riak and build a [five node](http://basho.com/why-your-riak-cluster-should-have-at-least-five-nodes/) cluster running on your local machine.

## Install Riak

Basho's pre-packaged Riak binaries (found under [[Downloads]]) embed the Erlang runtime. However, this tutorial is based on a source build, so if you do not have Erlang already installed, first [[install erlang|Installing Erlang]]. Building Riak from source requires Erlang R15B01.

### Get the Source

The following links provide platform-specific instructions for downloading and installing Riak from source.

  * [[Debian and Ubuntu|Installing on Debian and Ubuntu#Installing-From-Source]]
  * [[RHEL and CentOS|Installing on RHEL and CentOS#Installing-From-Source]]
  * [[Mac OS X|Installing on Mac OS X#Installing-From-Source]]
  * [[FreeBSD|Installing on FreeBSD#Installing-From-Source]]
  * [[SUSE|Installing on SUSE]]
  * [[Windows Azure|Installing on Windows Azure]]
  * [[AWS Marketplace|Installing on AWS Marketplace]]
  * [[Unlisted Operating System|Installing Riak from Source]]

### Build Riak

So now you have a copy of Riak. Time to build it. Do this by accessing the *riak* directory and running `make all`

```bash
$ cd riak-{{V.V.V}}
$ make all
```

`make all` grabs all the Riak dependencies for you so that you don't have to chase them down. This will take a few moments.

## Start Up Five Nodes

Now that Riak is built, we are going to use [Rebar](https://github.com/basho/rebar), a packaging and build system for Erlang applications, to get four self-contained Riak nodes running on your machine. Tomorrow, when you put Riak into production, Rebar will enable you to ship a pre-built Riak package to your deployment machines. But for now, we will just stick to the four nodes. {{#1.3.0+}}You can set the number of nodes you wish to create via `DEVNODES`{{/1.3.0+}} To start these up, run:

{{#1.3.0+}}

```bash
$ make devrel DEVNODES=5
```
{{/1.3.0+}}
{{#1.3.0-}}

```bash
$ make devrel
```
{{/1.3.0-}}

You have just generated a `dev` directory. Let's go into that directory to check out its contents:

```bash
$ cd dev; ls
```

That should give you the following:

```bash
dev1       dev2       dev3       dev4       dev5
```

Each directory starting with `dev` is a complete package containing a Riak node. We now need to start each node. Let's start with `dev1`

```bash
$ dev1/bin/riak start
```

<div class="note">
<div class="title">ulimit warning</div>

At this point you may receive a warning message to increase the number of open file handles (ulimit).  See [[Open Files Limit]] for platform-specific instructions on doing this.

</div>

Then do the same for `dev2` through `dev5`

```bash
$ dev2/bin/riak start
$ dev3/bin/riak start
$ dev4/bin/riak start
$ dev5/bin/riak start
```

### Check Running Nodes

After you have the nodes up and running, it's time to test them and make sure they are available. You can do this by taking a quick look at your process list. To do this, run:

```bash
$ ps aux | grep beam
```

This should give you details on five running Riak nodes.

## Create the Cluster

The next step is to join these five nodes together to form a cluster. You can do this using the Riak Admin tool. Specifically, what we want to do is join `dev2`, `dev3`, `dev4` and `dev5` to `dev1`:

```bash
$ dev2/bin/riak-admin cluster join dev1@127.0.0.1
$ dev3/bin/riak-admin cluster join dev1@127.0.0.1
$ dev4/bin/riak-admin cluster join dev1@127.0.0.1
$ dev5/bin/riak-admin cluster join dev1@127.0.0.1
```

To make the above joins take effect, you first must review the `plan`.

```bash
$ dev1/bin/riak-admin cluster plan
```

The plan will print out a synopsis of what it plans to do, and how the cluster will look
after this is completed.

```bash
=============================== Staged Changes ================================
Action         Nodes(s)
-------------------------------------------------------------------------------
join           'dev2@127.0.0.1'
join           'dev3@127.0.0.1'
join           'dev4@127.0.0.1'
join           'dev5@127.0.0.1'
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 1 cluster transition

###############################################################################
                         After cluster transition 1/1
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid     100.0%     20.3%    'dev1@127.0.0.1'
valid       0.0%     20.3%    'dev2@127.0.0.1'
valid       0.0%     20.3%    'dev3@127.0.0.1'
valid       0.0%     20.3%    'dev4@127.0.0.1'
valid       0.0%     18.8%    'dev5@127.0.0.1'
-------------------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

Transfers resulting from cluster changes: 51
  12 transfers from 'dev1@127.0.0.1' to 'dev5@127.0.0.1'
  13 transfers from 'dev1@127.0.0.1' to 'dev4@127.0.0.1'
  13 transfers from 'dev1@127.0.0.1' to 'dev3@127.0.0.1'
  13 transfers from 'dev1@127.0.0.1' to 'dev2@127.0.0.1'
```

Finally, you can commit the batch.

```bash
$ dev2/bin/riak-admin cluster commit
```

<div class="info">
<div class="title">About riak-admin</div>

riak-admin is Riak's administrative tool. It's used to do any operational tasks
other than starting and stopping node, e.g. to join and leave a cluster, to back
up data, and to manage general cluster operations. You can read more about
[[riak-admin|riak-admin Command Line]].

</div>

## Test the Cluster

Now we now a have a running five node Riak cluster. Let's make sure it's working correctly. For this we have a couple options. A simple option is to run the member-status command.

```bash
$ dev1/bin/riak-admin member-status
```

This will give us a high-level view of our cluster, and what percentage of a ring each node manages.

```bash
================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      20.3%      --      'dev1@127.0.0.1'
valid      20.3%      --      'dev2@127.0.0.1'
valid      20.3%      --      'dev3@127.0.0.1'
valid      20.3%      --      'dev4@127.0.0.1'
valid      18.8%      --      'dev5@127.0.0.1'
-------------------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

If you want, you can add a file to your Riak cluster and test it's working properly. Let's say, for instance, we wanted to add an image and make sure it was accessible. First, copy an image to your directory if you don't already have one:

```bash
$ cp ~/image/location/image_name.jpg .
```

We can then PUT that image into Riak using a curl command (your port might differ, check your `etc/app.config` file for a proper `http` port):

```
$ curl -XPUT http://127.0.0.1:10018/riak/images/1.jpg \
  -H "Content-type: image/jpeg" \
  --data-binary @image_name.jpg
```

You can then verify that image was in fact stored. To do this, simply copy the URL where we PUT the image and paste it into a browser. Your image should appear.

You should now have a running, five node Riak cluster. Congratulations!

<div class="note"><div class="title">HTTP interface ports</div>The above configuration sets up nodes with HTTP interfaces listening on ports `10018`, `10028`, `10038` and `10048` for dev1, dev2, dev3, dev4, dev5 respectively. The default port for single nodes to listen on is 8098 and users will need to take note of this when trying to use any of the default other-language client settings.</div>
