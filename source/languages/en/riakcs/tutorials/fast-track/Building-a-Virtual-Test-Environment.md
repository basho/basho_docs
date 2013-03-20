---
title: Building a Virtual Testing Environment
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [tutorial, fast-track, installing]
prev: "[[Building a Local Test Environment]]"
up:   "[[The Riak CS Fast Track]]"
next: "[[Testing The Riak CS Installation]]"
---

This option for building an environment uses a Vagrant project powered by Chef to bring up a local Riak CS cluster. Each node can run either `Ubuntu 12.04` or `CentOS 6.3` 32-bit with `1024MB`of RAM by default. If you want to tune the OS or node/memory count, you'll
have to edit the `Vagrantfile` directly.

If you want to build a testing environment with more flexibility in configuration and durability across environment resets there are instructions for [[Building a Local Test Environment]].

## Configuration

### Install Prerequisites

Download and install VirtualBox via the [VirtualBox Downloads](https://www.virtualbox.org/wiki/Downloads)

Download and install Vagrant via the [Vagrant Installer](http://downloads.vagrantup.com/tags/v1.0.7).

**NOTE: It is necessary, at present, to install Vagrant 1.0.7 due to a compatibility issue.**

### Clone the Repository

In order to begin, it is necessary to clone a github repository to your local machine and change directories into the cloned folder.

``` bash
$ git clone https://github.com/hectcastro/vagrant-riak-cs-cluster
$ cd vagrant-riak-cs-cluster
```

### Install Cookbooks

``` bash
$ gem install bundler
$ bundle install
$ bundle exec berks install
```

### Launch Cluster

With the cookbooks installed, it's time to actually launch our virtual environment.  The command below will initiate the vagrant project

``` bash
$ RIAK_CS_CREATE_ADMIN_USER=1 vagrant up
```
If you haven't already downloaded an Ubuntu .box this step will download and create one and, the first time you run it, take a bit of extra time.

### Create Admin User & Provision

Creating the admin user is an optional step, but it's a good test of our new services.Creating a Riak CS user requires two inputs:

1. Name - a URL encoded string. Example: "admin"
2. Email - a unique email address. Example: "admin@admin.com"

We can create the admin user with the following `curl` command:

``` bash
$ curl -H 'Content-Type: application/json'                          \
    -X POST http://localhost:8080/riak-cs/user                      \
    --data '{"email":"admin@admin.com", "name":"admin"}'
$ RIAK_CS_ADMIN_KEY="<ADMIN_KEY>" RIAK_CS_SECRET_KEY="<SECRET_KEY>"  \
    vagrant provision
```

You will see an output that looks something like:

```bash
{"email":"admin@admin.com","display_name":"admin","name":"admin user","key_id":"5N2STDSXNV-US8BWF1TH","key_secret":"RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==","id":"4b823566a2db0b7f50f59ad5e43119054fecf3ea47a5052d3c575ac8f990eda7"}
```

The user's access key and secret key are returned in the `key_id` and `key_secret` fields respectively.  Take note of these keys as they will be required in the testing step.

In this case, those keys are:

    Access key: 5N2STDSXNV-US8BWF1TH
    Secret key: RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==

## Next Steps

Congratulations, you have deployed a virtualized environment of Riak CS.  You are ready to progress to [[Testing the Riak CS Installation]].

### Stopping Your Virtual Environment

When you are done testing, or just want to start again from scratch, you can end the current virtualized environment by typicng

    vagrant destroy

**NOTE:  Executing this command will reset the environment to a clean state removing any/all changes that you have done.**
