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

This option for building an environment uses a Vagrant project powered by Chef
to bring up a local Riak CS cluster. Each node can run either `Ubuntu 12.04`
or `CentOS 6.4` 64-bit with `1536MB`of RAM by default. If you want to tune the
OS or node/memory count, you'll have to edit the `Vagrantfile` directly.

If you want to build a testing environment with more flexibility in
configuration and durability across environment resets there are instructions
for [[Building a Local Test Environment]].

## Configuration

### Install Prerequisites

Download and install VirtualBox via the [VirtualBox Downloads](https://www.virtualbox.org/wiki/Downloads)

Download and install Vagrant via the [Vagrant Installer](http://downloads.vagrantup.com/).

**NOTE: Please make sure to install Vagrant `1.1.0` and above.**

### Install Vagrant plugins

Install the following Vagrant plugins:

```bash
$ vagrant plugin install vagrant-berkshelf
$ vagrant plugin install vagrant-omnibus
```

### Clone the Repository

In order to begin, it is necessary to clone a GitHub repository to your local
machine and change directories into the cloned folder.

``` bash
$ git clone https://github.com/basho/vagrant-riak-cs-cluster
$ cd vagrant-riak-cs-cluster
```

### Launch Cluster

With VirtualBox and Vagrant installed, it's time to actually launch our
virtual environment. The command below will initiate the Vagrant project:


``` bash
$ RIAK_CS_CREATE_ADMIN_USER=1 vagrant up
```

If you haven't already downloaded the Ubuntu or CentOS Vagrant box, this step
will download it.

### Recording Admin User credentials

In the Chef provisioning output you will see entries that look like:

```
[2013-03-27T11:59:12+00:00] INFO: Riak CS Key: 5N2STDSXNV-US8BWF1TH
[2013-03-27T11:59:12+00:00] INFO: Riak CS Secret: RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==
```

Take note of these keys as they will be required in the testing step.

In this case, those keys are:

    Access key: 5N2STDSXNV-US8BWF1TH
    Secret key: RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==

## Next Steps

Congratulations, you have deployed a virtualized environment of Riak CS.  You
are ready to progress to [[Testing the Riak CS Installation]].

### Stopping Your Virtual Environment

When you are done testing, or just want to start again from scratch, you can end
the current virtualized environment by typing

    vagrant destroy

**NOTE: Executing this command will reset the environment to a clean state removing
any/all changes that you have done.**
