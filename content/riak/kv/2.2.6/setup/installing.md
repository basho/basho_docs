---
title: "Installing Riak KV"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Installing"
    identifier: "installing"
    weight: 101
    parent: "setup_index"
    pre: cog
toc: true
aliases:
  - /riak/2.2.6/ops/building/installing
  - /riak/kv/2.2.6/ops/building/installing
  - /riak/2.2.6/installing/
  - /riak/kv/2.2.6/installing/
---

[install aws]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/amazon-web-services
[install debian & ubuntu]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/debian-ubuntu
[install freebsd]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/freebsd
[install mac osx]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/mac-osx
[install rhel & centos]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/rhel-centos
[install smartos]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/smartos
[install solaris]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/solaris
[install suse]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/suse
[install windows azure]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/windows-azure
[install source index]: {{<baseurl>}}riak/kv/2.2.6/setup/installing/source
[community projects]: {{<baseurl>}}community/projects
[upgrade index]: {{<baseurl>}}riak/kv/2.2.6/setup/upgrading

## Supported Platforms

Riak is supported on numerous popular operating systems and virtualized
environments. The following information will help you to
properly install or upgrade Riak in one of the supported environments:

  * [Amazon Web Services][install aws]
  * [Debian & Ubuntu][install debian & ubuntu]
  * [FreeBSD][install freebsd]
  * [Mac OS X][install mac osx]
  * [RHEL & CentOS][install rhel & centos]
  * [SmartOS][install smartos]
  * [Solaris][install solaris]
  * [SUSE][install suse]
  * [Windows Azure][install windows azure]

## Building from Source

If your platform isn’t listed above, you may be able to build Riak from source. See [Installing Riak from Source][install source index] for instructions.

## Community Projects

Check out [Community Projects][community projects] for installing with tools such as [Chef](https://www.chef.io/chef/), [Ansible](http://www.ansible.com/), or [Cloudsoft](http://www.cloudsoftcorp.com/).

## Upgrading

For information on upgrading an existing cluster see [Upgrading Riak KV][upgrade index].
