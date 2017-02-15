---
title: "Installing Riak KV"
description: ""
project: "riak_kv"
project_version: "2.0.0"
menu:
  riak_kv-2.0.0:
    name: "Installing"
    identifier: "installing"
    weight: 101
    parent: "setup_index"
    pre: cog
toc: true
aliases:
  - /riak/2.0.0/ops/building/installing
  - /riak/kv/2.0.0/ops/building/installing
  - /riak/2.0.0/installing/
  - /riak/kv/2.0.0/installing/
---

[install aws]: /riak/kv/2.0.0/setup/installing/amazon-web-services
[install debian & ubuntu]: /riak/kv/2.0.0/setup/installing/debian-ubuntu
[install freebsd]: /riak/kv/2.0.0/setup/installing/freebsd
[install mac osx]: /riak/kv/2.0.0/setup/installing/mac-osx
[install rhel & centos]: /riak/kv/2.0.0/setup/installing/rhel-centos
[install smartos]: /riak/kv/2.0.0/setup/installing/smartos
[install solaris]: /riak/kv/2.0.0/setup/installing/solaris
[install suse]: /riak/kv/2.0.0/setup/installing/suse
[install windows azure]: /riak/kv/2.0.0/setup/installing/windows-azure
[install source index]: /riak/kv/2.0.0/setup/installing/source
[community projects]: /community/projects
[upgrade index]: /riak/kv/2.0.0/setup/upgrading

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
