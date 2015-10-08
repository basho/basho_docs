---
title: Installing Riak
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, upgrading]
moved: {
    '1.4.0-': '/tutorials/installation',
    '2.1.1-': '/ops/building/installing',
}
---

## Supported Platforms

Riak is supported on numerous popular operating systems and virtualized
environments. The following information will help you to
properly install or upgrade Riak in one of the supported environments:

  * [[Amazon Web Services|Installing on Amazon Web Services]]
  * [[Debian & Ubuntu|Installing on Debian and Ubuntu]]
  * [[FreeBSD|Installing on FreeBSD]]
  * [[Mac OS X|Installing on Mac OS X]]
  * [[RHEL & CentOS|Installing on RHEL and CentOS]]
  * [[SmartOS|Installing on SmartOS]]
  * [[Solaris|Installing on Solaris]]
  * [[SUSE|Installing on SUSE]]
  * [[Windows Azure|Installing on Windows Azure]]
  * [[From Source|Installing Riak from Source]] *(for unlisted-operating systems, requires [[Erlang installation|Installing Erlang]])*

## Chef

  * [[Installing Riak With Chef]]

## Cloudsoft

Cloudsoft provides tested, optimized Riak blueprints to help develop and deploy
applications faster and easier on a wide range of environments. The AMP-Basho
software will automatically deploy and manage Basho Riak and Riak Enterprise
clusters using Apache Brooklyn's easy-to-use YAML blueprinting approach,
combined with Cloudsoft's enterprise-supported Application Management Platform
(AMP).

  * [Install Riak with Cloudsoft](https://github.com/cloudsoft/amp-basho)

## Building from Source

If your platform isn’t listed above, you may still be able to build Riak from source. Check out [[Installing Riak from Source]] for instructions.

## Upgrading

For information on upgrading an existing cluster, check out [[Rolling Upgrades]].
