---
title: Upgrading from Basho Data Platform 1.0.0 to 1.1.0
project: dataplatform
version: 1.1.0+
document: guide
toc: false
index: true
audience: beginner
---

There were many changes between the Basho Data Platform (BDP) 1.0.0 release and the 1.1.0 release. If you were previously using a BDP 1.0.0 release, you must uninstall the BDP 1.0.0 packages before you install the BDP 1.1.0 release.

##Uninstall Instructions

Choose the installation instructions below that match your OS.

###Ubuntu or Debian
Run the following to remove the BDP beta packages:

```bash
sudo dpkg --purge data-platform-extras
sudo dpkg --purge data-platform
```

Once you have run the above commands, you will need to check whether some files still remain. It is possible that some files will be need to be manually removed. Please check /usr/lib/riak or /usr/lib64/riak (for CentOS).

###CentOS or RHEL
Run the following to remove the BDP beta packages:

```bash
sudo yum remove data-platform-extras data-platform
```

Once you have run the above commands, you will need to check whether some files still remain. It is possible that some files will be need to be manually removed. Please check /usr/lib/riak or /usr/lib64/riak (for CentOS).
