---
title: Rolling Upgrade to Enterprise
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [installing, upgrading]
---

Instructions for upgrading:

  1. Backup your etc (app.config and vm.args) and data directories.
  2. Shutdown the node you are going to upgrade.
  3. Uninstall your riak package.
  4. Install the riak_ee package.
  5. A standard package uninstall should not have removed your data directories. If it did, move your backup to where the data directory should be.
  6. Copy any customizations from your backed up vm.args to the riak_ee installed one, these files may be identical.
  7. The app.config file from riak_ee will be significantly different from your backed up one. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original app.config file into the sections in the new one.
  8. Start Riak on the upgraded node.
