---
title: Rolling Upgrades for Riak CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [upgrading]
---

Each node in a Riak CS cluster contains settings that define its
operating modes and API coverage. The following steps outline the
process of upgrading Riak CS in a rolling fashion.

Be sure to check the Riak CS [[Version Compatibility]] chart to ensure
that your version of Riak, Riak CS, and Stanchion have been tested to
work together.

1. Stop Riak, Riak CS, and Stanchion:

	```bash
	riak stop
	riak-cs stop
	stanchion stop
	```

2. Back up Riak's configuration files:

	```bash
	sudo tar -czf riak_config_backup.tar.gz /etc/riak
	```

3. Optionally, back up your data directories:

	```bash
	sudo tar -czf riak_data_backup.tar.gz /var/lib/riak
	```

4. If you are upgrading from a version prior to 1.3, take note of the
   value of `cs_version` in `/etc/riak-cs/app.config`.

5. Upgrade Riak, Riak CS, and Stanchion. See the <a
   href="http://docs.basho.com/riakcs/latest/riakcs-downloads">Riak
   CS Downloads</a> and <a
   href="http://docs.basho.com/riak/latest/downloads">Riak Downloads</a>
   pages to find the appropriate packages.

    **Mac OS X**

    ```bash
    curl -O http://s3.amazonaws.com/downloads.basho.com/<riakcs-os-x-package.tar.gz>
    tar -xvzf <riakcs-os-x-package.tar.gz>
    ```


	**Debian** / **Ubuntu**

	```bash
	sudo dpkg -i <riak_package_name>.deb
	sudo dpkg -i <riak-cs_package_name>.deb
	sudo dpkg -i <stanchion_package_name>.deb
	```

	**RHEL** / **CentOS**

	```bash
	sudo rpm -Uvh <riak_package_name>.rpm
	sudo rpm -Uvh <riak-cs_package_name>.rpm
	sudo rpm -Uvh <stanchion_package_name>.rpm
	```

    <div class="note">
    <div class="title">Note on Package Name Change</div>
    If you are upgrading Riak CS Enterprise Edition from a version before
    1.3.0, a change to the package name can result in an upgrade error.
    To address this, uninstall the old Riak CS package before installing
    the new one.
    </div>

6. The `add_paths` setting in the `riak_kv` section of each Riak node's
   `app.config` configuration file must be changed to reflect the new
   path of the node's `/ebin` directory. To give an example, if the
   previous `/ebin` directory was located at
   `/usr/lib/riak-cs/lib/riak_cs-1.5.2/ebin` and you're upgrading to
   version 1.5.3, you will need to change the value in `add_paths`:

   ```appconfig
   {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-1.5.2/ebin"]}

   %% should be changed to:

   {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-1.5.3/ebin"]}
   ```

7. Examine the differences between your backed up `app.config` files and
   the newly installed copies in `etc/riak`, `etc/riak-cs`, and
   `etc/stanchion`. There may be new settings in the new `app.config`
   files. Make any changes that are specific to your installation.

    One thing that you will likely need to update is the location of any
    `.beam` files that you are using for MapReduce jobs or commit hooks,
    in the `add_paths` subsection of the `riak_kv` settings in
    `app.config`. Here's an example:

	  ```appconfig
	  {add_paths, ["/old/path/to/beam/files"]},

 	  %% should be changed to:

	  {add_paths, ["/new/path/to/beam/files"]},
	  ```

8. If you are upgrading from a version prior to 1.3, locate the
   following setting in the `/etc/riak-cs/app.config` file for Riak CS:

	```erlang
	{cs_version, 10300},
	```

    This value will have changed from your previous installation.  To
    avoid conflicts between nodes, change this value to the one you noted
    in step 3.  This will restrict the Riak CS nodes to their previous
    version's capabilities until the rolling upgrade is fully complete.
    If your previous `app.config` had no value for `cs_version`, use a
    value of `0`.

9. If you are upgrading from a version prior to 1.3, change `cs_version`
   to its previous value:

	```erlang
	{cs_version, <previous_value>},
	```

10. Start the node:

	```bash
	riak start
	stanchion start
	riak-cs start
	```

11. Wait for any handoff to complete:

	```bash
	riak-admin transfers
	```

12. Move on to the next node and repeat this process throughout the
   cluster.

13. If you are upgrading from a version prior to 1.3, once all nodes
    have been upgraded and restarted in this manner, once again locate
    the `/etc/riak-cs/app.config` file's `cs_version` setting and change
    it back to its upgraded value, as listed here:

	```erlang
	{cs_version, 10300},
	```

14. Restart all Riak CS nodes with this new setting in the same rolling
    fashion as before:

	```bash
	riak-cs restart
	```
