---
title: Rolling Upgrades for Riak CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [upgrading]

---

Each node in a Riak CS cluster contains settings that define its operating modes and API coverage.  The following steps outline the process of upgrading Riak CS in a rolling fashion.  

Be sure to check the Riak CS [[Version Compatibility]] chart to ensure that your version of Riak, Riak CS and Stanchion have been tested to work together.


1. Stop Riak, Riak CS, and Stanchion:

	```
	riak stop
	riak-cs stop
	stanchion stop
	```

2. Back up Riak's configuration files: 

	```
	sudo tar -czf riak_config_backup.tar.gz /etc/riak
	```
	
3. Optionally, back up your data directories:

	```
	sudo tar -czf riak_data_backup.tar.gz /var/lib/riak 
	```

4. Take note of the value of `cs_version` in `/etc/riak-cs/app.config`.


5. Upgrade Riak, Riak CS, and Stanchion:


	**Debian / Ubuntu**

	```
	sudo dpkg -i <riak_package_name>.deb
	sudo dpkg -i <riak-cs_package_name>.deb
	sudo dpkg -i <stanchion_package_name>.deb
	```
	
	**RHEL / Centos**
	
	```
	sudo rpm -Uvh <riak_package_name>.rpm
	sudo rpm -Uvh <riak-cs_package_name>.rpm
	sudo rpm -Uvh <stanchion_package_name>.rpm
	```

	<div class="note"><div class="title">Note on Package Name Change</div>
	<p>If you are upgrading Riak CS Enterprise Edition from a version before 1.3.0, a change to the package name can result in an upgrade error.  To address this, uninstall the old Riak CS package before installing the new one.</p>
	</div>



6. Examine the differences between your backed up `app.config` files and the newly installed copies in `etc/riak`, `etc/riak-cs`, `etc/stanchion`.  There may be new settings in the new `app.config` files.  Make any changes that are specific to your installation.

7. In the `/etc/riak-cs/app.config` file for Riak CS, locate the following setting:

	```
	{cs_version, 10300 },
	```

	This value will have changed from your previous installation.  To avoid conflicts between nodes, change this value to the one you noted in step 3.  This will restrict the Riak CS nodes to their previous version's capabilities until the rolling upgrade is fully complete.  If your previous `app.config` had no value for `cs_version`, use a value of 0.

8. Change cs_version to its previous value:

	```
	{cs_version, <previous_value> },
	```

9. Start the node:

	```
	riak start
	stanchion start
	riak-cs start
	```

10. Wait for any handoff to complete:

	```
	riak-admin transfers
	```

11. Move on to the next node and repeat this process throughout the cluster.

12. Once all nodes have been upgraded and restarted in this manner, once again locate the `/etc/riak-cs/app.config` file's `cs_version` setting and change it back to its upgraded value, as listed here:

	```
	{cs_version, 10300},
	```

13. Restart all Riak CS nodes with this new setting in the same rolling fashion as before:

	```
	riak-cs restart
	```