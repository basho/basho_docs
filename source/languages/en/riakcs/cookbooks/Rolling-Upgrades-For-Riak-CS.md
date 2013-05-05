---
title: Rolling Upgrades for Riak CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [upgrading]

---

Riak CS nodes negotiate with each other to determine supported operating modes and S3 API coverage.  This allows clusters containing mixed version of Riak CS to be upgraded in a rolling fashion.  

Be sure to check the Riak CS [[Version Compatibility]] chart to ensure that your version of Riak, Riak CS and Stanchion have been tested to work together.


## Debian/Ubuntu


1. Stop Riak, Riak CS, and Stanchion:

	```
	riak stop
	riak-cs stop
	stanchion stop
	```

2. Backup Riak's configuration and data directories (optional, but recommended):

	```
	sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
	```

3. Take note of the value of `cs_version` in `/etc/riak-cs/app.config`.


4. Upgrade Riak, Riak CS, and Stanchion:

	```
	sudo dpkg -i <riak_package_name>.deb
	sudo dpkg -i <riak-cs_package_name>.deb
	sudo dpkg -i <stanchion_package_name>.deb
	```

	<div class="note"><div class="title">Note on Package Name Change</div>
	<p>If you are upgrading Riak CS from a version before 1.3.0, a change to the package name can result in an upgrade error.  To address this, uninstall the old Riak CS package before installing the new one:</p>
	</div>

	```
	sudo dpkg -r <riak-cs_package_name_OLD>.deb
	sudo dpkg -i <riak-cs_package_name_NEW>.deb
	```


5. Examine the differences between your backed up `app.config` files and the newly installed copies in `etc/riak`, `etc/riak-cs`, `etc/stanchion`.  There may be new settings in the new `app.config` files.  Make any changes that are specific to your installation.

6. In the `/etc/riak-cs/app.config` file for Riak CS, locate the following setting:

	```
	{cs_version, 10300 },
	```

	This value will have changed from your previous installation.  To avoid conflicts between nodes, change this value to the one you noted in step 3.  This will restrict the Riak CS nodes to their previous version's capabilities until the rolling upgrade is fully complete.  If your previous `app.config` had no value for `cs_version`, use a value of 0.

7. Change cs_version to its previous value:

	```
	{cs_version, <previous_value> },
	```

8. Start the node:

	```
	riak start
	stanchion start
	riak-cs start
	```

9. Wait for any handoff to complete:

	```
	riak-admin transfers
	```

10. Move on to the next node and repeat this process throughout the cluster.

11. Once all nodes have been upgraded and restarted in this manner, once again locate the `/etc/riak-cs/app.config` file's `cs_version` setting and change it back to its upgraded value, as listed here:

	```
	{cs_version, 10300},
	```

12. Restart all Riak CS nodes with this new setting in the same rolling fashion as before:

	```
	riak-cs restart
	```



## RHEL/Centos


1. Stop Riak, Riak CS, and Stanchion:

	```
	riak stop
	riak-cs stop
	stanchion stop
	```

2. Backup Riak's configuration and data directories (optional, but recommended):

 	```
	sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
	```

3. Take note of the value of `cs_version` in `/etc/riak-cs/app.config`.

4. Upgrade Riak, Riak CS, and Stanchion:

	```
	sudo rpm -Uvh <riak_package_name>.rpm
	sudo rpm -Uvh <riak-cs_package_name>.rpm
	sudo rpm -Uvh <stanchion_package_name>.rpm
	```

	<div class="note"><div class="title">Note on Package Name Change</div>
	<p>If you are upgrading Riak CS from a version before 1.3.0, a change to the package name results in an upgrade error.  To address this, uninstall the old Riak CS package before installing the new one:</p>
	</div>

	```
	sudo rpm -ev <riak-cs_package_name_OLD>.rpm
	sudo rpm -ivh <riak-cs_package_name_NEW>.rpm
	```

5. Examine the differences between your backed up `app.config` files and the newly installed copies in `etc/riak`, `etc/riak-cs`, `etc/stanchion`.  There may be new settings in the new `app.config` files.  Make any changes that are specific to your installation.

6. In the `/etc/riak-cs/app.config` file for Riak CS, locate the following setting:

	```
	{cs_version, 10300 },
	```

	This value will have changed from your previous installation.  To avoid conflicts between nodes, change this value to the one you noted in step 3.  This will restrict the Riak CS nodes to their previous version's capabilities until the rolling upgrade is fully complete.  If your previous `app.config` had no value for `cs_version`, use a value of 0.

7. Change cs_version to its previous value, like so:

	```
	{cs_version, <previous_value> },
	```

8. Start the node:

	```
	riak start
	stanchion start
	riak-cs start
	```

9. Wait for any handoff to complete:

	```
	riak-admin transfers
	```

10. Move on to the next node and repeat this process throughout the cluster.

11. Once all nodes have been upgraded and restarted in this manner, once again locate the `/etc/riak-cs/app.config` file's `cs_version` setting and change it back to its upgraded value, as listed here:

	```
	{cs_version, 10300},
	```

12. Restart all Riak CS nodes with this new setting in the same rolling fashion as before:

	```
	riak-cs restart
	```
