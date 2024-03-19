---
title: "Rolling Upgrades For Riak CS"
description: ""
menu:
  riak_cs-2.1.0:
    name: "Rolling Upgrades"
    identifier: "advanced_upgrades"
    weight: 100
    parent: "run_advanced"
project: "riak_cs"
project_version: "2.1.0"
lastmod: 2015-10-15T00:00:00-00:00
sitemap:
  priority: 0.2
aliases:
  - /riakcs/2.1.0/cookbooks/Rolling-Upgrades-For-Riak-CS/
  - /riak/cs/2.1.0/cookbooks/Rolling-Upgrades-For-Riak-CS/
---

Each node in a Riak CS cluster contains settings that define its
operating modes and API coverage. The following steps outline the
process of upgrading Riak CS in a rolling fashion.

Be sure to check the Riak CS [Version Compatibility]({{<baseurl>}}riak/cs/2.1.0/cookbooks/version-compatibility) chart to ensure that your version of Riak, Riak CS, and Stanchion have been tested to work together.  As Basho supports upgrades from the previous two major versions, this document will cover upgrades from Riak CS 1.4.x and Riak CS 1.5.x.

As Riak CS 2.0.0 only works with Riak 2.0.5, the underlying Riak installation
*must* be upgraded to Riak 2.0.5.

{{% note title="Note on upgrading from Riak CS < 1.5.4" %}}
<a href="https://github.com/basho/riak_cs/blob/release/1.5/RELEASE-NOTES.md#notes-on-upgrading">
Some key objects changed names</a> after the upgrade. Applications may need to
change their behaviour due to this bugfix.
{{% /note %}}

{{% note title="Note on upgrading from Riak CS < 1.5.1" %}}
<a href="https://github.com/basho/riak_cs/blob/release/1.5/RELEASE-NOTES.md#notes-on-upgrading-1">
Bucket number limitation per user</a> have been introduced in 1.5.1. Users who
have more than 100 buckets cannot create any bucket after the upgrade unless
the limit is extended in the system configuration.
{{% /note %}}

{{% note title="Note on upgrading From Riak CS 1.4.x" %}}
An operational procedure
<a href="https://github.com/basho/riak_cs/blob/release/1.5/RELEASE-NOTES.md#incomplete-multipart-uploads">
to clean up incomplete multipart under deleted buckets</a> is needed.
Otherwise new buckets with names that used to exist in the past can't be
created. The operation will fail with a `409 Conflict` error.

Leeway seconds and disk space should also be carefully watched during the
upgrade, because timestamp management of garbage collection has changed since
the 1.5.0 release. Consult the
<a href="https://github.com/basho/riak_cs/blob/release/1.5/RELEASE-NOTES.md#leeway-seconds-and-disk-space">
Leeway seconds and disk space</a> section of the 1.5 release notes for a more
detailed description.
{{% /note %}}

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

    <div class="note"><div class="title">Note on Patches</div>
    Remember to remove all patches from the `basho-patches` directory, as the
    version of Erlang has changed in Riak CS 2.0.  All official patches
    previously released by Basho have been included in this release.
    </div>

4. Upgrade Riak, Riak CS, and Stanchion. See the <a
    href="{{< baseurl >}}riak/cs/latest/downloads">Riak
    CS Downloads</a> and <a
    href="{{< baseurl >}}riak/kv/latest/downloads">Riak Downloads</a>
    pages to find the appropriate packages.

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

5. The `add_paths` setting in your configuration file must be updated to reflect
    the current version's `/ebin` directory.  To give an example, if the
    previous `/ebin` directory was located at
    `/usr/lib/riak-cs/lib/riak_cs-1.5.2/ebin` and you're upgrading to version
    2.0.0, you will need to change the value in `add_paths`:

    ```advancedconfig
    {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-2.0.0/ebin"]}
    ```

    ```appconfig
    {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-2.0.0/ebin"]}
    ```

6. Riak CS 2.0 introduces a new style of configuration known as `riak-cs.conf`.
    You may choose to continue the use of the `app.config` file, or migrate your
    existing configuration to `riak-cs.conf` (recommended).  If you choose to
    use `riak-cs.conf`, you should migrate all supported settings to the new
    format, and copy all others to the new `advanced.config` file.

    <div class="note"><div class="title">Note on Legacy app.config usage</div>
    **If you choose to use the legacy `app.config` files for Riak CS and/or
    Stanchion, some parameters have changed names and must be updated**.

    In particular, for the Riak CS `app.config`:
    \- `cs_ip` and `cs_port` have been combined into `listener`.
    \- `riak_ip` and `riak_pb_port` have been combined into `riak_host`.
    \- `stanchion_ip` and `stanchion_port` have been combined into
    `stanchion_host`.
    \- `admin_ip` and `admin_port` have been combined into `admin_listener`.
    \- `webmachine_log_handler` has become `webmachine_access_log_handler`.
    \- `{max_open_files, 50}` has been deprecated and should be replaced with
    `{total_leveldb_mem_percent, 30}`.

    For the Stanchion `app.config`:
    \- `stanchion_ip` and `stanchion_port` have been combined into `listener`.
    \- `riak_ip` and `riak_port` have been combined into `riak_host`.

    Each of the above pairs follows a similar form. For example, if your legacy
    `app.config` configuration was previously:

    ```
    {riak_cs, [
        {cs_ip, "127.0.0.1"},
        {cs_port, 8080 },
        . . .
    ]},
    ```

    It should now read:

    ```
    {riak_cs, [
        {listener, {"127.0.0.1", 8080}},
        . . .
    ]},
    ```

    and so on. More details can be found at [configuring Riak CS]({{<baseurl>}}riak/cs/2.1.0/cookbooks/configuration/riak-cs).
    </div>

    <div class="note"><div class="title">Note on Memory Sizing</div>
    Some changes have been made to both Riak and Riak CS that may warrant
    some performance tuning. Please consult the
    <a href="https://github.com/basho/riak_cs/blob/develop/RELEASE-NOTES.md#redesign-of-memory-sizing">
    Release Notes</a> for more details.
    </div>

7. Riak has also moved to the new configuration format, using a file called
   `riak.conf`. Remember to migrate all existing Riak configurations during
   the upgrade process. For example, the default bucket properties:

    ```riakconf
    buckets.default.allow_mult = true
    ```

    ```appconfig
    {riak_core, [
       ...
       {default_bucket_props, [{allow_mult, true}]},
       ...
    ]}.
    ```

8. Start the node:

    ```bash
    riak start
    stanchion start
    riak-cs start
    ```

9. Wait for any handoff to complete:

    ```bash
    riak-admin transfers
    ```

10. Move on to the next node and repeat this process throughout the
    cluster.
