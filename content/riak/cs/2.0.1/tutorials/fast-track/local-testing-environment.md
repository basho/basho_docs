---
title: "Building a Local Test Environment"
description: ""
menu:
  riak_cs-2.0.1:
    name: "Building a Local Test Environment"
    identifier: "fast_track_local_test"
    weight: 101
    parent: "fast_track"
project: "riak_cs"
project_version: "2.0.1"
aliases:
  - /riakcs/2.0.1/cookbooks/tutorials/fast-track/Building-a-Local-Test-Environment/
  - /riak/cs/2.0.1/cookbooks/tutorials/fast-track/Building-a-Local-Test-Environment/
---

The following instructions will guide you through installing a Riak CS
test environment. This guide does not cover system/service tuning and it
does not attempt to optimize your installation for your particular
architecture.

If you want to build a testing environment with a minimum of
configuration, there is an option for [Building a Virtual Testing Environment]({{<baseurl>}}riak/cs/2.0.1/tutorials/fast-track/virtual-test-environment).

## Installing Your First Node

You should complete the following preparatory steps _before_ installing
and running Riak and Riak CS.

### Step 1: Raise your system's open file limits

Riak can consume a large number of open file handles during normal
operation. See the [Open Files Limit]({{<baseurl>}}riak/kv/2.1.3/using/performance/open-files-limit) document for more information on
how to increase your system's open files limit.

If you are the root user, you can increase the system's open files limit
*for the current session* with this command:

```bash
ulimit -n 65536
```

For this setting to persist in most Linux distributions, you also need
to save it for the `root` and `riak` users in
`/etc/security/limits.conf`:

```bash
# ulimit settings for Riak CS
root soft nofile 65536
root hard nofile 65536
riak soft nofile 65536
riak hard nofile 65536
```

For Mac OS X, consult the [open files limit]({{<baseurl>}}riak/kv/2.1.3/using/performance/open-files-limit/#mac-os-x) documentation.

### Step 2: Download and install packages

This guide uses `curl` for downloading packages and interacting with the
Riak CS API, so let's make sure that it's installed:

```bash
sudo apt-get install -y curl
```

**Note**: If you're running Riak CS on a non-Debian/Ubuntu OS,
substitute the appropriate CLI commands.

If you are running Ubuntu 11.10 or later, you will also need the
`libssl0.9.8` package. See [Installing on Debian and Ubuntu]({{<baseurl>}}riak/kv/2.1.3/setup/installing/debian-ubuntu) for more information.

```bash
sudo apt-get install -y libssl0.9.8
```

Now, grab the appropriate packages: Riak, Riak CS, and Stanchion. See
[Download Riak]({{<baseurl>}}riak/kv/2.1.3/downloads/) and [Download Riak CS]({{<baseurl>}}riak/cs/2.0.1/downloads).
You can skip Riak CS Control for now.

Once you have the packages, install them per the instructions below.

#### First, install Riak

The following links provide platform-specific instructions for
installing Riak.

**Do not attempt to configure or start Riak until step 3 in this
document.**

  * [Debian and Ubuntu]({{<baseurl>}}riak/kv/2.1.3/setup/installing/debian-ubuntu)
  * [RHEL and CentOS]({{<baseurl>}}riak/kv/2.1.3/setup/installing/rhel-centos)
  * [Mac OS X]({{<baseurl>}}riak/kv/2.1.3/setup/installing/mac-osx)
  * [FreeBSD]({{<baseurl>}}riak/kv/2.1.3/setup/installing/freebsd)
  * [SUSE]({{<baseurl>}}riak/kv/2.1.3/setup/installing/suse)
  * [Windows Azure]({{<baseurl>}}riak/kv/2.1.3/setup/installing/windows-azure)
  * [AWS Marketplace]({{<baseurl>}}riak/kv/2.1.3/setup/installing/amazon-web-services)
  * [From Source]({{<baseurl>}}riak/kv/2.1.3/setup/installing/source)

#### Next, install Riak CS

For Mac OS X:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/<riak-cs-os-x.tar.gz>
tar -xvzf <riak-cs-os-x.tar.gz>
```

Replace `<riak-cs-os-x.tar.gz>` with the actual filename for the package
you are installing.

For RedHat Enterprise distributions (and similar):

```bash
rpm -Uvh <riak-cs-package.rpm>
```

Replace `<riak-cs-package.rpm>` with the actual filename for the package
you are installing.

Ubuntu distributions and similar:

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package
you are installing.

#### Finally, install Stanchion

For Mac OS X:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/<stanchion-os-x.tar.gz>
tar -xvzf <stanchion-os-x.tar.gz>
```

Replace `<stanchion-os-x.tar.gz>` with the actual filename for the
package you are installing.

For RedHat Enterprise distributions (and similar):

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

Replace `<stanchion-package.rpm>` with the actual filename for the
package you are installing.

For Ubuntu distributions:

```bash
sudo dpkg -i <stanchion-package.deb>
```

Replace `<stanchion-package.deb>` with the actual filename for the
package you are installing.


### Step 3: Set service configurations and start the services

You will need to make changes to several configuration files.

#### `/etc/riak/riak.conf`

Be sure the storage backend is not set:

```riakconf
## Delete this line
storage_backend = . . .
```

And that the default bucket properties allow siblings:

```riakconf
## Append this line at the end of the file
buckets.default.allow_mult = true
```

Next, you need to expose the necessary Riak CS modules to Riak and instruct Riak
to use the custom backend provided by Riak CS. You'll have to use the old-style
`/etc/riak/advanced.config` for these settings. The file should look like:

```advancedconfig
[
 {riak_kv, [
              {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-2.0.1/ebin"]},
              {storage_backend, riak_cs_kv_multi_backend},
              {multi_backend_prefix_list, [{<<"0b:">>, be_blocks}]},
              {multi_backend_default, be_default},
              {multi_backend, [
                  {be_default, riak_kv_eleveldb_backend, [
                      {total_leveldb_mem_percent, 30},
                      {data_root, "/var/lib/riak/leveldb"}
                  ]},
                  {be_blocks, riak_kv_bitcask_backend, [
                      {data_root, "/var/lib/riak/bitcask"}
                  ]}
              ]}
  ]}
].
```

{{% note title="Note on OS-specific paths" %}}
The path for `add_paths` may be `/usr/lib/riak-cs` or `/usr/lib64/riak-cs`
depending on your operating system.
{{% /note %}}

Next, set your interface IP addresses in the `riak.conf` file. In a
production environment, you'd likely have multiple NICs, but for this
test cluster, assume one NIC with an example IP address of 10.0.2.10.

Change the following lines in `/etc/riak/riak.conf`

```riakconf
listener.http.internal = 127.0.0.1:8098
listener.protobuf.internal = 127.0.0.1:8087
```

to

```riakconf
listener.http.internal = 10.0.2.10:8098
listener.protobuf.internal = 10.0.2.10:8087
```

#### `/etc/riak-cs/riak-cs.conf`

Change the following lines in `/etc/riak-cs/riak-cs.conf`

```riakcsconf
listener = 127.0.0.1:8080
riak_host = 127.0.0.1:8087
stanchion_host = 127.0.0.1:8085
```

to

```riakcsconf
listener = 10.0.2.10:8080
riak_host = 10.0.2.10:8087
stanchion_host = 10.0.2.10:8085
```

The `listener` could also be set to `0.0.0.0 `if you prefer Riak CS to listen on
all interfaces.

#### `/etc/stanchion/stanchion.conf`

Change the following lines in `/etc/stanchion/stanchion.conf`

```stanchionconf
listener = 127.0.0.1:8085
riak_host = 127.0.0.1:8087
```

to

```stanchionconf
listener = 10.0.2.10:8085
riak_host = 10.0.2.10:8087
```

#### Service names

Next, set your service names, using either use the local IP address for
this or set hostnames. If you choose to set hostnames, you should ensure
that the hostnames are resolvable by DNS or set in `/etc/hosts` on all
nodes. **Note**: Service names require at least one period in the name.

Change the following line in `/etc/riak/riak.conf`

```riakconf
nodename = riak@127.0.0.1
```

to

```riakconf
nodename = riak@10.0.2.10
```

Then change the following line in `/etc/riak-cs/riak-cs.conf`

```riakcsconf
nodename = riak-cs@127.0.0.1
```

to

```riakcsconf
nodename = riak-cs@10.0.2.10
```

Change the following line in `/etc/stanchion/stanchion.conf`

```stanchionconf
nodename = stanchion@127.0.0.1
```

to

```stanchionconf
nodename = stanchion@10.0.2.10
```

#### Start the services

That is the minimum amount of service configuration required to start a
complete node. To start the services, run the following commands in the
appropriate `/bin` directories:

```bash
sudo riak start
sudo stanchion start
sudo riak-cs start
```

The order in which you start the services is important, as each is a
dependency for the next. Make sure that you successfully start Riak
before Stanchion and Stanchion before Riak CS.

You can check the liveness of your Riak CS installation and its
connection to the supporting Riak node. If the Riak CS node is running,
the following command should return `PONG`.

```bash
riak-cs ping
```

To check that the Riak CS node is communicating with its supporting Riak
node, run a `GET` request against the `riak-cs/ping` endpoint of the
Riak CS node. For example:

```curl
curl http://localhost:8080/riak-cs/ping
```

### Step 4: Create the admin user

Creating the admin user is an optional step, but it's a good test of our
new services. Creating a Riak CS user requires two inputs:

1. **Name** --- A URL-encoded string, e.g. `admin%20user`
2. **Email** --- A unique email address, e.g. `admin@admin.com`

To create an admin user, we need to grant permission to create new users
to the `anonymous` user. This configuration setting is only required on
a single Riak CS node.

Add this entry to `/etc/riak-cs/riak-cs.conf`:

```riakcsconf
anonymous_user_creation = on
```

Then run `sudo riak-cs stop && sudo riak-cs start` to put the new config setting into
effect.

We can create the admin user with the following `curl` command, on the
same Riak CS machine where the `anonymous_user_creation` configuration
option was enabled:

```curl
curl -XPOST http://localhost:8080/riak-cs/user \
  -H 'Content-Type: application/json' \
  -d '{"email":"admin@admin.com", "name":"admin"}'
```

The output of this command will be a JSON object that looks something like this:

```json
{
  "email": "admin@admin.com",
  "display_name": "admin",
  "name": "admin user",
  "key_id": "5N2STDSXNV-US8BWF1TH",
  "key_secret": "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==",
  "id": "4b823566a2db0b7f50f59ad5e43119054fecf3ea47a5052d3c575ac8f990eda7"
}
```

The user's access key and secret key are returned in the `key_id` and
`key_secret` fields respectively. Take note of these keys as they will
be required in the testing step.

In this case, those keys are:

* **Access key** --- `5N2STDSXNV-US8BWF1TH`
* **Secret key** -- `RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==`

You can use this same process to create additional Riak CS users. To
make this user the admin user, we set these keys in the Riak CS
`riak-cs.conf` and `stanchion.conf` files.

{{% note title="Note on admin keys" %}}
The same admin keys will need to be set on all nodes of the cluster.
{{% /note %}}

Change the following lines in `/etc/riak-cs/riak-cs.conf` on all Riak CS
machines:

```riakcsconf
admin.key = admin-key
admin.secret = admin-secret
```

to

```riakcsconf
admin.key = 5N2STDSXNV-US8BWF1TH
admin.secret = RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==
```

**Note**: Make sure to set the `anonymous_user_creation` setting to
`off` at this point.

Change the following lines in `/etc/stanchion/stanchion.conf`

```stanchion.conf
admin.key = admin-key
admin.secret = admin-secret
```

to

```stanchion.conf
admin.key = 5N2STDSXNV-US8BWF1TH
admin.secret = RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==
```

Now we have to restart the services for the change to take effect:

```bash
sudo stanchion stop && sudo stanchion start
sudo riak-cs stop && sudo riak-cs start
```

## Installing Additional Nodes

The process for installing additional nodes is identical to installing
your first node with two exceptions:

1. Stanchion only needs to be installed on your first node; there is no
   need to install it again on each node. The `stanchion_ip` setting in
   your Riak CS `app.config` files should be set to the `stanchion_ip`
   from your first node.
2. To add additional nodes to the Riak cluster, use the following
   command

    ```bash
    sudo riak-admin cluster join riak@10.0.2.10
    ```

    where `riak@10.0.2.10` is the Riak node name set in your first
    node's `/etc/riak/vm.args` file

You will then need to verify the cluster plan with the `riak-admin
cluster plan` command, and commit the cluster changes with `riak-admin
cluster commit` to complete the join process. More information is
available in the [Command Line Tools]({{<baseurl>}}riak/kv/2.1.3/using/admin/riak-admin/#cluster) documentation.

> **Note**
>
> **Riak CS is not designed to function directly on TCP port 80, and
should not be operated in a manner which exposes it directly to the
public internet**. Instead, consider a load balancing solution,
such as a dedicated device, [HAProxy](http://haproxy.1wt.eu),
or [Nginx](http://wiki.nginx.org/Main) between Riak CS and
the outside world.

Once you have completed this step, You can progress to [testing the Riak CS installation]({{<baseurl>}}riak/cs/2.0.1/tutorials/fast-track/test-installation) using s3cmd.
