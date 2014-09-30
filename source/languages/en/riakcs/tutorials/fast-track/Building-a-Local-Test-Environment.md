---
title: Building a Local Test Environment
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [tutorial, fast-track, installing]
prev: "[[What is Riak CS]]"
up:   "[[The Riak CS Fast Track]]"
next: "[[Building a Virtual Testing Environment]]"
---

The following instructions will guide you through installing a Riak CS
test environment. This guide does not cover system/service tuning and it
does not attempt to optimize your installation given your particular
architecture.  This procedure is ideal only for building a test
environment either on local or remote hardware that allows for durable,
repeatable testing.

If you want to build a testing environment with a minimum of
configuration, there is an option for [[Building a Virtual Testing
Environment]].

## Installing Your First Node

### Step 1: Raise your system's open file limits

Riak can consume a large number of open file handles during normal
operation. See the [[Open Files Limit|Open-Files-Limit]] documentation
for more information on how to increase your system's open files limit.

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

For Mac OS X, consult the [[open files limit|Open Files Limit#Mac-OX-X]]
documentation.

### Step 2: Download and install packages

This guide uses `curl` for downloading packages and interacting with the
Riak CS API so let's make sure that it's installed:

```bash
sudo apt-get install -y curl
```

If you are running Ubuntu 11.10 or later, you will also need the
`libssl0.9.8` package. See [[Installing on Debian and Ubuntu]] for more
information.

```bash
sudo apt-get install -y libssl0.9.8
```

Now, grab the appropriate packages: Riak, Riak CS, and Stanchion. See
[[Download Riak|Downloads]] and [[Download Riak CS]]. You can skip Riak
CS Control for now.

Once you have the packages, install them per the instructions below.

#### First, install Riak

The following links provide platform-specific instructions for
installing Riak.

**Do not attempt to configure or start Riak until step 3 in this
document.**

  * [[Debian and Ubuntu|Installing on Debian and Ubuntu]]
  * [[RHEL and CentOS|Installing on RHEL and CentOS]]
  * [[Mac OS X|Installing on Mac OS X]]
  * [[FreeBSD|Installing on FreeBSD]]
  * [[SUSE|Installing on SUSE]]
  * [[Windows Azure|Installing on Windows Azure]]
  * [[AWS Marketplace|Installing on AWS Marketplace]]
  * [[From Source|Installing Riak from Source]]

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

We need to make changes to several configuration files.

#### `/etc/riak/app.config`

First, we need to add this line to the `riak_core` section, which starts
off like this:

```erlang
{riak_core, [
```

We'll add this line to that section:

```erlang
{default_bucket_props, [{allow_mult, true}]},
```

Next, Riak ships with Bitcask as the default backend. We need to change
this to the custom Riak CS backend.

Change the following line in `/etc/riak/app.config`

```erlang
{storage_backend, riak_kv_bitcask_backend}
```

to

```erlang
{add_paths, ["/usr/lib/riak-cs/lib/riak_cs-{{VERSION}}/ebin"]},
{storage_backend, riak_cs_kv_multi_backend},
{multi_backend_prefix_list, [{<<"0b:">>, be_blocks}]},
{multi_backend_default, be_default},
{multi_backend, [
    {be_default, riak_kv_eleveldb_backend, [
        {max_open_files, 50},
        {data_root, "/var/lib/riak/leveldb"}
    ]},
    {be_blocks, riak_kv_bitcask_backend, [
        {data_root, "/var/lib/riak/bitcask"}
    ]}
]},
```

<div class="note">
<div class="title">Note</div>
The path for `add_paths` may be `/usr/lib/riak-cs` or
`/usr/lib64/riak-cs` depending on your operating system.
</div>

Next, we set our interface IP addresses in the `app.config` files. In a
production environment, you will likely have multiple NICs, but for this
test cluster, we are going to assume one NIC with an example IP address
of 10.0.2.10.

Change the following lines in `/etc/riak/app.config`

```erlang
{http, [ {"127.0.0.1", 8098 } ]}
{pb,   [ {"127.0.0.1", 8087 } ]}
```

to

```erlang
{http, [ {"10.0.2.10", 8098 } ]}
{pb,   [ {"10.0.2.10", 8087 } ]}
```

#### `/etc/riak-cs/app.config`

Change the following lines in `/etc/riak-cs/app.config`

```erlang
{cs_ip, "127.0.0.1"}
{riak_ip, "127.0.0.1"}
{stanchion_ip, "127.0.0.1"}
```

to

```erlang
{cs_ip, "10.0.2.10"}
{riak_ip, "10.0.2.10"}
{stanchion_ip, "10.0.2.10"}
```

The `cs_ip` could also be set to `0.0.0.0 `if you prefer Riak CS to
listen on all interfaces.

Change the following lines in `/etc/stanchion/app.config`

```erlang
    {stanchion_ip, "127.0.0.1"}
    {riak_ip, "127.0.0.1"}
```

to

```erlang
{stanchion_ip, "10.0.2.10"}
{riak_ip, "10.0.2.10"}
```

#### Service names

Next, we set our service names. You can either use the local IP address
for this or set hostnames. If you choose to set hostnames, you should
ensure that the hostnames are resolvable by DNS or set in `/etc/hosts`
on all nodes.

<div class="note"
<div class="title">Note</div>
Service names require at least one period in the name
</div>

Change the following line in `/etc/riak/vm.args`

    -name riak@127.0.0.1

to

    -name riak@10.0.2.10


Change the following line in `/etc/riak-cs/vm.args`

    -name riak-cs@127.0.0.1

to

    -name riak-cs@10.0.2.10


Change the following line in `/etc/stanchion/vm.args`

    -name stanchion@127.0.0.1

to

    -name stanchion@10.0.2.10

#### Start the services

That is the minimum amount of service configuration required to start a
complete node. To start the services, run the following commands:

```bash
sudo riak start
sudo stanchion start
sudo riak-cs start
```

<div class="info"><div class="title">Basho Tip</div
The order in which you start the services is important as each is a
dependency for the next
</div>

Finally, you can check the liveness of your Riak CS installation with
the `riak-cs ping` command, which should return `pong` if Riak CS is up
and able to successfully communicate with Riak.

```bash
riak-cs ping
```

<div class="note"
<div class="title">Note</div>
The <code>riak-cs ping</code> command will fail if the Riak CS node is not
able to communicate with the supporting Riak node. Ensure all components
of the Riak CS system are running before checking liveness with
<code>riak-cs ping</code>.
</div>

### Step 4: Create the admin user

Creating the admin user is an optional step, but it's a good test of our new services. Creating a Riak CS user requires two inputs:

1. Name --- A URL-encoded string. Example: `admin%20user`
2. Email --- A unique email address. Example: `admin@admin.com`

To create an admin user, we need to grant permission to create new
users to the "anonymous" user. This configuration setting is only required on a single Riak CS node.

Add this entry to `/etc/riak-cs/app.config` immediately before the `{cs_ip, ...}` entry:

```erlang
{anonymous_user_creation, true},
```

Then run `riak-cs restart` to put the new config setting into effect.

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

The user's access key and secret key are returned in the `key_id` and `key_secret` fields respectively. Take note of these keys as they will be required in the testing step.

In this case, those keys are:

    Access key: 5N2STDSXNV-US8BWF1TH
    Secret key: RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==

You can use this same process to create additional Riak CS users. To make this user the admin user, we set these keys in the Riak CS and Stanchion `app.config` files.

<div class="note"><div class="title">Note</div>The same admin keys will need to be set on all nodes of the cluster.
</div>

Change the following lines in `/etc/riak-cs/app.config` on all Riak CS machines:

```erlang
{admin_key, "admin-key"}
{admin_secret, "admin-secret"}
```

to

```erlang
{admin_key, "5N2STDSXNV-US8BWF1TH"}
{admin_secret, "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw=="}
```

<div class="note"><div class="title">Note</div>Do not forget to remove
the <code>anonymous_user_creation</code> setting!</div>

Change the following lines in `/etc/stanchion/app.config`

```erlang
{admin_key, "admin-key"}
{admin_secret, "admin-secret"}
```

to

```erlang
{admin_key, "5N2STDSXNV-US8BWF1TH"}
{admin_secret, "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw=="}
```

Now we have to restart the services for the change to take effect:

```bash
sudo stanchion restart
sudo riak-cs restart
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
available in the [[Command Line Tools|riak-admin Command Line#cluster]]
documentation.

<div class="note"
<div class="title">Note</div>
<strong>Riak CS is not designed to function directly on TCP port 80, and
should not be operated in a manner which exposes it directly to the
public internet</strong>. Instead, consider a load balancing solution,
such as dedicated device, <a href="http://haproxy.1wt.eu">HAProxy</a>,
or <a href="http://wiki.nginx.org/Main">Nginx</a> between Riak CS and
the outside world.
</div>

Once you have completed this step, You can progress to [[Testing the
Riak CS Installation]] using s3cmd.
