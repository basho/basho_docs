---
title: Riak CS Quick Start Guide
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [installing]
---

The following instructions will guide you through installing a Riak CS test environment. This guide does not cover system/service tuning,
nor does it attempt to optimize your installation given your particular architecture.

## Installing Your First Node
**Step 1: Raise your system's open file limits**

Riak can consume a large number of open file handles during normal operation. See the [[Open Files Limit|Open-Files-Limit]] documentation for more information on how to increase your system's open files limit.

If you are the root user, you can increase the system's open files limit *for the current session* with this command:

    ulimit -n 65536

For this setting to persist, we also need to save it for the root and riak users in `/etc/security/limits.conf`:

    # ulimit settings for Riak CS
    root soft nofile 65536
    root hard nofile 65536
    riak soft nofile 65536
    riak hard nofile 65536

**Step 2: Download and install packages**

This guide uses `curl` for downloading packages and interacting with the Riak CS API so let's make sure it's installed:

    sudo apt-get install -y curl

If you are running Ubuntu 11.10 or later, you will also need the `libssl0.9.8` package. See  [[Installing on Debian and Ubuntu|Installing-on-Debian-and-Ubuntu]] for more information.

    sudo apt-get install -y libssl0.9.8

Now let's grab the Riak and Riak CS packages. Since this is our first node, we'll also be installing the Stanchion package as well.

As a licensed Riak CS customer, you can use your Basho provided credentials to access Riak CS from the [downloads](https://help.basho.com/forums/20747106-riak-cs-downloads) section of the Basho help desk website.

After downloading Riak EE, Stanchion, and Riak CS, install them using your operating system's package management commands.

First install Riak EE:

**RHEL6**:

    rpm -Uvh <riak-ee-package.rpm>

Replace `<riak-ee-package.rpm>` with the actual file name for the package you are installing.

**Ubuntu Lucid**:

    sudo dpkg -i <riak-ee-package.deb>

Replace `<riak-ee-package.deb>` with the actual file name for the package you are installing.

Next, install Riak CS:

RHEL6:

    rpm -Uvh <riak-cs-package.rpm>

Replace `<riak-cs-package.rpm>` with the actual file name for the package you are installing.

Ubuntu Lucid:

    sudo dpkg -i <riak-cs-package.deb>

Replace `<riak-cs-package.deb>` with the actual file name for the package you are installing.

Finally, install Stanchion:

RHEL 6:

    sudo rpm -Uvh <stanchion-package.rpm>

Replace `<stanchion-package.rpm>` with the actual file name for the package you are installing.

Ubuntu Lucid:

    sudo dpkg -i <stanchion-package.deb>

Replace `<stanchion-package.deb>` with the actual file name for the package you are installing.

**Step 3: Set service configurations and start the services**

We need to make some changes to the Riak configuration. We'll be editing
`/etc/riak/app.config`. First, we need to add this line to the
`riak_core` section, which starts off like:

```erlang
{riak_core, [
```

We'll add this line to that section:

```erlang
{default_bucket_props, [{allow_mult, true}]},
```

Next, Riak ships with Bitcask as the default backend.
We need to change this to the custom Riak CS backend.

Change the following line in `/etc/riak/app.config`

    {storage_backend, riak_kv_bitcask_backend}

to

    {add_paths, ["/usr/lib64/riak-cs/lib/riak_cs-1.2.2/ebin"]},
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

Next, we set our interface IP addresses in the app.config files. In a production environment, you will likely have multiple NICs, but for this test cluster, we are going to assume one NIC with an example IP address of 10.0.2.10.

Change the following lines in `/etc/riak/app.config`

    {http, [ {"127.0.0.1", 8098 } ]}
    {pb_ip,   "127.0.0.1" }

to

    {http, [ {"10.0.2.10", 8098 } ]}
    {pb_ip,   "10.0.2.10" }


Change the following lines in `/etc/riak-cs/app.config`

    {cs_ip, "127.0.0.1"}
    {riak_ip, "127.0.0.1"}
    {stanchion_ip, "127.0.0.1"}

to

    {cs_ip, "10.0.2.10"}
    {riak_ip, "10.0.2.10"}
    {stanchion_ip, "10.0.2.10"}


The cs_ip could also be set to 0.0.0.0 if you prefer Riak CS to listen on all interfaces.


Change the following lines in `/etc/stanchion/app.config`

    {stanchion_ip, "127.0.0.1"}
    {riak_ip, "127.0.0.1"}

to

    {stanchion_ip, "10.0.2.10"}
    {riak_ip, "10.0.2.10"}


Next, we set our service names. You can either use the local IP address for this or set hostnames. If you choose to set hostnames, you should ensure that the hostnames are resolvable by DNS or set in `/etc/hosts` on all nodes.

<div class="note"><div class="title">Note</div>Service names require at least one period in the name.</div>

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


That is the minimum amount of service configuration required to start a complete node. To start the services, type:

    sudo riak start
    sudo stanchion start
    sudo riak-cs start

<div class="info"><div class="title">Basho Tip</div>The order in which you start the services is important as each is a dependency for the next.</div>

Finally, you can check the liveness of your Riak CS installation with the `riak-cs ping` command, which should return `pong` if Riak CS is up and able to successfully communicate with Riak.

```bash
riak-cs ping
```

<div class="note"><div class="title">Note</div>The <tt>riak-cs ping</tt> command will fail if the Riak CS node is not able to communicate with the supporting Riak node. Ensure all components of the Riak CS system are running before checking liveness with <tt>riak-cs ping</tt>.</div>

**Step 4: Create the admin user**

Creating the admin user is an optional step, but it's a good test of our new services. Creating a Riak CS user requires
two inputs:

1. Name - a URL encoded string. Example: "admin%20user"

2. Email - a unique email address. Example: "admin@admin.com"

We can create the admin user with the following `curl` command:

To create an admin user, we need to grant permission to create new
users to the "anonymous" user.
This configuration setting is only required on a single Riak CS node.

Add this entry to
`/etc/riak-cs/app.config` immediately before the `{cs_ip, ...}`
entry:

    {anonymous_user_creation, true},

Then run `riak-cs restart` to put the new config setting into effect.

We can create the admin user with the following `curl` command, on the
same Riak CS machine where the `anonymous_user_creation` configuration
option was enabled:


```bash
curl -H 'Content-Type: application/json' \
  -X POST http://localhost:8080/riak-cs/user \
  --data '{"email":"admin@admin.com", "name":"admin"}'
```

The output of this command will be a JSON object that looks like this:

```bash
{"email":"admin@admin.com","display_name":"admin","name":"admin user","key_id":"5N2STDSXNV-US8BWF1TH","key_secret":"RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==","id":"4b823566a2db0b7f50f59ad5e43119054fecf3ea47a5052d3c575ac8f990eda7"}
```

The user's access key and secret key are returned in the `key_id` and `key_secret` fields respectively.

In this case, those keys are:

    Access key: 5N2STDSXNV-US8BWF1TH
    Secret key: RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==

You can use this same process to create additional Riak CS users. To make this user the admin user, we set these
keys in the Riak CS and Stanchion `app.config` files.

<div class="note"><div class="title">Note</div>The same admin keys will need to be set on all nodes of the cluster.</div>

Change the following lines in `/etc/riak-cs/app.config` on all Riak CS machines:

    {admin_key, "admin-key"}
    {admin_secret, "admin-secret"}

to

    {admin_key, "5N2STDSXNV-US8BWF1TH"}
    {admin_secret, "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw=="}

<div class="note"><div class="title">Note</div>Do not forget to remove the `anonymous_user_creation` setting!.</div>

Change the following lines in `/etc/stanchion/app.config`

    {admin_key, "admin-key"}
    {admin_secret, "admin-secret"}

to

    {admin_key, "5N2STDSXNV-US8BWF1TH"}
    {admin_secret, "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw=="}


Now we have to restart the services for the change to take effect:

    sudo stanchion restart
    sudo riak-cs restart

**Step 5: Testing the installation**

The simplest way to test the installation is using the `s3cmd` script. We can install it by typing:

    sudo apt-get -y install s3cmd

We need to configure `s3cmd` to use our Riak CS server rather than S3 as well as our user keys. To do that interactively, type:

    s3cmd --configure

There are 4 default settings you should change:

* Access Key - use the Riak CS user access key you generated above.
* Secret Key - use the Riak CS user secret key you generated above.
* Proxy Server - use your Riak CS IP. Example: 10.0.2.10
* Proxy Port - the default Riak CS port is 8080

Once `s3cmd` is configured, we can use it to create a test bucket:

    s3cmd mb s3://test-bucket

We can see if it was created by typing:

    s3cmd ls

We can now upload a test file to that bucket:

    dd if=/dev/zero of=test_file bs=1M count=2 # Create a test file
    s3cmd put test_file s3://test-bucket

We can see if it was uploaded by typing:

    s3cmd ls s3://test-bucket

We can now download the test file:

    rm test_file # remove the local test file
    s3cmd get s3://test-bucket/test_file


## Installing Additional Nodes
The process for installing additional nodes is identical to your first node with two exceptions:

1. Stanchion only needs to be installed on your first node; there is no need to install it again on each node. The `stanchion_ip` setting in your
    Riak CS `app.config` files should be set to the `stanchion_ip` from your first node.
2. To add additional nodes to the Riak cluster, use the following command:

        sudo riak-admin cluster join riak@10.0.2.10

    Where `riak@10.0.2.10` is the Riak node name set in your first node's `/etc/riak/vm.args` file

You will then need to verify the cluster plan with the `riak-admin cluster plan` command, and commit the cluster changes with `riak-admin cluster commit` to complete the join process. More information is available in the [[Command Line Tools|Command-Line-Tools---riak-admin#cluster]] documentation.

<div class="note"><div class="title">Note</div><strong>Riak CS is not designed to function directly on TCP port 80, and should not be operated in a manner which exposes it directly to the public internet</strong>. Instead, consider a load balancing solution, such as dedicated device, <a href="http://haproxy.1wt.eu">HAProxy</a> or <a href="http://wiki.nginx.org/Main">Nginx</a> between Riak CS and the outside world.</div>
