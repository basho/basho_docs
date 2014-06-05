---
title: Installing on Windows Azure
project: riak
version: 1.1.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, windows, azure]
prev: "[[Installing on SUSE]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on AWS Marketplace]]"
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-Windows-Azure'
}
---

Steps to install Riak on CentOS VMs using the Windows Azure platform.

## Creating CentOS VMs

You will need to sign up for the Windows Azure Virtual Machines preview feature in order to create a virtual machine. You can also sign up for a free trial account if you do not have a Windows Azure account.

1. Navigate to [https://account.windowsazure.com](https://account.windowsazure.com/) and sign in with your Windows Azure account.

2. Click "preview features" to view the available previews.

    ![](/images/antares-iaas-preview-01.png)

3. Scroll down to Virtual Machines & Virtual Networks and click "try it now".

    ![](/images/antares-iaas-preview-02.png)

4. Select your subscription and click the check.

    ![](/images/antares-iaas-preview-04.png)

### Create a virtual machine running CentOS Linux

1. Login to the Windows Azure (Preview) Management Portal using your Windows Azure account.

2. In the Management Portal, at the bottom left of the web page, click "+New", click "Virtual Machine", and then click "From Gallery".

    ![](/images/createvm_small.png)

3. Select a CentOS virtual machine image from "Platform Images", and then click the next arrow at the bottom right of the page.

    ![](/images/vmconfiguration0.png)

4. On the VM Configuration page, provide the following information:
    - Provide a "Virtual Machine Name", such as "testlinuxvm".
    - Specify a "New User Name", such as "newuser", which will be added to the Sudoers list file.  **Do NOT** use the username "riak", as it may conflict with the installation package.
    - In the "New Password" box, type a strong password.
    - In the "Confirm Password" box, retype the password.
    - Select the appropriate "Size" from the drop down list.
    - Click the next arrow to continue.

    ![](/images/vmconfiguration1.png)

5. On the VM Mode page, provide the following information:
    - **If this is the first node**, select the "STANDALONE VIRTUAL MACHINE" radio button. **Otherwise**, select the "CONNECT TO EXISTING VIRTUAL MACHINE" radio button, and select the first node in the drop down list.
    - In the "DNS Name" box, type a valid DNS address, e.g "testlinuxvm".
    - In the "Storage Account" box, select "Use Automatically Generated Storage Account".
    - In the "Region/Affinity Group/Virtual Network" box, select a region where this virtual image will be hosted.
    - Click the next arrow to continue.

    ![](/images/vmconfiguration2.png)

6. On the VM Options page, select "(none)" in the "Availability Set" box. Click the check mark to continue.

    ![](/images/vmconfiguration3.png)

7. Wait while Windows Azure prepares your virtual machine.

### Configure Endpoints

Once the virtual machine is created you must configure endpoints in order to remotely connect.

1. In the Management Portal, click "Virtual Machines", then click the name of your new VM, then click "Endpoints".

2. **If this is the first node**, click "Add Endpoint", leave "Add Endpoint" checked, hit the right arrow and fill out the next form as follows:
    - Name: https
    - Protocol: leave set to 'TCP'
    - Public Port: 443
    - private Port: 8069

## Connect to CentOS VMs using PuTTY or SSH

When the virtual machine has been provisioned and the endpoints configured you can connect to it using SSH or PuTTY.

### Connecting Using SSH

**For Linux & Mac Users:**

```bash
ssh newuser@testlinuxvm.cloudapp.net -o ServerAliveInterval=180
```
Enter the user's password.

**For Windows Users, use PuTTY:**

If you are using a Windows computer, connect to the VM using PuTTY. PuTTY can be downloaded from the [PuTTY Download Page](http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html).

1. Download and save putty.exe to a directory on your computer. Open a command prompt, navigate to that folder, and execute putty.exe.

2. Enter the SSH DETAILS as found on the Node's Dashboard, i.e., "testlinuxvm.cloudapp.net" for the Host Name and "22" for the Port.

    ![](/images/putty.png)

## Install Riak and configure using a shell script

1. **On each node**, once you've connected using the steps above, execute:

```bash
sudo su -
curl -s https://raw.github.com/basho/riak_on_azure/1.0/azure_install_riak.sh | sh
```

## Configure Riak using Riak Control

You can either use Riak Control or the command line to add nodes to your Riak Cluster. If you wish to add nodes via the command line, skip down to the section entitled "Configure Riak using Command Line"

1. Find the dns name and "Deployment ID" in the virtual machine dashboard of the VM you created the https endpoint for.  For Example:
    - **dns:** basho-example.cloudapp.net
    - **Deployment ID:** 7ea145743aeb4402a088da1234567890

2. Visit https://dns-name.cloudapp.net/admin in your browser

3. Ender 'admin' as the username, and the "Deployement ID" as the password.

4. Select 'Cluster' on the left.

5. Add VMs which also have the Riak software installed and configured by entering riak@yourhostnamehere in the input box, and clicking 'Add Node'.  Use the short name of each vm, not the DNS name.  For Example:
    - riak@basho-centos1

You now have a Riak cluster on Azure

## Configure Riak using Command Line

If you have already followed the instructions in the section "Configure Riak using Riak Control", skip this section.

First, SSH into the second (and subsequent nodes) and execute:

```bash
riak-admin cluster join riak@yourhostnamehere
```

(Where 'yourhostnamehere' is the short name of the **first node** in your cluster)

(NOTE: The host you choose can actually be any host that has already joined the cluster. The first host has no special significance, but it's important not to attempt to join to a node that hasn't joined a cluster yet.  Doing this would create a second cluster; thus we use the first node for these instructions.)

After all the nodes have have been joined to the first node via the previous command, connect to any of the nodes via SSH or PuTTY and execute the following:

```bash
riak-admin cluster plan
```

Verify all the nodes are listed as expected.  If the cluster plan looks good:

```bash
riak-admin cluster commit
```

To check the status of clustering use:

```bash
riak-admin member-status
```

You now have a Riak cluster on Azure

## Load Test Data

Execute on any one of the nodes:

```bash
curl -s http://rekon.basho.com | sh
```
    
Visit DNS address listed on the dashboard, at the port we opened as an endpoint:

```
http://testlinuxvm.cloudapp.net:8098/riak/rekon/go
```

Further Reading:

- [[Basic Riak API Operations|The Basics]]
