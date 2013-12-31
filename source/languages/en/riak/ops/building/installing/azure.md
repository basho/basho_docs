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

Steps to install Riak on Centos VMs using the Windows Azure platform.

## Creating CentOS VMs

In order to create a virtual machine, you will first need to sign up for the Windows Azure Virtual Machines preview feature. You can also sign up for a free trial account if you do not have a Windows Azure account.

1. Navigate to [https://account.windowsazure.com](https://account.windowsazure.com/) and sign in with your Windows Azure account.

2. Click "preview features" to view the available previews.

    ![](/images/antares-iaas-preview-01.png)

3. Scroll down to Virtual Machines & Virtual Networks and click "try it now".

    ![](/images/antares-iaas-preview-02.png)

4. Select your subscription and click "Check".

    ![](/images/antares-iaas-preview-04.png)

### Create a virtual machine running CentOS Linux

1. Login to the Windows Azure (Preview) Management Portal using your Windows Azure account.

2. In the Management Portal, at the bottom left of the web page, click ""+New"", click "Virtual Machine", and then click "From Gallery".

    ![](/images/createvm_small.png)

3. Select a CentOS virtual machine image from Platform Images, and then click the next arrow at the bottom right of the page.

    ![](/images/vmconfiguration0.png)

4. On the VM Configuration page, provide the following information:
    - Virtual Machine Name, such as "testlinuxvm".
    - New User Name, such as "newuser", which will be added to the Sudoers list file.
    - New Password box - type a strong password.
    - In the Confirm Password box, retype the password.
    - Select the appropriate Size from the drop down list.
    - Click the next arrow to continue.

    ![](/images/vmconfiguration1.png)

5. On the VM Mode page, provide the following information:
    - **If this is the first node**, select the "STANDALONE VIRTUAL MACHINE" radio button. **Otherwise**, select the "CONNECT TO EXISTING VIRTUAL MACHINE" radio button, and select the first node in the drop down list.*
    - In the DNS Name box, type a valid DNS address, e.g "testlinuxvm".
    - In the Storage Account box, select "Use Automatically Generated Storage Account".
    - In the Region/Affinity Group/Virtual Network box, select a region where this virtual image will be hosted.
    - Click the next arrow to continue.

    ![](/images/vmconfiguration2.png)

6. On the VM Options page, select "(none)" in the Availability Set box. Click the check mark to continue.

    ![](/images/vmconfiguration3.png)

7. Wait while Windows Azure prepares your virtual machine.

### Configure Endpoints

Once the virtual machine is created you must configure endpoints in order to remotely connect.

1. In the Management Portal, click "Virtual Machines", then click the name of your new VM, then click "Endpoints".

2. **If this is the first node**, click "Add Endpoint", leave "Add Endpoint" checked, hit the right arrow and fill out the next form as follows:
    - Name: riak_web
    - Protocol: leave set to 'TCP'
    - Public Port: 8098
    - private Port: 8098

## Connect to CentOS VMs using PuTTY or SSH

When the virtual machine has been provisioned and the endpoints configured you can connect to it using SSH or PuTTY.

### Connecting Using SSH

**For Linux & Mac Users:**

    $ ssh newuser@testlinuxvm.cloudapp.net -o ServerAliveInterval=180
Enter the user's password.

**For Windows Users, use PuTTY:**

If you are using a Windows computer, connect to the VM using PuTTY. PuTTY can be downloaded from the [PuTTY Download Page](http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html).

1. Download and save putty.exe to a directory on your computer. Open a command prompt, navigate to that folder, and execute putty.exe.

2. Enter the SSH DETAILS as found on the Node's Dashboard, i.e., "testlinuxvm.cloudapp.net" for the Host Name and "22" for the Port.

    ![](/images/putty.png)

## Configure Centos and Riak using a shell script

1. On each node, once you've connected using the steps above:

Execute:

    sudo su -

    curl -s https://raw.github.com/glickbot/riak_on_azure/master/azure_install_riak.sh | sh

**FOR THE FIRST NODE**, note the "INTERNAL IP ADDRESS" listed on the right in the nodes dashboard.


**FOR ALL OTHER NODES**, use the "INTERNAL IP ADDRESS"" of the first node:

Execute:

    riak-admin cluster join riak@<ip.of.first.node>

## Cluster Riak & load test data

After all the nodes are installed, and joined using the steps above, connect to one of the nodes using SSH or PuTTY and execute the following:

    riak-admin cluster plan

If this looks good:

    riak-admin cluster commit

To check the status of clustering use:

    riak-admin member_status

You now have a Riak cluster on Azure

### Load test data

Execute on any one of the nodes:

    curl -sL http://rekon.basho.com | sh

Visit DNS address listed on the dashboard, at the port we opened as an endpoint:

    http://testlinuxvm.cloudapp.net:8098/riak/rekon/go

Further Reading:

- [[Basic Riak API Operations|The Basics]]
