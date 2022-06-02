---
title: "Open Files Limit"
description: ""
project: "riak_kv"
project_version: "2.2.1"
menu:
  riak_kv-2.2.1:
    name: "Open Files Limit"
    identifier: "performance_open_files_limit"
    weight: 101
    parent: "managing_performance"
toc: true
aliases:
  - /riak/2.2.1/ops/tuning/open-files-limit/
  - /riak/kv/2.2.1/ops/tuning/open-files-limit/
---

[plan backend]: {{<baseurl>}}riak/kv/2.2.1/setup/planning/backend/
[blog oracle]: http://blogs.oracle.com/elving/entry/too_many_open_files

Riak KV can accumulate a large number of open file handles during operation. The creation of numerous data files is normal, and the [backend][plan backend] performs periodic merges of data file collections to avoid accumulating file handles.

To accomodate this you should increase the open files limit on your system. We recommend setting a soft limit of 65536 and a hard limit of 200000.

{{% note %}}
Superuser or root access may be required to perform these steps.
{{% /note %}}

## Changing Limit For Current Session

Most operating systems can change the open-files limit for the current shell session using the `ulimit -n` command:

```bash
ulimit -n 200000
```

## Debian & Ubuntu

Start by checking the current open file limit values with:

```bash
ulimit -Hn # Hard limit
ulimit -Sn # Soft limit
```

If you installed Riak KV from a binary package, you will need to the add the following settings to the /etc/security/limits.conf file for the `riak` user:

```/etc/security/limits.conf
riak soft nofile 65536
riak hard nofile 200000
```

If you use initialization scripts to start Riak KV, you can create a /etc/default/riak file and add the following to specify a limit:

```/etc/default/riak
ulimit -n 200000
```

This file is automatically sourced from the initialization script, and the Riak KV process will inherit this setting. Since initialization scripts are always run as the root user, there’s no need to set limits in /etc/security/limits.conf.

## Enable PAM-Based Limits for Debian & Ubuntu

You can enable PAM-based user limits so that non-root users, such as the `riak` user, may specify a higher value for maximum open files.

For example, follow these steps to enable PAM-based limits for all users to allow a maximum of 200000 open files.

1\.  Edit /etc/pam.d/common-session and add the following line:

```/etc/pam.d/common-session
session    required   pam_limits.so
```

2\. Save and close the file. If /etc/pam.d/common-session-noninteractive exists, append the same line as above.

3\. Edit /etc/security/limits.conf and append the following lines to the file:

```/etc/security/limits.conf
* soft nofile 65536
* hard nofile 200000
```

4\. Save and close the file.

5\. (**Optional**) If you will be accessing the Riak KV nodes via secure shell (SSH), you should also edit /etc/ssh/sshd_config and uncomment the following line:

```/etc/ssh/sshd_config
#UseLogin no
```

And set its value to `yes` as shown here:

```/etc/ssh/sshd_config
UseLogin yes
```

6\. Restart the machine so the limits take effect and verify that the new limits are set with the following command:

```bash
ulimit -a
```

{{% note %}}
In the above examples, the open files limit is raised for all users of the system. The limit can be specified for the `riak` user only by substituting the
two asterisks (`*`) in the examples with `riak`.
{{% /note %}}


## CentOS & Red Hat

Start by checking the current open file limit values with:

```bash
ulimit -Hn # Hard limit
ulimit -Sn # Soft limit
```

If you installed Riak KV from a binary package, you will need to the add the following settings to the /etc/security/limits.conf file for the `riak` user:

```/etc/security/limits.conf
riak soft nofile 65536
riak hard nofile 200000
```

If you use initialization scripts to start Riak KV, you can create a /etc/default/riak file and add the following to specify a limit:

```/etc/default/riak
ulimit -n 200000
```

This file is automatically sourced from the initialization script, and the Riak KV process will inherit this setting. Since initialization scripts are always run as the root user, there’s no need to set limits in /etc/security/limits.conf.

## Enable PAM-Based Limits for CentOS and Red Hat

You can enable PAM-based user limits so that non-root users, such as the `riak` user, may specify a higher value for maximum open files.

For example, follow these steps to enable PAM-based limits for all users to allow a maximum of 200000 open files.

1\. Edit /etc/pam.d/login and add the following line:

```/etc/pam.d/login
session    required   pam_limits.so
```

2\. Save and close /etc/pam.d/login

3\. Edit /etc/security/limits.conf and append the following lines to the file:

```/etc/security/limits.conf
* soft nofile 65536
* hard nofile 200000
```

4\. Save and close the /etc/security/limits.conf file.

5\. Restart the machine so that the limits to take effect and verify that
the new limits are set with the following command:

```bash
ulimit -a
```

{{% note %}}
In the above examples, the open files limit is raised for all users of the system. The limit can be specified for the `riak` user only by substituting the
two asterisks (`*`) in the examples with `riak`.
{{% /note %}}


## Solaris

To increase the open file limit on Solaris, add the following line to the /etc/system file:

```/etc/system
set rlim_fd_max=200000
```

[Reference][blog oracle]

## Mac OS X El Capitan

Start by checking the current open file limit values with:

```bash
launchctl limit maxfiles
```

The response should look something like this:

```bash
maxfiles    65536          65536
```

The first column is the soft limit and the last column is the hard limit.

To change the open files limits on Mac OS X El Capitan, perform the following steps:

1\. Add the following line to your .bash_profile or analogous file:

```bash
ulimit -n 65536 200000
```

2\. Save and close the file. Next open /etc/sysctl.conf (or create it if it doesn't already exist) and add the following settings:

```/etc/sysctl.conf
kern.maxfiles=200000
kern.maxfilesperproc=200000
```

4\. Restart your computer and enter `ulimit -n` into your terminal. If your system is configured correctly, you should see that `maxfiles` has been set to 200000.


## Mac OS X Yosemite

Start by checking the current open file limit values with:

```bash
launchctl limit maxfiles
```

The response should look something like this:

```bash
maxfiles    65536          65536
```

The first column is the soft limit and the last column is the hard limit.

To change the open files limits on Mac OS X Yosemite, perform these steps:

1\. Add the following line to your .bash_profile or analogous file:

```bash
ulimit -n 65536 200000
```

2\. Save and close the file. Next edit the /etc/launchd.conf file and add:

```/etc/launchd.conf
limit maxfiles 200000
```

3\. Save and close the file.

4\. After restarting, verify the new limits by running:

```bash
launchctl limit maxfiles
```

The response output should look something like this:

```bash
maxfiles    65536          200000
```

## Mac OS X Older Versions

Start by checking the current open file limit values with:

```bash
launchctl limit maxfiles
```

The response should look something like this:

```bash
maxfiles    10240          10240
```

The first column is the soft limit and the last column is the hard limit.

To adjust the maximum open file limits in OS X 10.7 (Lion) up to but not including OS X Yosemite, perform the following steps:

1\. Edit (or create) /etc/launchd.conf and increase the limits by adding:

```bash
limit maxfiles 65536 200000
```

2\. Save the file and restart the system for the new limits to take effect.

3\. After restarting, verify the new limits by running:

```bash
launchctl limit maxfiles
```

The response output should look something like this:

```bash
maxfiles    65536          200000
```
