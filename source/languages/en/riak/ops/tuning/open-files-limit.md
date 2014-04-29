---
title: Open Files Limit
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: advanced
keywords: [troubleshooting, os]
moved: {
    '1.4.0-': '/cookbooks/Open-Files-Limit'
}
---

Riak can consume a large number of open file handles during normal operation. The Bitcask backend in particular may accumulate a high number of data files before it has a chance to run a merge process. You can count the number of data files in the Bitcask directory with following command:

```bash
ls data/bitcask/*/* | wc -l
```

Please note that the creation of numerous data files is normal. Each time Riak is started Bitcask creates a new data file per partition; every so often Bitcask will merge a collection of data files into a single file to avoid accumulating file handles. It’s possible to artificially inflate the number of file handles Bitcask uses by repeatedly writing data and restarting Riak. The shell command below illustrates this issue:

```bash
for i in {1..100}
  do
    riak stop
    riak start
    sleep 3
    curl http://localhost:8098/riak/test -X POST -d "x" \
      -H "Content-Type: text/plain"
    ls data/bitcask/*/* | wc -l
done
```

## Changing the limit

Most operating systems can change the open-files limit using the `ulimit -n` command. Example:

```bash
ulimit -n 65536
```

However, this only changes the limit for the **current shell session**. Changing the limit on a system-wide, permanent basis varies more between systems.

## Linux

On most Linux distributions, the total limit for open files is controlled by `sysctl`.

```bash
sysctl fs.file-max
fs.file-max = 50384
```

As seen above, it is generally set high enough for Riak. If you have other things running on the system, you might want to consult the [[sysctl manpage|http://linux.die.net/man/8/sysctl]] for how to change that setting. However, what most needs to be changed is the per-user open files limit. This requires editing `/etc/security/limits.conf`, for which you'll need superuser access.
If you installed Riak or Riak Search from a binary package, add lines for the `riak` user like so, substituting your desired hard and soft limits:

```config
riak soft nofile 4096
riak hard nofile 65536
```

On Ubuntu, if you’re always relying on the init scripts to start Riak, you can create the file `/etc/default/riak` and specify a manual limit like so:

```bash
ulimit -n 65536
```

This file is automatically sourced from the init script, and the Riak process started by it will properly inherit this setting. As init scripts are always run as the root user, there’s no need to specifically set limits in `/etc/security/limits.conf` if you’re solely relying on init scripts.

On CentOS/RedHat systems, make sure to set a proper limit for the user you’re usually logging in with to do any kind of work on the machine, including managing Riak. On CentOS, `sudo` properly inherits the values from the executing user.

Reference: [[http://www.cyberciti.biz/faq/linux-increase-the-maximum-number-of-open-files/]]

### Enable PAM-Based Limits for Debian & Ubuntu

It can be helpful to enable PAM user limits so that non-root users, such as the `riak` user, may specify a higher value for maximum open files. For example, follow these steps to enable PAM user limits and set the soft and hard values *for all users of the system* to allow for up to *65536* open
files.

Edit `/etc/pam.d/common-session` and append the following line:
    
```config
session    required   pam_limits.so
```
    
If `/etc/pam.d/common-session-noninteractive` exists, append the same line as above.

Save and close the file.

Edit `/etc/security/limits.conf` and append the following lines to the file:

```config    
*               soft     nofile          65536
*               hard     nofile          65536
```

4. Save and close the file.

5. (optional) If you will be accessing the Riak nodes via secure shell (ssh), you should also edit `/etc/ssh/sshd_config` and uncomment the following line:

    ```config
    #UseLogin no
    ```

    and set its value to `yes` as shown here:

    ```config
    UseLogin yes
    ```

6. Restart the machine so that the limits to take effect and verify that the new limits are set with the following command:
    
    ```bash
    ulimit -a
    ```

### Enable PAM-Based Limits for CentOS and Red Hat

1. Edit `/etc/security/limits.conf` and append the following lines to the file:
    
    *               soft     nofile          65536
    *               hard     nofile          65536

2. Save and close the file.

3. Restart the machine so that the limits to take effect and verify that the new limits are set with the following command:
    
    ```bash
    ulimit -a
    ```

<div class="note">
<div class="title">Note</div> In the above examples, the
open files limit is raised for all users of the system. If you prefer, the
limit can be specified for the riak user only by substituting the two
asterisks (<tt>*</tt>) in the examples with <tt>riak</tt>.
</div>

## Solaris

In Solaris 8, there is a default limit of 1024 file descriptors per process. In Solaris 9, the default limit was raised to 65536. To increase the per-process limit on Solaris, add the following line to `/etc/system`:

```config
set rlim_fd_max=65536
```

Reference: [[http://blogs.oracle.com/elving/entry/too_many_open_files]]

## Mac OS X

To check the current limits on your Mac OS X system, run:

```bash
launchctl limit maxfiles
```

The last two columns are the soft and hard limits, respectively.

To adjust the maximum open file limits in OS X 10.7 (Lion) or newer, edit `/etc/launchd.conf` and increase the limits for both values as appropriate.

For example, to set the soft limit to 16384 files, and the hard limit to 32768 files, perform the following steps:

1. Verify current limits:

    ```bash
    launchctl limit
    ```

    The response output should look something like this:

    ```
    cpu         unlimited      unlimited
    filesize    unlimited      unlimited
    data        unlimited      unlimited
    stack       8388608        67104768
    core        0              unlimited
    rss         unlimited      unlimited
    memlock     unlimited      unlimited
    maxproc     709            1064
    maxfiles    10240          10240
    ```

2. Edit (or create) `/etc/launchd.conf` and increase the limits. Add lines that look like the following (using values appropriate to your environment):

    ```bash
    limit maxfiles 16384 32768
    ```

3. Save the file, and restart the system for the new limits to take effect. After restarting, verify the new limits with the launchctl limit command:

    ```bash
    launchctl limit
    ```

    The response output should look something like this:

    ```
    cpu         unlimited      unlimited
    filesize    unlimited      unlimited
    data        unlimited      unlimited
    stack       8388608        67104768
    core        0              unlimited
    rss         unlimited      unlimited
    memlock     unlimited      unlimited
    maxproc     709            1064
    maxfiles    16384          32768
    ```
