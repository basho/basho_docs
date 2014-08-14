---
title: SNMP Configuration
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [snmp]
---

Riak Enterprise provides a built-in SNMP server that allows an external system, such as Hyperic, to query the Riak node for statistics such as the average get and put times as well as the number of puts and gets. This document only covers SNMP v2c support at this time.

## Configuration
The first step in configuring your SNMP setup is to edit the appropriate files in the Riak node's `etc/snmp/agent/conf/` directory.

First, edit the `agent.conf` file and set the appropriate IP on which the SNMP server should listen (Ex: `192.168.1.20`):

```erlang
{intAgentIpAddress, [192,168,1,20]}.
{intAgentUDPPort, 4000}.
{snmpEngineID, "agent's engine"}.
{snmpEngineMaxMessageSize, 484}.

%% Note: The commas in the IP are in the correct format
```

Next, edit the `community.conf` file if you would like to change your community from public to a different string.

Finally, edit the `standard.conf` file and update it with the proper information:

```erlang
{sysName, "Riak Node 1"}.
{sysDescr, "Riak Agent"}.
{sysContact, "syadmin@company.com"}.
{sysLocation, "System and Rack Location"}.
{sysObjectID, [3,6,1,4,1,193,19]}.  %% {ericsson otp} - don't change
{sysServices, 72}. %% don't change
```

Riak needs to be restarted for configuration changes to take affect.

**Note**: Prior to Riak Enterprise 0.13, SNMP configuration values were not reloaded during a restart.

To force Riak to reload SNMP configuration files on startup:

  1. Open `app.config` (most package installs place this file in `/etc/riak/`; Solaris package installs place this file in `/opt/riak/etc/`).

  2. Locate the SNPM term:

    ```erlang
    {snmp,
      [{agent,
        [{config, [{dir, "/etc/riak/snmp/agent/conf/"},
                   {force_load, true}]},
         {db_dir, "/var/lib/riak/snmp/agent/db/"}]}]}
    {snmp,
      [{agent,
        [{config, [{dir, "/etc/riak/snmp/agent/conf/"}]},
         {db_dir, "/var/lib/riak/snmp/agent/db/"}]}]}
    ```

  3. Add `{force_load, true}` to the `config` term:

    ```erlang
      {snmp,
        [{agent,
          [{config, [{dir, "/etc/riak/snmp/agent/conf/"},
           {force_load, true}]},
           {db_dir, "/var/lib/riak/snmp/agent/db/"}]}]}
    ```

  4. Save `app.config`

  5. Restart Riak

Once you have configured the SNMP settingsm you can start your Riak node and will be able to snmpwalk the node to verify that the setup is working:

```bash
$ snmpwalk -OS -c public -v2c -m all 192.168.52.129:4000 .
```

If you would like to query the OIDs associated with Riak you will need to reference the MIB shipped with Riak. For example, the x86_64 packages have the MIB in the following folder:

```bash
/usr/lib64/riak/lib/riak_snmp-0.2/priv/mibs
```

This folder can be referenced in the snmpwalk command as follows:

```bash
$ snmpwalk -OS -c public -v 2c -m ALL \
  -M +/usr/lib64/riak/lib/riak_snmp-0.2/priv/mibs \
  192.168.52.129:4000 RIAK 
```
