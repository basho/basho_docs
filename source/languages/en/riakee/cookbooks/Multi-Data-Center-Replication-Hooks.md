---
title: "Multi Data Center Replication: Hooks"
project: riakee
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator, troubleshooting]
---

## Overview

  This document is a guide to developing extensions for Riak EE Replication.

### Replication Hooks

   Riak replication allows applications to register replication hooks to control
   when extra objects need to be replicated along with the current object, or
   when an object should not be replicated.

   To register a hook, you must call:

```
riak_core:register([{repl_helper, MyMod}])
```

   Where MyMod defines the following functions:

### `send_realtime/2` (riak_object, RiakClient) -> ok | cancel | [riak_object]

   This hook controls whether an object replicated in realtime should be sent.
   To send this object, return 'ok', otherwise return 'cancel'.

### `send/2` (riak_object, RiakClient) -> ok | cancel | [riak_object]

   This hook is used in fullsync replication. As above, you can return ok or
   cancel, but you can also return a list of riak objects to be replicated
   immediately *before* the current object. This is useful for when you have an
   object that refers to other objects (eg. a chunked file) and want to be sure
   all the needed keys are replicated before the key referencing them is
   replicated.

### `recv/1` (riak_object) -> ok | cancel

   When an object is received by the client site, this hook is run. You can use
   it to update some metadata, or to deny the object.
