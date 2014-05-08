---
title: Managing Active Anti-Entropy
project: riak
version: 2.0.0+
document: guide
audience: advanced
keywords: [operators, aae, active anti-entropy]
---

Riak's [[active anti-entropy]] (AAE) subsystem is a background process that
repairs object conflicts stemming from missing or divergent object
values across nodes.

## Enabling and Disabling AAE

Whether AAE is currently enabled in a node is determined by the value of
the `anti_entropy` parameter in the node's [[configuration files]]. In
Riak versions 2.0 and later, AAE is turned on by default. If AAE is
currently turned off, 

```riakconf
anti_entropy = active
```

```appconfig
{riak_kv, [

	%% Without debugging
	{anti_entropy, {on, []}},

	%% With debugging
	{anti_entropy, {on, [debug]}},

	%% More riak_kv settings...
]}
```

```appconfig
	%% Turned off
	{anti_entropy, off},
``