---
title: Riak CS Logging
project: riakcs
version: 1.5.0+
document: cookbook
audience: advanced
keywords: [riak-cs, logging, lager]
---

In versions 1.5.0 and later, you can use Riak CS in conjunction with
[Lager](https://github.com/basho/lager), the logging framework used for
Riak. By default, all Riak CS logs can be found in the `/log` directory
of each node.

You can configure Lager for Riak CS in the `advanced.config` configuration
file in each Riak CS node, in the section of that file named `lager`.
That section looks something like this:

```advancedconfig
{lager, [
    {handlers, [
    ...

    %% Other configs
]}
```

```appconfig
{lager, [
    {handlers, [
    ...

    %% Other configs
]}
```

A full description of all available parameters can be found in the
[[configuration files]] document for Riak.