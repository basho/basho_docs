---
title: "Riak TS"
description: "Riak TS"
menu:
  riak_ts-1.5.1:
    name: "Riak TS"
    identifier: "introduction"
    weight: 100
    pre: riak
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/
---


[download]: downloads/
[installing]: setup/installing/
[learnabout]: learn-about/
[querying]: using/querying/
[supported clients]: developing/
[cluster ops]: using/core-fundamentals/#cluster-operations


Riak TS is a distributed NoSQL key/value store optimized for time series data. With TS, you can associate a number of data points with a specific point in time. TS uses discrete slices of time to co-locate data. For example, humidity and temperature readings from a meter reported during the same slice of time will be stored together on disk.

Riak TS uses tables defined according to a schema for each coherent group of
data. This allows for both flexibility, in terms of collecting many different
sets of data, and enough structure to make the experience of working with the collected data better.

Suppose you have a number of meters (for humidity and temperature) spread across
the US. It would make sense to group your data by the state and meter number. You could even store data along multiple dimensions to ease querying.

Assuming we have a number of meters measuring humidity, temperature, and the general weather conditions, our table might look like:

```sql
CREATE TABLE BashoWeather
(
   region      VARCHAR   NOT NULL,
   state       VARCHAR   NOT NULL,
   meter_no    VARCHAR   NOT NULL,
   time        TIMESTAMP NOT NULL,
   weather     VARCHAR   NOT NULL,
   temperature DOUBLE,
   humidity    DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
);
```

Using this table we can map region, state and time (in 15 minute chunks) to weather condition, temperature, and humidity readings.


## Supported Operating Systems

* CentOS 6
* CentOS 7
* RHEL 6
* RHEL 7
* Ubuntu 14.04 (Trusty)
* Ubuntu 16.04 (Xenial)
* Debian 7 "Wheezy" (development only)
* Debian 8 "Jessie"
* OS X 10.11+ (development only)
* Amazon Linux 2016.09

{{% note %}}
LevelDB is the only supported backend.
{{% /note %}}

## Get Started

Get started with Riak TS by [downloading][download] the TS package, then check out how to [install][installing] it.

Once you have setup your first node, you will want to proceed with setting up the rest of your cluster. If you're already familiar with Riak KV, you'll feel right at home. If you're new to Riak, refer to our [Cluster Operations][cluster ops] documentation.

When you're up and running, check out our docs on [querying] or our [supported clients].

If you want to learn more about how Riak TS was designed and how it is structured, checkout our [Learn About Riak TS][learnabout] section.

## Academy Training

[Basho's Academy](https://academy.basho.com) courses offer hands-on experience with Riak TS, from installing Riak TS to using the Spark-Riak Connector. Sign up for a [free account](https://academy.basho.com/users/sign_up) to get started.
