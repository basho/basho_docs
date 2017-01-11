---
title: "Riak TS"
description: "Riak TS"
menu:
  riak_ts-1.3.1:
    name: "Riak TS"
    identifier: "introduction"
    weight: 100
    pre: riak
project: "riak_ts"
project_version: "1.3.1"
toc: true
aliases:
    - /riakts/1.3.1/
---


[download]: downloads/
[installing]: installing/
[learnabout]: learn-about/


Riak TS is a distributed NoSQL key/value store optimized for time series data. With TS, you can associate a number of data points with a specific point in time. Time quantization says “group data by 15 minute clumps, or 10 second clumps, or 60 day clumps” depending on how quickly your time series data come in and how you need to analyze them. For example, you can store humidity and temperature readings from a meter together.

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
)
```

Using this table we can map region, state and time (in 15 minute chunks) to weather condition, temperature, and humidity readings.


## Supported Operating Systems

* CentOS 6
* CentOS 7
* RHEL 6
* RHEL 7
* Ubuntu 12.04
* Ubuntu 14.04
* Debian 7 (development only)
* OS X 10.8 (development only)

>**Note:** At this release, LevelDB is the only supported backend.


## Get Started

Get started with Riak TS by [downloading][download] the TS package, then check out how to [install][installing] it.

If you want to learn more about how Riak TS was designed and how it is structured, checkout our [Learn About Riak TS][learnabout] section.