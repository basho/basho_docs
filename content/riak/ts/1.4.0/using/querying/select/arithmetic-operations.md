---
title: "Riak TS Arithmetic Operations"
description: "Riak TS Arithmetic Operations"
menu:
  riak_ts-1.4.0:
    name: "Arithmetic Operations"
    identifier: "arithmetic_operations_riakts"
    weight: 300
    parent: "select_riakts"
project: "riak_ts"
project_version: "1.4.0"
toc: true
version_history:
  present_from: "1.4.0+"
  moved:
    - ["1.1.0+", "using/arithmetic-operations"]
aliases:
    - /riak/ts/1.4.0/using/arithmetic-operations
    - /riakts/1.4.0/using/arithmetic-operations
    - /riakts/1.4.0/using/querying/select/arithmetic-operations
---


[querying select]: {{<baseurl>}}riak/ts/1.4.0/using/querying/#select-query


Riak TS supports arithmetic operations in the SELECT statement.

Arithmetic operations default to 64-bit integer math unless mixed with a
`double`, at which point they become floating-point.

{{% note title="Important" %}}
Proper spacing around arithmetic operators is required.
{{% /note %}}

The examples on this page will assume you are using the following table schema:

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
)
```

### Numeric Literals

Integer, decimal floating point, and exponent notation floating point
numeric literals are accepted.

```sql
SELECT 555, 1.1, 1e1, 1.123e-2 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns:

| 555\<SINT64\> | 1.1\<DOUBLE\> | 10.0\<DOUBLE\> | 0.01123\<DOUBLE\> |
|---------------|---------------|----------------|-----------------|
| 555           | 1.1           | 10.0           | 0.01123         |


### Addition and Subtraction

```sql
SELECT temperature, temperature + 1, temperature - 1 FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns: 

| temperature\<DOUBLE\> | (temperature\+1)\<DOUBLE\> | (temperature\-1)\<DOUBLE\> |
|-----------------------|----------------------------|-------------------------|
| 27.1                  | 28.1                       | 26.1                    |


### Multiplication and Division

```sql
SELECT temperature, temperature * 2, temperature / 2 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns:

| temperature\<DOUBLE\> | (temperature\*2)\<DOUBLE\> | (temperature/2)\<DOUBLE\> |
|-----------------------|----------------------------|-------------------------|
| 27.1                  | 54.2                       | 13.55                   |


### Negation

```sql
SELECT temperature, -temperature from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns:

| temperature\<DOUBLE\> | -temperature\<DOUBLE\> |
|-----------------------|----------------------|
| 27.1                  | -27.1                |


### Order of Operations

```sql
SELECT temperature + 2 * 3, (temperature + 2) * 3 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns:

| (temperature+(2\*3))\<DOUBLE\> | ((temperature\+2)\*3)\<DOUBLE\> |
|--------------------------------|-----------------------------|
| 33.1                           | 87.30000000000001           |


### Floating Point Odds and Ends

Operations on floating point numbers that would return `Infinity` or `NaN` are
not supported.

For example, neither of these queries return successfully:

```sql
SELECT 0.0 / 0.0 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'

SELECT 1.0 / 0.0 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```


### Operations with Multiple Column References

Operations involving two or more references to columns are not supported.

This query will return an error:

```sql
SELECT temperature + temperature FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```
