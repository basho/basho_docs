---
title: Riak TS Arithmetic
project: riakts
version: 1.1.0+
document: guide
toc: true
index: true
audience: intermediate
---

Riak TS supports arithmetic operations in the select list.

As of Riak TS 1.1, arithmetic operations and aggregate functions cannot be mixed
in a single value expression.

Arithmetic operations default to 64-bit integer math, unless mixed with a
`double`, at which point they become floating-point.

### Addition and Subtraction

* **Since:** 1.1.0

```sql
SELECT temperature, temperature + 1, temperature - 1 FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

| temperature<DOUBLE> | (temperature+1)<DOUBLE> | (temperature-1)<DOUBLE> |
|---------------------|-------------------------|-------------------------+
| 27.1                | 28.1                    | 26.1                    |

### Multiplication and Division

* **Since:** 1.1.0

```sql
SELECT temperature, temperature * 2, temperature / 2 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

| temperature<DOUBLE> | (temperature*2)<DOUBLE> | (temperature/2)<DOUBLE> |
|---------------------|-------------------------|-------------------------+
| 27.1                | 54.2                    | 13.55                   |

### Negation

* **Since:** 1.1.0

```sql
SELECT temperature, -temperature from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

| temperature<DOUBLE> | -temperature<DOUBLE> |
|---------------------|----------------------|
| 27.1                | -27.1                |

### Order of Operations

* **Since:** 1.1.0

```sql
SELECT 1 + 2 * 3, (1 + 2) * 3 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

| (1+(2\*3))<SINT64> | ((1+2)\*3)<SINT64> |
|-------------------|-------------------|
| 7                 | 9                 |
