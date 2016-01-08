---
title: Riak TS Aggregate Functions
project: riakts
version: 1.1.0+
document: guide
toc: true
index: true
audience: intermediate
---

### AVG

* **Since:** 1.1.0
* **Alias:** `MEAN`

```sql
SELECT AVG(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Mean average over the specified `sint64` or `double` column.

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### COUNT

* **Since:** 1.1.0
* **Alias:** None

```sql
SELECT COUNT(*) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Count the number of returned values.

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| Any | sint64 |
| `*` | sint64 |

### MAX

```sql
SELECT MAX(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Return the largest value from the set of values returned by the query.

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### MIN

```sql
SELECT MIN(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Return the smallest value from the set of values returned by the query.

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### STDEV

```sql
SELECT STDEV(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Calculate the standard deviation for a set of values returned by the query.

Returns `NULL` if less than non-null two values were returned.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | double |
| double | double |

### SUM

* **Since:** 1.1.0
* **Alias:** None

```sql
SELECT SUM(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Return the sum of all the values of one `sint64` or `double` column returned by the query. 

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |