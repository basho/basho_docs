---
title: Riak TS Aggregate Functions
project: riakts
version: 1.1.0+
document: guide
toc: true
index: true
audience: beginner
---


##### Null Handling



### AVG

* **Since:** 1.1
* **Alias:** `MEAN`

```sql
SELECT AVG(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Mean average over the specified `sint64` or `double` column. Returns `NULL` if no columns were returned.

| Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### COUNT

* **Since:** 1.1
* **Alias:** None

```sql
SELECT COUNT(*) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Count the number of returned values. Returns `NULL` if no columns were returned.

| Input Type | Return Type |
|------------|-------------|
| Any | sint64 |
| `*` | sint64 |

### MAX

### MIN

### STDEV

### SUM

