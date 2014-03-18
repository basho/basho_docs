---
title: Key Filters Reference
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: beginner
keywords: [developers, mapreduce, keyfilters]
moved: {
  '1.4.0-': '/cookbooks/Key-Filters'
}
---

## Transform functions

Transform key-filter functions manipulate the key so that it can be turned into a format suitable for testing by the [[predicate functions|Using Key Filters#Predicate-functions]]. Each function description is followed by a sample usage in JSON notation.

When using these from Erlang, function names (and key values) are binaries.

### `int_to_string`

Turns an integer (previously extracted with `string_to_int`), into a string.

```json
[["int_to_string"]]
```

### `string_to_int`

Turns a string into an integer.

```json
[["string_to_int"]]
```

### `float_to_string`

Turns a floating point number (previously extracted with `string_to_float`), into a string.

```json
[["float_to_string"]]
```

### `string_to_float`

Turns a string into a floating point number.

```json
[["string_to_float"]]
```

### `to_upper`

Changes all letters to uppercase.

```json
[["to_upper"]]
```

### `to_lower`

Changes all letters to lowercase.

```json
[["to_lower"]]
```

### `tokenize`

Splits the input on the string given as the first argument and returns the nth token specified by the second argument.

```json
[["tokenize", "/", 4]]
```

### `urldecode`

URL-decodes the string.

```json
[["urldecode"]]
```

## Predicate functions

Predicate key-filter functions perform a test on their inputs and return true or false. As such, they should be specified last in a sequence of key-filters and are often preceded by [[transform functions|Using Key Filters#Transform-functions]].

<div class="note"><div class="title">Comparison predicates</div> Predicates like `greater_than`, `less_than_eq`, and `between` follow Erlang's precedence rules for comparisons. Generally this means that numbers will be compared by value (including appropriate coercions) and strings will be compared lexically.</div>

### `greater_than`

Tests that the input is greater than the argument.

```json
[["greater_than", 50]]
```

### `less_than`

Tests that the input is less than the argument.

```json
[["less_than", 10]]
```

### `greater_than_eq`

Tests that the input is greater than or equal to the argument.

```json
[["greater_than_eq", 2000]]
```

### `less_than_eq`

Tests that the input is less than or equal to the argument.

```json
[["less_than_eq", -2]]
```

### `between`

Tests that the input is between the first two arguments.  If the third argument is given, it is whether to treat the range as inclusive. If the third argument is omitted, the range is treated as inclusive.

```json
[["between", 10, 20, false]]
```

### `matches`

Tests that the input matches the regular expression given in the argument.

```json
[["matches", "solutions"]]
```

### `neq`

Tests that the input is not equal to the argument.

```json
[["neq", "foo"]]
```

### `eq`

Tests that the input is equal to the argument.

```json
[["eq", "basho"]]
```

### `set_member`

Tests that the input is contained in the set given as the arguments.

```json
[["set_member", "basho", "google", "yahoo"]]
```

### `similar_to`

Tests that input is within the [[Levenshtein distance|http://en.wikipedia.org/wiki/Levenshtein_distance]] of the first argument given by the second argument.

```json
[["similar_to", "newyork", 3]]
```

### `starts_with`

Tests that the input begins with the argument (a string).

```json
[["starts_with", "closed"]]
```

### `ends_with`

Tests that the input ends with the argument (a string).

```json
[["ends_with", "0603"]]
```

### `and`

Joins two or more key-filter operations with a logical AND operation.

```json
[["and", [["ends_with", "0603"]], [["starts_with", "basho"]]]]
```

### `or`

Joins two or more key-filter operations with a logical OR operation.

```json
[["or", [["eq", "google"]], [["less_than", "g"]]]]
```

### `not`

Negates the result of key-filter operations.

```json
[["not", [["matches", "solution"]]]]
```
