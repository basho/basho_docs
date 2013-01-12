---
title: Key Filters
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator, os]
---

Key filters are a way to pre-process [[MapReduce]] inputs from a full bucket query simply by examining the key &mdash; without loading the object first. This is especially useful if your keys are composed of domain-specific information that can be analyzed at query-time.

## Understanding key filters

Key filters can be thought of as a series or pipeline of [transformations](#Transform-functions) and [predicates](#Predicate-functions) that attempt to match keys produced by the list-keys operation.  Keys that match the predicates are fed into the MapReduce query as if they had been specified manually.

To illustrate this, let's contrive an example.  Let's say we're storing customer invoices with a key constructed from the customer name and the date, in a bucket called "invoices".  Here are some sample keys:

<notextile><pre>basho-20101215
google-20110103
yahoo-20090613</pre></notextile>

Given that key scheme, here are some queries we will be able to do simply with key filters:

* Find all invoices from a given customer.
* Find all invoices from a range of dates.
* Find invoices from customers who have names containing the word "solutions".
* Find invoices that were sent on the 3rd of June.

Solutions to these queries are shown in the [examples](#Example-query-solutions) below.

Once the keys are filtered to only the items we care about, the normal MapReduce pipeline can further filter, transform, extract, and aggregate all the data we are interested in.

## Constructing key filters

Key filters change the structure of the "inputs" portion of the MapReduce query.

When submitting a query in JSON format, this makes the inputs a JSON object containing two entries, "bucket" and "key_filters". All filters are specified as arrays, even if the filter takes no arguments. Example:

```javascript
{
  "inputs":{
     "bucket":"invoices",
     "key_filters":[["ends_with", "0603"]]
  }
  // ... rest of mapreduce job
}
```

When submitting a query from the Erlang local or Protobuffs client, the inputs become a two-tuple where the first element is the bucket as a binary, and the second element is a list of filters. Like the JSON format, the filters are specified as lists, even for filters with no arguments, and the filter names are binaries.

```erlang
riakc_pb_socket:mapred(Pid, {<<"invoices">>, [[<<"ends_with">>,<<"0603">>]]}, Query).
```

## Transform functions

Transform key-filter functions manipulate the key so that it can be turned into a format suitable for testing by the [predicate functions](#Predicate-functions).  Each function description is followed by a sample usage in JSON notation.

### `int_to_string`

Turns an integer (previously extracted with `string_to_int`), into a string.

```javascript
[["int_to_string"]]
```

### `string_to_int`

Turns a string into an integer.

```javascript
[["string_to_int"]]
```

### `float_to_string`

Turns a floating point number (previously extracted with `string_to_float`), into a string.

```javascript
[["float_to_string"]]
```

### `string_to_float`

Turns a string into a floating point number.

```javascript
[["string_to_float"]]
```

### `to_upper`

Changes all letters to uppercase.

```javascript
[["to_upper"]]
```

### `to_lower`

Changes all letters to lowercase.

```javascript
[["to_lower"]]
```

### `tokenize`

Splits the input on the string given as the first argument and returns the nth token specified by the second argument.

```javascript
[["tokenize", "/", 4]]
```

### `urldecode`

URL-decodes the string.

```javascript
[["urldecode"]]
```

## Predicate functions

Predicate key-filter functions perform a test on their inputs and return true or false. As such, they should be specified last in a sequence of key-filters and are often preceded by [transform functions](#Transform-functions).

<div class="note"><div class="title">Comparison predicates</div> Predicates like `greater_than`, `less_than_eq`, and `between` follow Erlang's precedence rules for comparisons. Generally this means that numbers will be compared by value (including appropriate coercions) and strings will be compared lexically.</div>

### `greater_than`

Tests that the input is greater than the argument.

```javascript
[["greater_than", 50]]
```

### `less_than`

Tests that the input is less than the argument.

```javascript
[["less_than", 10]]
```

### `greater_than_eq`

Tests that the input is greater than or equal to the argument.

```javascript
[["greater_than_eq", 2000]]
```

### `less_than_eq`

Tests that the input is less than or equal to the argument.

```javascript
[["less_than_eq", -2]]
```

### `between`

Tests that the input is between the first two arguments.  If the third argument is given, it is whether to treat the range as inclusive. If the third argument is omitted, the range is treated as inclusive.

```javascript
[["between", 10, 20, false]]
```

### `matches`

Tests that the input matches the regular expression given in the argument.

```javascript
[["matches", "solutions"]]
```

### `neq`

Tests that the input is not equal to the argument.

```javascript
[["neq", "foo"]]
```

### `eq`

Tests that the input is equal to the argument.

```javascript
[["eq", "basho"]]
```

### `set_member`

Tests that the input is contained in the set given as the arguments.

```javascript
[["set_member", "basho", "google", "yahoo"]]
```

### `similar_to`

Tests that input is within the [[Levenshtein distance|http://en.wikipedia.org/wiki/Levenshtein_distance]] of the first argument given by the second argument.

```javascript
[["similar_to", "newyork", 3]]
```

### `starts_with`

Tests that the input begins with the argument (a string).

```javascript
[["starts_with", "closed"]]
```

### `ends_with`

Tests that the input ends with the argument (a string).

```javascript
[["ends_with", "0603"]]
```

### `and`

Joins two or more key-filter operations with a logical AND operation.

```javascript
["and", [["ends_with", "0603"]], [["starts_with", "basho"]]]
```

### `or`

Joins two or more key-filter operations with a logical OR operation.

```javascript
["or", [["eq", "google"]], [["less_than", "g"]]]
```

### `not`

Negates the result of key-filter operations.

```javascript
["not", [["matches", "solution"]]]
```

## Example query solutions

### Find all invoices for a given customer

```javascript
{
  "inputs":{
     "bucket":"invoices"
     "key_filters":[["tokenize", "-", 1],["eq", "basho"]]
   },
   // ...
}
```

### Find all invoices from a range of dates

```javascript
{
  "inputs":{
     "bucket":"invoices"
     "key_filters":[["tokenize", "-", 2],
                    ["between", "20100101", "20101231"]]
   },
   // ...
}
```

### Find invoices from customers who have names containing the word "solutions"


```javascript
{
  "inputs":{
     "bucket":"invoices"
     "key_filters":[["tokenize", "-", 1],
                    ["to_lower"],
                    ["matches", "solutions"]]
   },
   // ...
}
```

### Find invoices that were sent on the 3rd of June

```javascript
{
  "inputs":{
     "bucket":"invoices"
     "key_filters":[["ends_with", "0603"]]
   },
   // ...
}
```
