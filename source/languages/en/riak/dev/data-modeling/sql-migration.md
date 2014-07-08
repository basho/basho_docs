---
title: Migrating from an SQL Database to Riak
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [migration, sql]
---

Relational databases are powerful and reliable technologies, but there
are many [[use cases]] for which Riak is a better fit. If 

<div class="note">
<div class="title">Use cases warning</div>
Because data models vary so widely, it is difficult if not impossible to
generalize across all potential paths from an SQL database to Riak. This
document is intended only to suggest one possible approach to SQL data
migration&mdash;an approach that may not work well with your use case.
</div>

## Our Example

In this tutorial, we'll store a [PostgreSQL](http://www.postgresql.org/)
table housing a series of blog posts in Riak using a [Python](https://www.python.org/)
script relying on the [psycopg2](http://initd.org/psycopg/docs/)
library. The `posts` table we'll be converting was created using the
following SQL script:

```sql
CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    author VARCHAR(30) NOT NULL,
    title VARCHAR(50) NOT NULL,
    body TEXT NOT NULL,
    created DATE NOT NULL
);
```

A typical post looks like this when queried:

```sql
SELECT * FROM posts WHERE id = 99;
```

The response:

```
 id |   author   |             title              |            body            |  created
----+------------+--------------------------------+----------------------------+------------
 99 | John Daily | Riak Development Anti-Patterns | Writing an application ... | 2014-01-07
```

Our basic conversion and storage approach will be the following:

1. Each row will be converted into a JSON object storing all of the fields except the `id` field
2. The `id` field will not be in the JSON object because the `id`, an integer in this case, will act as our Riak [[key|Keys and Objects#keys]]
3. All of the JSON objects produced from the `posts` table will be stored in a single Riak [[bucket|Buckets]] called `posts`
4. The keys for our various objects will be stored in a [[Riak set|Using Data Types#sets]] so that all stored objects can be queried at once if need be

## Converting the Table to a List

Using the pysopg2 library, we can establish a connection to our database
(we'll call the database `blog_db`) and create a
[cursor](http://www.postgresql.org/docs/9.2/static/plpgsql-cursors.html)
object:

```python
import psycopg2

connection = psycopg2.connection('dbname=blog_database')
cursor = connection.cursor()
```

With that cursor, we can execute a `SELECT * FROM posts` query and then
fetch the information from the cursor using the `fetchall` function:

```python
cursor.execute('SELECT * FROM posts')
table = cursor.fetchall()
```

The `table` object consists of a Python list of tuples that looks
something like this:

```python
[(1, 'John Doe', 'Post 1 title', 'Post body ...', datetime.date(2014, 1, 1)),
 (2, 'Jane Doe', 'Post 2 title', 'Post body ...', datetime.date(2014, 1, 2)),
 # more posts in the list
]
```

As we can see, psycopg2 has automatically converted the `created` row
from a Postgres `DATE` data type into a Python datetime. At this point,
we need to convert each row into a Python [dictionary](https://docs.python.org/2/tutorial/datastructures.html#dictionaries).

## Converting Rows to JSON Objects

Converting rows in an SQL table to JSON can be tricky because rows can
contain a wide variety of data types, each of which must be converted
into one of the data types 
[compatible with JSON](http://en.wikipedia.org/wiki/JSON#Data_types.2C_syntax_and_example).
In our example, that conversion

Now, we can convert each row into a dict:

```python
import datetime

def convert_row_to_dict(row):
	return {
		'name': row[1],
		'title': row[2],
		'body': row[3],
		'created': row[4].strftime()
	}
```

Then, we can write a function that converts each row (as a dictionary)
into a Riak object and then stores that object:

```python
def store_row_in_riak(row):
	#
	obj = RiakObject(client, bucket, row[0])
	obj.content_type = 'application/json'
	obj.data = convert_row_to_dict(row)
	obj.store()
```

That enables us to write an iterator that stores all rows:

```python
# Using our "table" object from above:

for row in table:
	store_row_in_riak(row)
```

## Querying our Set of Objects

Once all of our JSON objects have been stored in Riak, we can perform
normal key/value operations to fetch them one by one. Here's an example:

```curl
curl http://localhost:8098/buckets/posts/keys/99
```

That will return a JSON object containing one of the blog posts from our
original table:

```json
{
  "author": "John Daily",
  "title": "Riak Development Anti-Patterns",
  "body": "Writing an application ...",
  "created": "2014-01-07"
}
```

But we can also fetch all of those objects at once if need be.
Previously, we stored the keys for all of our objects in a [[Riak set|Data Types#sets]].
We can write a function that fetches all of the values from that set
and then fetches all of the objects from the various keys in that set.