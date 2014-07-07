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
table storing blog posts in Riak using a [Python](https://www.python.org/)
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

Our basic conversion and storage approach will be the following:

1. Each row will be converted into a JSON object storing all of the fields except the `id` field
2. The `id` field will not be in the JSON object because the `id`, an integer in this case, will act as our Riak [[key|Keys and Objects#keys]]
3. All of the JSON objects produced from the `posts` table will be stored in a single Riak [[bucket|Buckets]] called `posts`
4. The keys for our various objects will be stored in a [[Riak set|Using Data Types#sets]] so that all stored objects can be queried at once if need be

## Converting the Table to JSON Objects

For the database 'blog_database':

```python
import psycopg2

connection = psycopg2.connection('dbname=blog_database', ...)
cursor = connection.cursor()
```

Extract all the table information (as a list of tuples) and have a look
at one of the table entries:

```python
table = cursor.execute('SELECT * FROM posts;')
table[0]
# (101, 'John Daily', 'Riak Development Anti-Patterns', 'Writing an application that can take full advantage...', datetime.date(2014, 1, 7))
```

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

Then, we can write a function that converts each row (as a dict) into a
Riak object:

```python
def store_row_in_riak(row):
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