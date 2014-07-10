---
title: Migrating from an SQL Database to Riak
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [migration, sql]
---

Relational databases are powerful and reliable technologies, but there
are many [[use cases]] for which Riak is a better fit, e.g. when data
availability is more important than SQL-style queryability or when
relational databases begin to run into scalability problems. You can
find out more in [[Why Riak]].

<div class="note">
<div class="title">Use cases warning</div>
Because data models vary so widely, it is difficult if not impossible to
generalize across all potential paths from an SQL database to Riak. This
document is intended only to suggest one possible approach to SQL data
migration&mdash;an approach that may not work well with your use case.
</div>

## Our Example

Let's say that we've been storing a series of blog posts in
[PostgreSQL](http://www.postgresql.org/), in a database called `blog`
and a table called `posts`. This table has the following schema:

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

In this tutorial, we'll store a table housing a series of blog posts in
Riak using a [Python](https://www.python.org/) script relying on
[psycopg2](http://initd.org/psycopg/docs/), a PostgreSQL driver for
Python.

Using the pysopg2 library, we can establish a connection to our database
(we'll call the database `blog_db`) and create a
[cursor](http://www.postgresql.org/docs/9.2/static/plpgsql-cursors.html)
object that will allow us to interact with the `posts` table using
traditional SQL commands:

```python
import psycopg2

connection = psycopg2.connection('dbname=blog_db')
cursor = connection.cursor()
```

With that cursor, we'll execute a `SELECT * FROM posts` query and then
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
from a Postgres `DATE` data type into a Python datetime. We'll need to
convert that datetime to a string when we convert each row to JSON in
the next section.

## Converting Rows to JSON Objects

In the section above, we saw that psycopg2 converted each row of our
`posts` table into a tuple with five elements (one for each column in
our table). Tuples aren't a terribly useful data type to store in Riak,
so we'll convert each row tuple into a Python
[dictionary](https://docs.python.org/2/tutorial/datastructures.html#dictionaries)
instead. The official [Riak Python client](https://github.com/basho/riak-python-client)
automatically converts Python dictionaries to JSON to store in Riak, so
once we have a list of dictionaries instead of tuples, we can store
those dictionaries directly in Riak.

Converting rows in an SQL table to dictionaries can be tricky because
rows can contain a wide variety of data types, each of which must be
converted into one of the data types 
[compatible with JSON](http://en.wikipedia.org/wiki/JSON#Data_types.2C_syntax_and_example).
That conversion is fairly straightforward in our example, as the `name`,
`title`, and `body` columns are automatically converted into strings.

The one tricky part will be the `date` column. Fortunately, Python's
[datetime](https://docs.python.org/2/library/datetime.html) library
makes this fairly simple. We can use the `strftime` function to
convert the `date` column into a formated string. We'll use a
month-day-year format, i.e. `%m-%d-%Y`.

```python
import datetime

def convert_row_to_dict(row):
	return {
		'author': row[1],
		'title': row[2],
		'body': row[3],
		'created': row[4].strftime('%m-%d-%Y')
	}
```

That will convert each row into a dictionary that looks like this:

```json
{
  'author': 'John Daily',
  'title': 'Riak Development Anti-Patterns',
  'body': 'Writing an application ...',
  'created': '01-07-2014'
}
```

## Storing Row Objects

Then, we can write a function that converts each row (as a dictionary)
into a Riak object and then stores that object:

```python
bucket = client.bucket('posts')

def store_row_in_riak(row):
	# We'll use the "id" column for the key, which is the first field
	# in each row tuple
	obj = RiakObject(client, bucket, row[0])
	obj.content_type = 'application/json'
	obj.data = convert_row_to_dict(row)
	obj.store()
```

As stated above, we'll want to store all of the objects' keys in a
[[Riak set|Data Types#sets]] to assist us in querying the objects in the
future. We'll modify the `store_row_in_riak` function above to add each
key to a set:

```python
from riak.datatypes import Set

objects_bucket = client.bucket('posts')
key_set = Set(client.bucket_type('sets').bucket('key_sets'), 'posts')

def store_row_in_riak(row):
	key = row[0]
	obj = RiakObject(client, bucket, key)
	obj.content_type = 'application/json'
	obj.data = convert_row_to_dict(row)
	obj.store()
```

Now we can write an iterator that stores all rows:

```python
# Using our "table" object from above:

for row in table:
	store_row_in_riak(row)
```

Once all of those objects have been stored in Riak, we can perform
normal key/value operations to fetch them one by one. Here's an example,
using curl and Riak's [[HTTP API]]:

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
  "created": "01-07-2014"
}
```

But we can also fetch all of those objects at once if need be.
Previously, we stored the keys for all of our objects in a
[[Riak set|Data Types#sets]]. We can write a function that fetches all
of the keys from that set and in turn all of the objects corresponding
to those keys:

```python
from riak.datatypes import Set

set_bucket = client.bucket_type('sets').bucket('key_sets')
posts_bucket = client.bucket('posts')

def fetch_all_objects(table_name):
	keys = Set(client, bucket, table_name)
	for key in keys:
		return posts_bucket.get(key)

fetch_all_objects('posts')
```

That will return the full list of Python dictionaries we stored earlier.
