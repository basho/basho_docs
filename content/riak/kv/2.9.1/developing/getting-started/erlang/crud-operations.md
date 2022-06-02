---
title_supertext: "Getting Started:"
title: "CRUD Operations with Erlang"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "CRUD Operations"
    identifier: "getting_started_erlang_crud"
    weight: 100
    parent: "getting_started_erlang"
toc: true
---

## Creating Objects In Riak

First, let’s create a few Riak objects. For these examples we'll be
using the bucket `test`.

```erlang
MyBucket = <<"test">>.

Val1 = 1.
Obj1 = riakc_obj:new(MyBucket, <<"one">>, Val1).
riakc_pb_socket:put(Pid, Obj1).
```

In this first example, we have stored the integer 1 with the lookup key
of `one`. Next, let’s store a simple string value of `two` with a
matching key.

```erlang
Val2 = <<"two">>.
Obj2 = riakc_obj:new(MyBucket, <<"two">>, Val2).
riakc_pb_socket:put(Pid, Obj2).
```

That was easy. Finally, let’s store something more complex, a tuple this
time. You will probably recognize the pattern by now.

```erlang
Val3 = {value, 3}.
Obj3 = riakc_obj:new(MyBucket, <<"three">>, Val3).
riakc_pb_socket:put(Pid, Obj3).
```

## Reading Objects From Riak

Now that we have a few objects stored, let’s retrieve them and make sure
they contain the values we expect.

```erlang
{ok, Fetched1} = riakc_pb_socket:get(Pid, MyBucket, <<"one">>).
{ok, Fetched2} = riakc_pb_socket:get(Pid, MyBucket, <<"two">>).
{ok, Fetched3} = riakc_pb_socket:get(Pid, MyBucket, <<"three">>).

Val1 =:= binary_to_term(riakc_obj:get_value(Fetched1)). %% true
Val2 =:= riakc_obj:get_value(Fetched2).                 %% true
Val3 =:= binary_to_term(riakc_obj:get_value(Fetched3)). %% true
```

That was easy. We simply request the objects by bucket and key.

## Updating Objects In Riak

While some data may be static, other forms of data may need to be
updated. This is also easy to do. Let’s update the value in the third
example to 42, update the Riak object, and then save it.

```erlang
NewVal3 = setelement(2, Val3, 42).
UpdatedObj3 = riakc_obj:update_value(Fetched3, NewVal3).
{ok, NewestObj3} = riakc_pb_socket:put(Pid, UpdatedObj3, [return_body]).
```

We can verify that our new value was saved by looking at the value
returned.

```erlang
rp(binary_to_term(riakc_obj:get_value(NewestObj3))).
```

## Deleting Objects From Riak

Nothing is complete without a delete, as they say. Fortunately, that's
easy too.

```erlang
riakc_pb_socket:delete(Pid, MyBucket, <<"one">>).
riakc_pb_socket:delete(Pid, MyBucket, <<"two">>).
riakc_pb_socket:delete(Pid, MyBucket, <<"three">>).
```

Now we can verify that the objects have been removed from Riak.

```erlang
{error,notfound} =:= riakc_pb_socket:get(Pid, MyBucket, <<"one">>).
{error,notfound} =:= riakc_pb_socket:get(Pid, MyBucket, <<"two">>).
{error,notfound} =:= riakc_pb_socket:get(Pid, MyBucket, <<"three">>).
```

## Working With Complex Objects

Since the world is a little more complicated than simple integers and
bits of strings, let’s see how we can work with more complex objects.
Take, for example, this record that encapsulates some information about
a book.

```erlang
rd(book, {title, author, body, isbn, copies_owned}).

MobyDickBook = #book{title="Moby Dick",
                     isbn="1111979723",
                     author="Herman Melville",
                     body="Call me Ishmael. Some years ago...",
                     copies_owned=3}.
```

So we have some information about our Moby Dick collection that we want
to save. Storing this to Riak should look familiar by now:

```erlang
MobyObj = riakc_obj:new(<<"books">>,
                        list_to_binary(MobyDickBook#book.isbn),
                        MobyDickBook).

riakc_pb_socket:put(Pid, MobyObj).
```

Some of you may be thinking: "How does the Erlang Riak client
encode/decode my object?" If we fetch our book back and print the value,
we shall know:

```erlang
{ok, FetchedBook} = riakc_pb_socket:get(Pid,
                                        <<"books">>,
                                        <<"1111979723">>).

rp(riakc_obj:get_value(FetchedBook)).
```

The response:

```
<<131,104,6,100,0,4,98,111,111,107,107,0,9,77,111,98,121,
  32,68,105,99,107,107,0,15,72,101,114,109,97,110,32,77,
  101,108,118,105,108,108,101,107,0,34,67,97,108,108,32,
  109,101,32,73,115,104,109,97,101,108,46,32,83,111,109,
  101,32,121,101,97,114,115,32,97,103,111,46,46,46,107,0,
  10,49,49,49,49,57,55,57,55,50,51,97,3>>
```

Erlang binaries! The Riak Erlang client library encodes everything as
binaries. If we wanted to get a `book` object back we could use
`binary_to_term/1` to get our original object back:

```erlang
rp(binary_to_term(riakc_obj:get_value(FetchedBook))).
```

Next let’s clean up our mess:

```erlang
riakc_pb_socket:delete(Pid, <<"books">>, <<"1111979723">>).
riakc_pb_socket:stop(Pid).
```
