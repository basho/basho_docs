---
title: Object Updates
project: riak
version: 2.0.0+
document: tutorials
audience: beginner
keywords: [developers, updating, kv]
---

While Riak supports a variety of querying mechanisms, such as [[Riak
Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]],
we always recommend sticking to basic **C**reate, **R**ead, **U**pdate,
and **D**elete (CRUD) operations as much as possible, as these
operations are generally the most performant and reliable operations
that Riak offers. A complete guide to making decisions about which Riak
features to use can be found in our [[Application Guide|Building
Applications with Riak#Which-Features-Should-You-Consider]].

Amongst the four CRUD operations, object updates in Riak tend to be the
least straightforward and to require a bit more subtle reasoning on the
application side than the others. In this document, we'll discuss some
best practices for updating Riak objects and provide code examples for
each of our official [[client libraries]]: Java, Ruby, Python, .NET, and
Erlang.

<div class="note">
<div class="title">Note on immutable data</div>
An important thing to bear in mind is that Riak almost always performs
best with immutable data. If your use case allows for it, we suggest
sticking to that. If not, this tutorial shows you how to work with
mutable data in a way that is consistent with Riak's strengths.
</div>

## The Object Update Cycle

If you decide that your application requires mutable data in Riak, we
recommend that you:

* avoid high-frequency object updates to the same key (i.e. multiple
  updates per second for long periods of time), as this will degrade
  Riak performance; and that you
* follow a read-modify-write cycle when performing updates.

That cycle looks something like this:

1. **Read** the object from Riak. This step is important for updates
because this enables you to fetch the object's [[causal context]], which
is the information that Riak uses to make decisions about which object
values are most recent (this is especially useful for objects that are
frequently updated). This context object needs to be passed back to Riak
when you update the object. This step is handled for you by Basho's
client libraries as long as you perform a read prior to an update. In
addition, if you have chosen to allow Riak to generate
[[siblings|Conflict Resolution#Siblings]] \(which we recommend), you
should **resolve sibling conflicts** upon read if they exist. For more
on this, please see our documentation on [[conflict resolution]], along
with examples from our official client libraries:
  * [[Java|Conflict Resolution: Java]]
  * [[Ruby|Conflict Resolution: Ruby]]
  * [[Python|Conflict Resolution: Python]]
  * [[C#|Conflict Resolution: CSharp]]
2. **Modify the object** on the application side.
3. **Write** the new, modified object to Riak. Because you read the
object first, Riak will receive the object's causal context metadata.
Remember that this happens automatically.

In general, you should read an object before modifying it. Think of it
as performing a `GET` prior to any `PUT` when interacting with a REST
API.

<div class="note">
<div class="title">Note on strong consistency</div>
If you are using Riak's [[strong consistency|Using Strong Consistency]]
feature, it is not only desirable but also necessary to use the
read/modify/write cycle explained in the section above. If you attempt
to update an object without fetching the object first, your update
operation will necessarily fail. More information can be found in the
[[strong consistency documentation|Using Strong
Consistency#Strongly-Consistent-Writes]].
</div>

### Updating Deleted Objects

You should use the read-modify-write cycle explained above at all times,
_even if you're updating deleted objects_. The reasons for that can be
found in our documentation on [[tombstones|Object Deletion#Tombstones]].
There are some modifications that you may need to make if you are
updating objects that may have been deleted previously. If you are using
the Java client, an explanation and examples are given in the
[[Java-specific section below|Object Updates#Java-Client-Example]]. If
you are using the Python or Erlang clients, causal context for deleted
objects will be handled automatically. If you are using the Ruby client,
you will need to explicitly set the `deletedvclock` parameter to `true`
when reading an object, like so:

```ruby
bucket = client.bucket('fruits')
obj = bucket.get('banana', deletedvclock: true)
```

## Example Update

In this section, we'll provide an update example for Basho's official Ruby,
Python, .NET and Erlang clients. Because updates with the official Java client
functions somewhat differently, those examples can be found in the [[section
below|Object Updates#Java-Client-Example]].

For our example, imagine that you are storing information about NFL head
coaches in the bucket `coaches`, which will bear the bucket type
`siblings`, which sets `allow_mult` to `true`. The key for each object
is the name of the team, e.g. `giants`, `broncos`, etc. Each object will
consist of the name of the coach in plain text. Here's an example of
creating and storing such an object:

```ruby
bucket = client.bucket('coaches')
obj = bucket.get_or_new('seahawks', type: 'siblings')
obj.content_type = 'text/plain'
obj.raw_data = 'Pete Carroll'
obj.store
```

```python
bucket = client.bucket_type('siblings').bucket('coaches')
obj = RiakObject(client, bucket, 'seahawks')
obj.content_type = 'text/plain'
obj.data = 'Pete Carroll'
obj.store()
```

```csharp
var id = new RiakObjectId("siblings", "coaches", "seahawks");
var obj = new RiakObject(id, "Pete Carroll",
    RiakConstants.ContentTypes.TextPlain);
var rslt = client.Put(obj);
```

```erlang
Obj = riakc_obj:new({<<"siblings">>, <<"coaches">>},
                     <<"seahawks">>,
                     <<"Pete Carroll">>,
                     <<"text/plain">>).
riakc_pb_socket:put(Pid, Obj).
```

Every once in a while, though, head coaches change in the NFL, which
means that our data would need to be updated. Below is an example
function for updating such objects:

```ruby
def update_coach(team, new_coach)
  bucket = client.bucket('coaches')
  # The read phase
  obj = bucket.get_or_new(team, type: 'siblings')
  # The modify phase
  obj.data = new_coach
  # The write phase
  obj.store
end

# Example usage
update_coach('packers', 'Vince Lombardi')
```

```python
def update_coach(team, new_coach):
    bucket = client.bucket_type('siblings').bucket('coaches')
    # The read phase
    obj = bucket.get(team)
    # The modify phase
    obj.data = new_coach
    # The write phase
    obj.store()

# Example usage
update_coach('packers', 'Vince Lombardi')
```

```csharp
private void UpdateCoach(string team, string newCoach)
{
    var id = new RiakObjectId("siblings", "coaches", team);
    var getResult = client.Get(id);

    RiakObject obj = getResult.Value;
    obj.SetObject<string>(newCoach, RiakConstants.ContentTypes.TextPlain);
    client.Put(obj);
}
```

```erlang
update_coach(team, new_coach) ->
    {ok, Obj} = riakc_pb_socket:get(Pid,
                                    {<<"siblings">>, <<"coaches">>},
                                    <<team>>),
    ModifiedObj = riakc_obj:update(Obj, <<new_coach>>),
    riakc_pb_socket:put(Pid, ModifiedObj).

%% Example usage
update_coach('packers', 'Vince Lombardi')
```

In the example above, you can see the three steps in action: first, the
object is read, which automatically fetches the object's causal context;
then the object is modified, i.e. the object's value is set to the name
of the new coach; and finally the object is written back to Riak.

## Object Update Anti-patterns

The most important thing to bear in mind when updating objects is this:
you should always read an object prior to updating it _unless_ you are
certain that no object is stored there. If you are storing [[sensor
data|Use Cases#Sensor-Data]] in Riak and using timestamps as keys, for
example, then you can be sure that keys are not repeated. In that case,
making writes to Riak without first reading the object is fine. If
you're not certain, however, then we recommend always reading the object
first.

## Java Client Example

As with the other official clients, object updates using the Java client
will automatically fetch the object's causal context metadata, modify
the object, and then write the modified value back to Riak. You can
update object values by creating your own `UpdateValue` operations that
extend the abstract class `Update<T>`. An `UpdateValue` operation must
have an `apply` method that returns a new `T`. In our case, the data
class that we're dealing with is `User`. First, let's create a very
basic `User` class:

```java
public class User {
  public String username;
  public List<String> hobbies;

  public User(String username, List<String> hobbies) {
    this.name = username;
    this.hobbies = hobbies;
  }
}
```

In the example below, we'll create an update value operation called
`UpdateUserName`:

```java
import com.basho.riak.client.api.commands.kv.UpdateValue.Update;

public class UpdateUserName extends Update<User> {
    @Override
    public User apply(User original) {
        // update logic goes here
    }
}
```

In the example above, we didn't specify any actual update logic. Let's
change that by creating an `UpdateValue` operation that changes a `User`
object's `name` parameter:

```java
public class UpdateUserName extends Update<User> {
    private String newUsername;

    public UpdateUserName(String newUsername) {
        this.newUsername = newUsername;
    }

    @Override
    public User apply(User original) {
        original.username = newUsername;
        return original;
    }
}
```

Now, let's put our `UpdateUserName` operation into effect. In the
example below, we'll change a `User` object's `username` from whatever
it currently is to `cliffhuxtable1986`:

```java
import com.basho.riak.client.api.commands.kv.FetchValue;

Location location = new Location(...);
UpdateValue updateOp = new UpdateValue.Builder(location)
        .withFetchOption(FetchValue.Option.DELETED_VCLOCK, true)
        .withUpdate(new UpdateUserName("cliffhuxtable1986"))
        .build();
client.execute(updateOp);
```

You may notice that a fetch option was added to our `UpdateValue`
operation: `FetchValue.Option.DELETED_VCLOCK` was set to `true`.
Remember from the section above that you should always read an object
before modifying and writing it, _even if the object has been deleted_.
Setting this option to `true` ensures that the causal context is fetched
from Riak if the object has been deleted. We recommend always setting
this option to `true` when constructing `UpdateValue` operations.

### Clobber Updates

If you'd like to update an object by simply replacing it with an
entirely new value of the same type (unlike in the section above, where
only one property of the object was updated), the Java client provides
you with a "clobber" update that you can use to replace the existing
object with a new object of the same type rather than changing one or
more properties of the object. Imagine that there is a `User` object
stored in the bucket `users` in the key `cliffhuxtable1986`, as in the
example above, and we simply want to replace the object with a brand new
object:

```java
Location location = new Location(new Namespace("users"), "cliffhuxtable1986");
User brandNewUser = new User(/* new user info */);
UpdateValue updateOp = new UpdateValue.Builder(Location)
        // As before, we set this option to true
        .withFetchOption(FetchValue.Option.DELETED_VCLOCK, true)
        .withUpdate(Update.clobberUpdate(brandNewUser))
        .build();
client.execute(updateOp);
```

### No-operation Updates in Java

The Java client also enables you to construct **no-operation updates**
that don't actually modify the object and simply write the original
value back to Riak. What is the use of that, given that it isn't
changing the value of the object at all? No-operation updates can be
useful because they can help Riak resolve [[sibling conflicts|Conflict
Resolution#Siblings]]. If you have an object---or many objects, for that
matter---with siblings, a no-operation update will fetch the object _and
its causal context_ and write the object back to Riak with the same,
fetched context. This has the effect of telling Riak that you deem this
value to be most current. Riak can then use this information in internal
sibling resolution operations.

Below is an example:

```java
Location loc = new Location(...);
UpdateValue updateOp = new UpdateValue.Builder(loc)
        .withUpdate(Update.noopUpdate())
        .build();
client.execute(updateOp);
```

The example above would update the object without fetching it. You
could, however, use a no-operation update to _read_ an object as well if
you set `return_body` to `true` in your request:

```java
// Using the Location object "loc" from above:
UpdateValue updateOp = new UpdateValue.Builder(loc)
        .withFetchOption(Option.RETURN_BODY, true)
        .withUpdate(Update.noopUpdate())
        .build();
UpdateValue.Response response = client.execute(updateOp);
RiakObject object = response.getValue(RiakObject.class);

// Or to continue the User example from above:
User user = response.getValue(User.class);
```

In general, you should use no-operation updates only on keys that you
suspect may have accumulated siblings or on keys that are frequently
updated (and thus bear the possibility of accumulating siblings).
Otherwise, you're better off performing normal reads.

