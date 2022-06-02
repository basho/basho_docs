---
title: "Updating Objects"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Updating Objects"
    identifier: "usage_updating_objects"
    weight: 102
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.9.2/dev/using/updates
  - /riak/kv/2.9.2/dev/using/updates
---

[glossary vnode]: {{<baseurl>}}riak/kv/2.9.2/learn/glossary/#vnode

## Using Causal Context

If an object already exists under a certain key and you want to write a
new object to that key, Riak needs to know what to do, especially if
multiple writes are happening at the same time. Which of the objects
being written should be deemed correct? These kinds of scenarios can
arise quite frequently in distributed, [eventually consistent]({{<baseurl>}}riak/kv/2.9.2/learn/concepts/eventual-consistency) systems.

Riak decides which object to choose in case of conflict using [causal context]({{<baseurl>}}riak/kv/2.9.2/learn/concepts/causal-context). These objects track the causal history of objects.
They are attached to _all_ Riak objects as metadata, and they are not
readable by humans. They may sound complex---and they are fairly complex
behind the scenes---but using them in your application is very simple.

Using causal context in an update would involve the following steps;

1. Fetch the object
2. Modify the object's value (without modifying the fetched [context object]({{<baseurl>}}riak/kv/2.9.2/learn/concepts/causal-context)
3. Write the new object to Riak

Step 2 is the most important here. All of Basho's official Riak clients
enable you to modify an object's value without modifying its [causal context]({{<baseurl>}}riak/kv/2.9.2/learn/concepts/causal-context). Although a more detailed tutorial on context objects and
object updates can be found in [Conflict Resolution]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution), we'll walk you
through a basic example here.

Let's say that the current NBA champion is the Washington Generals.
We've stored that data in Riak under the key `champion` in the bucket
`nba`, which bears the bucket type `sports`. The value of the object is
a simple text snippet that says `Washington Generals`.

But one day the Harlem Globetrotters enter the league and dethrone the
hapless Generals (forever, as it turns out). Because we want our Riak
database to reflect this new development in the league, we want to make
a new write to the `champion` key. Let's read the object stored there
and modify the value.

```java
Location currentChampion = new Location(new Namespace("sports", "nba"), "champion");
FetchValue fetch = new FetchValue.Builder(currentChampion)
        .build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
obj.setValue(BinaryValue.create("Harlem Globetrotters"))
```

```ruby
bucket = client.bucket_type('sports').bucket('nba')
obj = bucket.get('champion')
obj.raw_data = 'Harlem Globetrotters'
obj.store
```

```php
$location = new \Basho\Riak\Location('champion', new \Basho\Riak\Bucket('nba', 'sports'));
$object = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->withLocation($location)
  ->build()
  ->execute()
  ->getObject();

$object->setData('Harlem Globetrotters');

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withLocation($location)
  ->withObject($object)
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('sports').bucket('nba')
obj = bucket.get('champion')
obj.data = 'Harlem Globetrotters'
```

```csharp
var id = new RiakObjectId("sports", "nba", "champion");
var obj = new RiakObject(id, "Washington Generals",
    RiakConstants.ContentTypes.TextPlain);
var rslt = client.Put(obj);

rslt = client.Get(id);
obj = rslt.Value;
obj.SetObject("Harlem Globetrotters",
    RiakConstants.ContentTypes.TextPlain);
rslt = client.Put(obj);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('Washington Generals');

var options = {
    bucketType: 'sports', bucket: 'nba', key: 'champion',
    value: riakObj
};
client.storeValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    delete options.value;
    client.fetchValue(options, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
        var fetchedObj = rslt.values.shift();
        fetchedObj.setValue('Harlem Globetrotters');
        options.value = fetchedObj;
        options.returnBody = true;
        client.storeValue(options, function (err, rslt) {
            if (err) {
                throw new Error(err);
            }
            var updatedObj = rslt.values.shift();
            logger.info("champion: %s", updatedObj.value.toString('utf8'));
        });
    });
});
```

```erlang
%% In the Erlang client, you cannot view a context objectdirectly, but it
%% will be included in the output when you fetch an object:

{ok, Obj} = riakc_pb_socket:get(Pid,
                                {<<"sports">>, <<"nba">>},
                                <<"champion">>),
UpdatedObj = riakc_obj:update_value(Obj, <<"Harlem Globetrotters">>),
{ok, NewestObj} = riakc_pb_socket:put(Pid, UpdatedObj, [return_body]).
```

```golang
obj := &riak.Object{
    ContentType:     "text/plain",
    Charset:         "utf-8",
    ContentEncoding: "utf-8",
    Value:           []byte("Washington Generals"),
}

cmd, err := riak.NewStoreValueCommandBuilder().
    WithBucketType("sports").
    WithBucket("nba").
    WithKey("champion").
    WithContent(obj).
    WithReturnBody(true).
    Build()

if err != nil {
    fmt.Println(err.Error())
    return
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println(err.Error())
    return
}

svc := cmd.(*riak.StoreValueCommand)
rsp := svc.Response
obj = rsp.Values[0]
obj.Value = []byte("Harlem Globetrotters")

cmd, err = riak.NewStoreValueCommandBuilder().
    WithBucketType("sports").
    WithBucket("nba").
    WithKey("champion").
    WithContent(obj).
    WithReturnBody(true).
    Build()

if err != nil {
    fmt.Println(err.Error())
    return
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println(err.Error())
    return
}

svc = cmd.(*riak.StoreValueCommand)
rsp = svc.Response
obj = rsp.Values[0]
fmt.Printf("champion: %v", string(obj.Value))
```

```curl
# When using curl, the context object is attached to the X-Riak-Vclock header

curl -i http://localhost:8098/types/sports/buckets/nba/keys/champion

# In the resulting output, the header will look something like this:

X-Riak-Vclock: a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=

# When performing a write to the same key, that same header needs to
# accompany the write for Riak to be able to use the context object
```

In the samples above, we didn't need to actually interact with the
context object, as retaining and passing along the context object was
accomplished automatically by the client. If, however, you do need
access to an object's context, the clients enable you to fetch it from
the object:

```java
// Using the RiakObject obj from above:

Vclock vClock = obj.getVclock();
System.out.println(vClock.asString());

// The context object will look something like this:
// a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```ruby
# Using the RObject obj from above:

obj.vclock

# The context object will look something like this:
# a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```php
# Using the RObject obj from above:

echo $object->getVclock(); // a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```python
# Using the RiakObject obj from above:

obj.vclock

# The context object will look something like this:
# a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```csharp
// Using the RiakObject obj from above:
var vclock = result.Value.VectorClock;
Console.WriteLine(Convert.ToBase64String(vclock));

// The output will look something like this:
// a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```javascript
// Using the RiakObject fetchedObj from above:
var fetchedObj = rslt.values.shift();
logger.info("vclock: %s", fetchedObj.getVClock().toString('base64'));

// The output will look something like this:
// vclock: a85hYGBgymDKBVIcR4M2cov1HeHKYEpkymNlsE2cfo4PKjXXjuOU+FHdWqAUM1CqECSVBQA=
```

```erlang
%% Using the Obj object from above:

riakc_obj:vclock(Obj).

%% The context object will look something like this in the Erlang shell:
%% <<107,206,97,96,96,96,204,96,226,82,28,202,156,255,126,
%% 6,175,157,255,57,131,41,145,49,143,149,225,240,...>>
```

```golang
svc := cmd.(*riak.StoreValueCommand)
rsp := svc.Response
fmt.Println(rsp.VClock)

// Output:
// X3hNXFq3ythUqvvrG9eJEGbUyLS
```

## The Object Update Cycle

If you decide that your application requires mutable data in Riak, we
recommend that you:

* avoid high-frequency object updates to the same key (i.e. multiple
  updates per second for long periods of time), as this will degrade
  Riak performance; and that you
* follow a read-modify-write cycle when performing updates.

That cycle looks something like this:

1. **Read** the object from Riak. This step is important for updates
because this enables you to fetch the object's [causal context]({{<baseurl>}}riak/kv/2.9.2/learn/concepts/causal-context), which
is the information that Riak uses to make decisions about which object
values are most recent (this is especially useful for objects that are
frequently updated). This context object needs to be passed back to Riak
when you update the object. This step is handled for you by Basho's
client libraries as long as you perform a read prior to an update. In
addition, if you have chosen to allow Riak to generate
[siblings]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution/#siblings) \(which we recommend), you
should **resolve sibling conflicts** upon read if they exist. For more
on this, please see our documentation on [conflict resolution]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution), along
with examples from our official client libraries:
  * [Java]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution/java)
  * [Ruby]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution/ruby)
  * [Python]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution/python)
  * [C#]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution/csharp)
  * [Go]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution/golang)
2. **Modify the object** on the application side.
3. **Write** the new, modified object to Riak. Because you read the
object first, Riak will receive the object's causal context metadata.
Remember that this happens automatically.

In general, you should read an object before modifying it. Think of it
as performing a `GET` prior to any `PUT` when interacting with a REST
API.

> **Note on strong consistency**
>
> If you are using Riak's [strong consistency]({{<baseurl>}}riak/kv/2.9.2/developing/app-guide/strong-consistency/) feature, it is not only desirable but also necessary to use the read/modify/write cycle explained in the section above. If you attempt to update an object without fetching the object first, your update operation will necessarily fail. More information can be found in the
[strong consistency documentation]({{<baseurl>}}riak/kv/2.9.2/developing/app-guide/strong-consistency/#strongly-consistent-writes).

### Updating Deleted Objects

You should use the read-modify-write cycle explained above at all times,
_even if you're updating deleted objects_. The reasons for that can be
found in our documentation on [tombstones]({{<baseurl>}}riak/kv/2.9.2/using/reference/object-deletion/#tombstones).

There are some modifications that you may need to make if you are
updating objects that may have been deleted previously. If you are using
the Java client, an explanation and examples are given in the
[Java-specific section below](#java-client-example). If
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
Python, .NET, Node.js, Erlang and Go clients. Because updates with the official
Java client functions somewhat differently, those examples can be found in the
[section below](#java-client-example).

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

```php
$location = new \Basho\Riak\Location('seahawks', new \Basho\Riak\Bucket('coaches', 'siblings'));
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->atLocation($location)
  ->build()
  ->execute();

if ($response->isSuccess()) {
  $object = $response->getObject();
  $object->setData('Pete Carroll');
} else {
  $object = new \Basho\Riak\Object('Pete Carroll', 'text/plain');
}

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withObject($object)
  ->atLocation($location)
  ->build()
  ->execute();
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

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setBucketType('siblings');
riakObj.setBucket('coaches');
riakObj.setKey('seahawks');
riakObj.setValue('Pete Carroll');
client.storeValue({ value: riakObj }, function (err, rslt) {
    if (err) {
        throw new Error(err);
    } else {
        logger.info('Stored Pete Carroll');
    }
});
```

```erlang
Obj = riakc_obj:new({<<"siblings">>, <<"coaches">>},
                     <<"seahawks">>,
                     <<"Pete Carroll">>,
                     <<"text/plain">>).
riakc_pb_socket:put(Pid, Obj).
```

```golang
obj := &riak.Object{
    ContentType:     "text/plain",
    Charset:         "utf-8",
    ContentEncoding: "utf-8",
    Value:           []byte("Pete Carroll"),
}

cmd, err := riak.NewStoreValueCommandBuilder().
    WithBucketType("siblings").
    WithBucket("coaches").
    WithKey("seahawks").
    WithContent(obj).
    Build()

if err != nil {
    fmt.Println(err.Error())
    return
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println(err.Error())
    return
}

fmt.Println("Stored Pete Carroll")
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

```php
function update_coach($team, $coach) {
  $location = new \Basho\Riak\Location('seahawks', new \Basho\Riak\Bucket('coaches', 'siblings'));
  $response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
    ->atLocation($location)
    ->build()
    ->execute();

  if ($response->isSuccess()) {
    $object = $response->getObject();
    $object->setData('Pete Carroll');
  } else {
    $object = new \Basho\Riak\Object('Pete Carroll', 'text/plain');
  }

  $response = (new \Basho\Riak\Command\Builder\StoreObject($riak))
    ->withObject($object)
    ->atLocation($location)
    ->build()
    ->execute();

  return $response->isSuccess();
}

echo update_coach('packers', 'Vince Lombardi'); // true
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

```javascript
function update_coach(team, newCoach) {
    client.fetchValue({
        bucketType: 'siblings', bucket: 'coaches', key: team
    }, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }

        var riakObj = rslt.values.shift();
        riakObj.setValue(newCoach);
        client.storeValue({ value: riakObj }, function (err, rslt) {
            if (err) {
                throw new Error(err);
            }
        });
    });
}
```

```erlang
update_coach(team, new_coach) ->
    {ok, Obj} = riakc_pb_socket:get(Pid,
                                    {<<"siblings">>, <<"coaches">>},
                                    <<team>>),
    ModifiedObj = riakc_obj:update_value(Obj, <<new_coach>>),
    riakc_pb_socket:put(Pid, ModifiedObj).

%% Example usage
update_coach('packers', 'Vince Lombardi')
```

```golang
func updateCoach(cluster *riak.Cluster, team, newCoach string) error {
    var cmd riak.Command
    var err error

    cmd, err = riak.NewFetchValueCommandBuilder().
        WithBucketType("siblings").
        WithBucket("coaches").
        WithKey(team).
        Build()

    if err != nil {
        return err
    }

    if err := cluster.Execute(cmd); err != nil {
        return err
    }

    fvc := cmd.(*riak.FetchValueCommand)
    obj := fvc.Response.Values[0]
    obj.Value = []byte(newCoach)

    cmd, err = riak.NewStoreValueCommandBuilder().
        WithBucketType("siblings").
        WithBucket("coaches").
        WithKey(team).
        WithContent(obj).
        Build()

    if err != nil {
        return err
    }

    if err := cluster.Execute(cmd); err != nil {
        return err
    }

    return nil
}
```

In the example above, you can see the three steps in action: first, the
object is read, which automatically fetches the object's causal context;
then the object is modified, i.e. the object's value is set to the name
of the new coach; and finally the object is written back to Riak.

## Object Update Anti-patterns

The most important thing to bear in mind when updating objects is this:
you should always read an object prior to updating it _unless_ you are
certain that no object is stored there. If you are storing [sensor data]({{<baseurl>}}riak/kv/2.9.2/developing/data-modeling/#sensor-data) in Riak and using timestamps as keys, for example, then you can be sure that keys are not repeated. In that case, making writes to Riak without first reading the object is fine. If
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
useful because they can help Riak resolve [sibling conflicts]({{<baseurl>}}riak/kv/2.9.2/developing/usage/conflict-resolution#siblings). If you have an object---or many objects, for that
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
