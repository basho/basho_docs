---
title_supertext: "Getting Started:"
title: "CRUD Operations with PHP"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "CRUD Operations"
    identifier: "getting_started_php_crud"
    weight: 100
    parent: "getting_started_php"
toc: true
---

## Creating Objects In Riak
First, let’s create a few objects and a bucket to keep them in.

```php
$bucket = new Riak\Bucket('testBucket');

$val1 = 1;
$location1 = new Riak\Location('one', $bucket);

$storeCommand1 = (new Command\Builder\StoreObject($riak))
                    ->buildObject($val1)
                    ->atLocation($location1)
                    ->build();
$storeCommand1->execute();
```

In this first example we have stored the integer 1 with the lookup key of ‘one’.  Next let’s store a simple string value of “two” with a matching key.

```php
$val2 = 'two';
$location2 = new Riak\Location('two', $bucket);

$storeCommand2 = (new Command\Builder\StoreObject($riak))
                    ->buildObject($val2)
                    ->atLocation($location2)
                    ->build();
$storeCommand2->execute();
```

That was easy.  Finally, let’s store an associative array.  You will probably recognize the pattern by now.

```php
$val3 = ['myValue' => 3];
$location3 = new Riak\Location('three', $bucket);

$storeCommand3 = (new Command\Builder\StoreObject($riak))
                    ->buildJsonObject($val3)
                    ->atLocation($location3)
                    ->build();
$storeCommand3->execute();
```

## Reading Objects From Riak
Now that we have a few objects stored, let’s retrieve them and make sure they contain the values we expect.

```php
$response1 = (new Command\Builder\FetchObject($riak))
                ->atLocation($location1)
                ->build()
                ->execute();

$response2 = (new Command\Builder\FetchObject($riak))
                ->atLocation($location2)
                ->build()
                ->execute();

$response3 = (new Command\Builder\FetchObject($riak))
                ->atLocation($location3)
                ->withDecodeAsAssociative()
                ->build()
                ->execute();

assert($val1 == $response1->getObject()->getData());
assert($val2 == $response2->getObject()->getData());
assert($val3 == $response3->getObject()->getData());
```

That was easy.  We create a [Fetch Command](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Object.Fetch.html) from a [FetchObject Builder](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Builder.FetchObject.html). 
For our object that is an associative array, we also add [`withDecodeAsAssociative()`](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Builder.FetchObject.html#_withDecodeAsAssociative) to the builder so it returns the object as an associative array instead of an stdClass object.

In either case, we'll get a [Response](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Object.Response.html) object back, which holds information about the operation, and the result data.

## Updating Objects In Riak
While some data may be static, other forms of data may need to be updated.  This is also easy to accomplish.  Let’s update the value of myValue in the 3rd example to 42.

```php
$object3 = $response3->getObject();
$data3 = $object3->getData();

$data3['myValue'] = 42;
$object3 = $object3->setData(json_encode($data3));

$updateCommand = (new Command\Builder\StoreObject($riak))
    ->withObject($object3)
    ->atLocation($location3)
    ->build();

$updateCommand->execute();
```

First we get the Riak [Object](http://basho.github.io/riak-php-client/class-Basho.Riak.Object.html) from the [Response](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Object.Response.html), then we get the stored data with [`getData()`](http://basho.github.io/riak-php-client/class-Basho.Riak.Object.html#_getData). We update the data to our liking, then use [`setData()`](http://basho.github.io/riak-php-client/class-Basho.Riak.Object.html#_setData) to set the new data back to the Riak Object. 
To store it we use the same pattern as before, but this time we use the [`withObject()`](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Builder.ObjectTrait.html#_withObject) method to tell it to store our updated Riak Object.

## Deleting Objects From Riak
As a last step, we’ll demonstrate how to delete data.  We just build a [Delete Command](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Object.Delete.html) from a [DeleteObject Builder](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Builder.DeleteObject.html), and execute it.  

```php
(new Command\Builder\DeleteObject($riak))->atLocation($location1)->build()->execute();
(new Command\Builder\DeleteObject($riak))->atLocation($location2)->build()->execute();
(new Command\Builder\DeleteObject($riak))->atLocation($location3)->build()->execute();
```

### Working With Complex Objects
Since the world is a little more complicated than simple integers and bits of strings, let’s see how we can work with more complex objects.  Take for example, this plain old PHP object(POPO) that encapsulates some knowledge about a book.

```php
class Book
{
    var $title;
    var $author;
    var $body;
    var $isbn;
    var $copiesOwned;
}

$book = new Book();
$book->isbn = '1111979723';
$book->title = 'Moby Dick';
$book->author = 'Herman Melville';
$book->body = 'Call me Ishmael. Some years ago...';
$book->copiesOwned = 3;
```

Ok, so we have some information about our Moby Dick collection that we want to save.  Storing this to Riak should look familiar by now:

```php
$bookLocation = new Riak\Location($book->isbn, new Riak\Bucket('books'));

$storeCommand1 = (new Command\Builder\StoreObject($riak))
    ->buildJsonObject($book)
    ->atLocation($bookLocation)
    ->build();

$storeCommand1->execute();
```

Some of you may be thinking “But how does the Riak client encode/decode my object”?  If we fetch the binary version of our book back and print it as a string, we shall know:

```php
$fetchBookResponse = (new Command\Builder\FetchObject($riak))
                        ->atLocation($bookLocation)
                        ->build()
                        ->execute();

print('Serialized Object:' . PHP_EOL);
print($fetchBookResponse->getBody() . PHP_EOL);
```

```json
Serialized Object:
{"title":"Moby Dick","author":"Herman Melville","body":"Call me Ishmael. Some years ago...","isbn":"1111979723","copiesOwned":3}
```

JSON!  The library encodes PHP objects as JSON strings when you use the [`buildJsonObject()`](http://basho.github.io/riak-php-client/class-Basho.Riak.Command.Builder.ObjectTrait.html#_buildJsonObject) method on the StoreObject builder.  

Now that we’ve ruined the magic of object encoding, let’s clean up our mess:

```php
(new Command\Builder\DeleteObject($riak))
    ->atLocation($bookLocation)
    ->build()
    ->execute();
```

## Next Steps

More complex use cases can be composed from these initial create, read, update, and delete (CRUD) operations. [In the next chapter]({{<baseurl>}}riak/kv/2.0.6/developing/getting-started/php/querying) we will look at how to store and query more complicated and interconnected data, such as documents. 
