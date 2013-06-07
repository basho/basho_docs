---
title: Taste of Riak - PHP Flavor
project: riak
document: guide
toc: true
audience: beginner
keywords: [developers, client, php]
---

If you haven't setup a Riak Node and started it, please visit the [[Prerequisites|Taste of Riak Prerequisites]] first.

To try this flavor of Riak, a working installation of PHP is required. 

###Client Setup
Download the latest PHP client from GitHub ([zip](https://github.com/basho/riak-php-client/archive/master.zip), [github repository](https://github.com/basho/riak-php-client/)).

Unzip the archive to your working directory and then start an interactive PHP shell (`php -a`) next to the client directory.  

Next, enter the following into the shell to load the client library and start a client instance. 

```php
require_once('riak-php-client/src/Basho/Riak/Riak.php');
require_once('riak-php-client/src/Basho/Riak/Bucket.php');
require_once('riak-php-client/src/Basho/Riak/Exception.php');
require_once('riak-php-client/src/Basho/Riak/Link.php');
require_once('riak-php-client/src/Basho/Riak/MapReduce.php');
require_once('riak-php-client/src/Basho/Riak/Object.php');
require_once('riak-php-client/src/Basho/Riak/StringIO.php');
require_once('riak-php-client/src/Basho/Riak/Utils.php');
require_once('riak-php-client/src/Basho/Riak/Link/Phase.php');
require_once('riak-php-client/src/Basho/Riak/MapReduce/Phase.php');

$client = new Basho\Riak\Riak('127.0.0.1', 8098);
```

We are now ready to start interacting with Riak.

###Creating Objects In Riak
First, let’s create a few objects and a bucket to keep them in.

```php
$myBucket = $client->bucket('test');

$val1 = 1;
$obj1 = $myBucket->newObject('one', $val1);
$obj1->store();
```

In this first example we have stored the integer 1 with the lookup key of ‘one’.  Next let’s store a simple string value of “two” with a matching key.

```php
$val2 = 'two';
$obj2 = $myBucket->newObject('two', $val2);
$obj2->store();
```

That was easy.  Finally, let’s store an associative array.  You will probably recognize the pattern by now.

```php
$val3 = array('myValue' => 3);
$obj3 = $myBucket->newObject('three', $val3);
$obj3->store();
```

###Reading Objects From Riak
Now that we have a few objects stored, let’s retrieve them and make sure they contain the values we expect.

```php
$fetched1 = $myBucket->get('one');
$fetched2 = $myBucket->get('two');
$fetched3 = $myBucket->get('three');

assert($val1 == $fetched1->getData());
assert($val2 == $fetched2->getData());
assert($val3 == $fetched3->getData());
```

That was easy.  We simply request the objects by bucket and key. 


###Updating Objects In Riak
While some data may be static, other forms of data may need to be updated.  This is also easy to accomplish.  Let’s update the value of myValue in the 3rd example to 42.

```php
$fetched3->data['myValue'] = 42;
$fetched3->store();
```

###Deleting Objects From Riak
As a last step, we’ll demonstrate how to delete data.  We just call the delete() method on the fetched Riak Object.  

```php
$fetched1->delete();
$fetched2->delete();
$fetched3->delete();
```

###Working With Complex Objects
Since the world is a little more complicated than simple integers and bits of strings, let’s see how we can work with more complex objects.  Take for example, this plain old PHP object(POPO) that encapsulates some knowledge about a book.

```php
class Book {
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
$booksBucket = $client->bucket('books');
$newBook = $booksBucket->newObject($book->isbn, $book);
$newBook->store();
```

Some of you may be thinking “But how does the Riak client encode/decode my object”?  If we fetch the binary version of our book back and print it as a string, we shall know:

```php
$riakObject = $booksBucket->getBinary($book->isbn);
print($riakObject->data);
```

```json
{"title":"Moby Dick",
 "author":"Herman Melville",
 "body":"Call me Ishmael. Some years ago...",
 "isbn":"1111979723",
 "copiesOwned":3}
```

JSON!  The library encodes POPO’s as JSON strings.  If instead we wanted to get a data record back we could use `$mobyDick = $booksBucket->get(book.ISBN)->data`, and then use an array accessor like `$mobyDick[‘isbn’]` to grab the info we want.  

Now that we’ve ruined the magic of object encoding, let’s clean up our mess:

```php
$newBook->delete();
```

###Next Steps
More complex use cases can be composed from these initial create, read, update, and delete (CRUD) operations. In the next chapter we will look at how to store and query more complicated and interconnected data, such as documents.  


