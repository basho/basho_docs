---
title: "Taste of Riak: Querying with PHP"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search, php]
---

####A Quick Note on Querying and Schemas
_Schemas_? Yes I said that correctly, S-C-H-E-M-A-S. It's not a dirty word.  
Even with a Key/Value store, you will still have a logical database schema of how all the data relates to one another. This can be as simple as using the same key across multiple buckets for different types of data, to having fields in your data that are related by name.  These querying methods will introduce you to some ways of laying out your data in Riak, along with how to query it back.

###Denormalization

If you're coming from a relational database, the easiest way to get your application's feet wet with NoSQL is to denormalize your data into related chunks.  For example with a customer database, you might have separate tables for Customers, Addresses, Preferences, etc.  In Riak, you can denormalize all that associated data into a single object and store it into a `Customer` bucket.  You can keep pulling in associated data until you hit one of the big denormalization walls:

* Size Limits (objects greater than 1MB)
* Shared/Referential Data (data that the object doesn't "own")
* Differences in Access Patterns (objects that get read/written once vs. often)

At one of these points we will have to split the model.

###Same Keys - Different Buckets

The simplest way to split up data would be to use the same identity key across different buckets. A good example of this would be a `Customer` object, an `Order` object, and an `OrderSummaries` object that keeps rolled up info about orders such as Total, etc. Let's put some data into Riak so we can play with it.

```php
<?php 
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

// Class definitions for our models
class Customer {
    var $customerId;
    var $name;
    var $address;
    var $city;
    var $state;
    var $zip;
    var $phone;
    var $createdDate;
}

class Order {
    public function __construct() {
        $this->items = array();
    }
    var $orderId;
    var $customerId;
    var $salespersonId;
    var $items;
    var $total;
    var $orderDate;
}

class Item {
    public function __construct($itemId, $title, $price) {
        $this->itemId = $itemId;
        $this->title = $title;
        $this->price = $price;
    }
    var $itemId;
    var $title;
    var $price;
}

class OrderSummary {
    public function __construct() {
        $this->summaries = array();
    }
    var $customerId;
    var $summaries;
}

class OrderSummaryItem {
    public function __construct(Order $order) {
        $this->orderId = $order->orderId;
        $this->total = $order->total;
        $this->orderDate = $order->orderDate;
    }
    var $orderId;
    var $total;
    var $orderDate;
}


// Creating Data
$customer = new Customer();
$customer->customerId = 1;
$customer->name = 'John Smith';
$customer->address = '123 Main Street';
$customer->city = 'Columbus';
$customer->state = 'Ohio';
$customer->zip = '43210';
$customer->phone = '+1-614-555-5555';
$customer->createdDate = '2013-10-01 14:30:26';


$orders = array();

$order1 = new Order();
$order1->orderId = 1;
$order1->customerId = 1;
$order1->salespersonId = 9000;
$order1->items = array(
        new Item('TCV37GIT4NJ',
                'USB 3.0 Coffee Warmer',
                15.99),
        new Item('PEG10BBF2PP', 
                 'eTablet Pro; 24GB; Grey', 
                 399.99));
$order1->total = 415.98;
$order1->orderDate = '2013-10-01 14:42:26';
$orders[] = $order1;

$order2 = new Order();
$order2->orderId = 2;
$order2->customerId = 1;
$order2->salespersonId = 9001;
$order2->items = array(
        new Item('OAX19XWN0QP',
                'GoSlo Digital Camera',
                359.99));
$order2->total = 359.99;
$order2->orderDate = '2013-10-15 16:43:16';
$orders[] = $order2;

$order3 = new Order();
$order3->orderId = 3;
$order3->customerId = 1;
$order3->salespersonId = 9000;
$order3->items = array(
        new Item('WYK12EPU5EZ',
                'Call of Battle = Goats - Gamesphere 4',
                69.99),
        new Item('TJB84HAA8OA',
                'Bricko Building Blocks',
                4.99));
$order3->total = 74.98;
$order3->orderDate = '2013-11-03 17:45:28';
$orders[] = $order3;


$orderSummary = new OrderSummary();
$orderSummary->customerId = 1;
foreach ($orders as $order) {
    $orderSummary->summaries[] = new OrderSummaryItem($order);
}
unset($order);


// Starting Client
$client = new Basho\Riak\Riak('127.0.0.1', 10018);

// Creating Buckets
$customersBucket = $client->bucket('Customers');
$ordersBucket = $client->bucket('Orders');
$orderSummariesBucket = $client->bucket('OrderSummaries');


// Storing Data
$customer_riak = $customersBucket->newObject(strval($customer->customerId), $customer);
$customer_riak->store();

foreach ($orders as $order) {
    $order_riak = $ordersBucket->newObject(strval($order->orderId), $order);
    $order_riak->store();
}
unset($order);

$order_summary_riak = $orderSummariesBucket->newObject(strval($orderSummary->customerId), $orderSummary);
$order_summary_riak->store();
```

 While individual `Customer` and `Order` objects don't change much (or shouldn't change), the `Order Summaries` object will likely change often.  It will do double duty by acting as an index for all a customer's orders, and also holding some relevant data such as the order total, etc.  If we showed this information in our application often, it's only one extra request to get all the info. 

```php
$fetched_customer = $customersBucket->get('1')->data;
$fetched_customer['orderSummary'] = $orderSummariesBucket->get('1')->data;
print("Customer with OrderSummary data: \n");
print_r($fetched_customer);
```

Which returns our amalgamated objects:

```php
Customer with OrderSummary data:
Array
(
    [customerId] => 1
    [name] => John Smith
    [address] => 123 Main Street
    [city] => Columbus
    [state] => Ohio
    [zip] => 43210
    [phone] => +1-614-555-5555
    [createdDate] => 2013-10-01 14:30:26
    [orderSummary] => Array
        (
            [customerId] => 1
            [summaries] => Array
                (
                    [0] => Array
                        (
                            [orderId] => 1
                            [total] => 415.98
                            [orderDate] => 2013-10-01 14:42:26
                        )

                    [1] => Array
                        (
                            [orderId] => 2
                            [total] => 359.99
                            [orderDate] => 2013-10-15 16:43:16
                        )

                    [2] => Array
                        (
                            [orderId] => 3
                            [total] => 74.98
                            [orderDate] => 2013-11-03 17:45:28
                        )

                )

        )

)
```

While this pattern is very easy and extremely fast with respect to queries and complexity, it's up to the application to know about these intrinsic relationships.  


###Secondary Indexes

If you're coming from a SQL world, Secondary Indexes (2i) are a lot like SQL indexes.  They are a way to quickly lookup objects based on a secondary key, without scanning through the whole dataset.  This makes it very easy to find groups of related data by values, or even ranges of values.  To properly show this off, we will now add some more data to our application, and add some secondary index entries at the same time.

```php
// Adding Index Data
$keys = array(1,2,3);
foreach($keys as $key) {
    $order = $ordersBucket->get(strval($key));
    $salespersonId = $order->data['salespersonId'];
    $orderDate = $order->data['orderDate'];
    $order->addIndex('SalespersonId','int',$salespersonId);
    $order->addIndex('OrderDate','bin',$orderDate);
    $order->store();
}
unset($key);
```

As you may have noticed, ordinary Key/Value data is opaque to 2i, so we have to add entries to the indexes at the application level. 
Now let's find all of Jane Appleseed's processed orders, we'll look up the orders by searching the `saleperson_id_int` index for Jane's id of `9000`.

```php
// Query for orders where the SalespersonId int index is set to 9000
$janes_orders = $ordersBucket->indexSearch('SalespersonId', 'int', 9000);
print("\n\nJane's Orders: \n");
print_r($janes_orders);
```

Which returns:

```php
Jane's Orders:
Array
(
    [0] => Basho\Riak\Link Object
        (
            [bucket] => Orders
            [key] => 1
            [tag] =>
            [client] => Basho\Riak\Riak Object
                (
                    [host] => 127.0.0.1
                    [port] => 10018
                    [prefix] => riak
                    [mapred_prefix] => mapred
                    [indexPrefix] => buckets
                    [clientid] => php_xrqqok
                    [r] => 2
                    [w] => 2
                    [dw] => 2
                )

        )

    [1] => Basho\Riak\Link Object
        (
            [bucket] => Orders
            [key] => 3
            [tag] =>
            [client] => Basho\Riak\Riak Object
                (
                    [host] => 127.0.0.1
                    [port] => 10018
                    [prefix] => riak
                    [mapred_prefix] => mapred
                    [indexPrefix] => buckets
                    [clientid] => php_xrqqok
                    [r] => 2
                    [w] => 2
                    [dw] => 2
                )

        )

)
```

Jane processed orders 1 and 3.  We used an "integer" index to reference Jane's id, next let's use a "binary" index.
Now, let's say that the VP of Sales wants to know how many orders came in during October 2013.  In this case, we can exploit 2i's range queries.  Let's search the `order_date_bin` index for entries between `20131001` and `20131031`.  

```php
// Query for orders where the OrderDate bin index is between 2013-10-01 and 2013-10-31
$october_orders = $ordersBucket->indexSearch('OrderDate', 'bin', '2013-10-01', '2013-10-31');
print("\n\nOctober's Orders: \n");
print_r($october_orders);

?>
```

Which returns:

```php
October's Orders:
Array
(
    [0] => Basho\Riak\Link Object
        (
            [bucket] => Orders
            [key] => 1
            [tag] =>
            [client] => Basho\Riak\Riak Object
                (
                    [host] => 127.0.0.1
                    [port] => 10018
                    [prefix] => riak
                    [mapred_prefix] => mapred
                    [indexPrefix] => buckets
                    [clientid] => php_xrqqok
                    [r] => 2
                    [w] => 2
                    [dw] => 2
                )

        )

    [1] => Basho\Riak\Link Object
        (
            [bucket] => Orders
            [key] => 2
            [tag] =>
            [client] => Basho\Riak\Riak Object
                (
                    [host] => 127.0.0.1
                    [port] => 10018
                    [prefix] => riak
                    [mapred_prefix] => mapred
                    [indexPrefix] => buckets
                    [clientid] => php_xrqqok
                    [r] => 2
                    [w] => 2
                    [dw] => 2
                )

        )

)
```

Boom, easy-peasy.  We used 2i's range feature to search for a range of values, and demonstrated binary indexes.  

So to recap:

* You can use Secondary Indexes to quickly lookup an object based on a secondary id other than the object's key. 
* Indexes can have either Integer or Binary(String) keys
* You can search for specific values, or a range of values
* Riak will return a list of keys that match the index query


