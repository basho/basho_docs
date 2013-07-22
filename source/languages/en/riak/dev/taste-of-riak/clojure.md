---
title: "Taste of Riak: Clojure"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, clojure]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of Java and
[Leiningen](https://github.com/technomancy/leiningen) are required.

### Client Setup

[Welle](http://clojureriak.info/) is a community-maintained Riak client
library for Clojure.

First, add Welle as a dependency to your project.

```clojure
[com.novemberain/welle "1.5.0"]
```

Start a Clojure repl with Leiningen:

```bash
$ lein repl
```

Then, enter the following into the repl:

```clojure
(ns taste-of-riak.docs.examples
  (:require [clojurewerkz.welle.core    :as wc]
            [clojurewerkz.welle.buckets :as wb]
            [clojurewerkz.welle.kv      :as kv])
  (:import com.basho.riak.client.http.util.Constants))


;; Connects to a Riak node at 127.0.0.1:8098
(wc/connect! "http://127.0.0.1:8098/riak")
```

If you set up a local Riak cluster using the [[five minute install]] method,
use this code snippet instead:

```clojure
;; Connects to a Riak node at 127.0.0.1:10018
(wc/connect! "http://127.0.0.1:10018/riak")
```

We are now ready to start interacting with Riak.

### Creating Objects in Riak

First, let’s create a few objects and a bucket to keep them in.

```clojure
(wb/create "test")
(kv/store "test" "one" 1 :content-type "application/clojure")
```

In this first example we have stored the integer 1 with the lookup key of
‘one’.  Next let’s store the string "two" as bytes with a matching key.

```clojure
(kv/store "test" "two" (.getBytes "two"))
```

That was easy.  Finally, let’s store a bit of JSON.  You will probably
recognize the pattern by now.

```clojure
(def three {:val 3})
(kv/store "test" "three" three :content-type Constants/CTYPE_JSON_UTF8)
```

### Reading Objects from Riak

Now that we have a few objects stored, let’s retrieve them and make sure they
contain the values we expect.

```clojure
(:value (first (kv/fetch "test" "one")))
; 1
(:value (first (kv/fetch "test" "one")))
(String. (:value (first (kv/fetch "test" "two"))))
; "two"
(:val (:value (first (kv/fetch "test" "three"))))
; 3
```

That was easy.  We simply request the objects by key.

### Deleting Objects from Riak

As a last step, we’ll demonstrate how to delete data.

```clojure
(kv/delete "test" "one")
```

### Working with Complex Objects

Since the world is a little more complicated than simple integers and bits of
strings, let’s see how we can work with more complex objects.  Take for
example, this map hash that encapsulates some knowledge about a book.

```clojure
(def book {:isbn "1111979723",
           :title "Moby Dick",
           :author "Herman Melville",
           :body "Call me Ishmael. Some years ago...",
           :copies_owned 3})
```

Ok, so we have some information about our Moby Dick collection that we want to
save.  Storing this to Riak should look familiar by now.

```clojure
(wb/create "books")
(kv/store "books" (:isbn book) book :content-type Constants/CTYPE_JSON_UTF8)
```

Here we serialized the Clojure map to a JSON value. If we fetch our book back,
we'll see a Clojure map again:

```clojure
(:value (first (kv/fetch "books" "1111979723")))
; {:author "Herman Melville", :title "Moby Dick", :copies_owned 3, :isbn "1111979723", :body "Call me Ishmael. Some years ago..."}
```

As you can see from our examples, Welle can serialize/deserialize values in:

* JSON
* JSON in UTF-8
* Clojure data (that can be read by the Clojure reader)
* Text
* Text in UTF-8

Finally, let’s clean up our mess:

```clojure
(kv/delete "books" "1111979723")
```
