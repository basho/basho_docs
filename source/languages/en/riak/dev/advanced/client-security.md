---
title: Client-Side Security
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, certificate]
---

If you have enabled [[Riak Security|Authentication and Authorization]] and chosen `certificate` as your [[security source|Managing Security Sources]], any client attempting to connect to Riak will have to authenticate itself using a signed SSL certificate that matches the certificate on your Riak server.

This tutorial will show you to use certificate-based authentication for each of Riak's officially supported [[client libraries]]. A guide to server-side certificate management can be found in our documentation on [[security source management|Managing Security Sources#certificate-based-authentication]].

## Initial Setup

Let's say that (a) your server-side SSL certificates have already been created and signed and that your Riak servers have been configured to recognize those certificates, (b) you have copies of those certificates in an `/ssl` directory that is accessible to your Riak client, and (c) your client's username is `riakuser` and the password is `rosebud`.

This tutorial will not cover the basics of generating certificates, and will assume that your signing authority and cert correspond to the files specified in your `riak.conf` [[configuration file|Configuration Files]] under `ssl.cacertfile` and `ssl.certfile`, respectively. For this example, we'll use the default names for those files: `cacertfile.pem` and `cert.pem`.

You will need to point your client to those certificates upon instantiation. Let's say that you're instantiating a client for a one-node cluster, using the host `127.0.0.1` and port `10017`.

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 10017,
  authentication: {
    user: 'riakuser',
    password: 'rosebud',
    server_ca: File.read('/path/to/ssl/cacertfile.pem'),
    server_cert: File.read('/path/to/ssl/cert.pem')
  }
)
```

```java
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

// Specify the location of the CA and create an input stream:
File cacert = new File("/path/to/ssl/cacertfile.pem");
FileInputStream inputStream = new FileInputStream(cacert);

// Create an X509Certificate object using the input stream:
CertificateFactory cFactory = CertificateFactory.getInstance("X.509");
X509Certificate caCert = (X509Certificate) cFactory.generateCertificate(inputStream);
inputStream.close();

// Identify a key store to be used during the authentication phase:
KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
ks.load(null, "rosebud".toCharArray());
ks.setCertificateEntry("cert.pem", caCert);

// You can then create a node using your username, password,
// and KeyStore object:
RiakNode node = new RiakNode.Builder()
        .withAuth("riakuser", "rosebud", ks)
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(10017);
```


```erlang
CertDir = "/path/to/ssl",
{ok, Pid} = riakc_pb_socket:start(
    "127.0.0.1", 10017,
    [{credentials, "riakuser", "rosebud"},
     {cacertfile, filename:join([CertDir, "certfile.pem"])},
     {certfile, filename:join([CertDir, "cert.pem"])}
    ]).
```

## Connecting

Once your client has been set up, you can verify that the security setup is working by performing a basic PUT operation followed by a GET:

```ruby
client = Riak::Client.new(host: '127.0.0.1', pb_port: 10017)
bucket = client.bucket('certificate_test')
obj = Riak::RObject.new(bucket, 'test_key')
obj.content_type = 'text/plain'
obj.raw_data = 'SSL now works!'
obj.store
bucket.get('test_key').data
```

```java
RiakNode node1 = new RiakNode.Build()
        .withAuth("riakuser", "rosebud", ks)
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(10017);
RiakCluster cluster = new RiakCluster.Builder(node1).build();
RiakClient client = new RiakClient(cluster);
RiakObject testObject = new RiakObject()
        .setContentType("text/plain")
        .setValue(BinaryValue.create("SSL now works!"));
Location loc = new Location("certificate_test").setKey("test_key")
StoreValue store = new StoreValue.Builder(testObject)
        .withLocation(loc);
client.execute(store);
FetchValue fetch = new FetchValue.Builder(loc).build();
FetchValue.Response res = client.execute(fetch);
RiakObject fetchedObject = res.getValue(RiakObject.class);
System.out.println(fetchObject.getValue().toString());
```

```erlang
%% Using the Pid variable from the code sample above:
Object = riakc_obj:new(<<"certificate_test">>,
                       <<"test_key">>,
                       <<"SSL now works!">>,
                       <<"text/plain">>),
riakc_pb_socket:put(Pid, Object).

{ok, FetchedObject} = riakc_pb_socket:get(Pid,
                                          <<"certificate_test">>,
                                          <<"test_key">>).
```

If you get the `SSL now works!` as a response, then your client-side authentication setup should be working normally.

## Resources

* [OpenSSL tutorial](http://www.madboa.com/geek/openssl/)