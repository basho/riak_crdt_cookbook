# Riak CRDT Cookbook: -> Counters

Riak KV master (at the time of writing) introduces Counters as a new opaque data type, so here we're going to walk through doing a few things with them using the files in this directory.

This cookbook should take you, the reader, from starting out, not having a clue about what they are and how they work, to being able to use them fully, even in Map-Reduce tasks.

##Let's Get Setup

First off, get the riak master branch setup, with a several-node dev cluster. Follow the [Riak Fast Track: Building a Development Environment](http://docs.basho.com/riak/latest/tutorials/fast-track/Building-a-Development-Environment/), but when installing use the "Installing from GitHub" section of [Installing Riak from Source](http://docs.basho.com/riak/latest/tutorials/installation/Installing-Riak-from-Source/) then continue building your development environment. 

## Baby Steps

First off, we're going to use Curl to set some things up. For the rest of this tutorial, I'm going to use a bucket called `crdt_cookbook`, for clarity.

Quick, Important Interlude: Counters require the bucket property `allow_mult` to be true, so we'll set that first. Don't worry about this, the whole point in CRDTs is that they cope with siblings completely. (NOTE: I've pretty-printed the JSON below, in your console it probably won't look as neat)

```
riak (master) $ curl -i http://localhost:10018/buckets/crdt_cookbook/props \
 -X PUT -d '{"props":{"allow_mult":true}}' -H "Content-Type: application/json"

HTTP/1.1 204 No Content
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 15:15:55 GMT
Content-Type: application/json
Content-Length: 0

riak (master) $ curl -i http://localhost:10018/buckets/crdt_cookbook/props
HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 15:19:54 GMT
Content-Type: application/json
Content-Length: 430

{
    "props": {
        "allow_mult": true,
        //-- SNIP --//
    }
}
```

And now let's use some counters. For this, we have a special endpoint to use instead of the keys endpoint, because we model the counter object as an opaque binary object from most of KV's standpoint.

The example we're going to use is hits for pages on a website, mostly because it gives us interesting things we can do with MapReduce tasks later.

```
riak (master) $ curl -i http://localhost:10018/buckets/crdt_cookbook/counters/basho.com \
 -X POST -d "1"

HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:15:15 GMT
Content-Type: text/plain
Content-Length: 0
```

This increments the counter at `<<"basho.com">>` in our bucket by one (it will also create the counter if it doesn't yet exist). To increment it by larger values, we can post different values to the counter like below. 

```
riak (master) $ curl -i http://localhost:10018/buckets/crdt_cookbook/counters/basho.com \
 -X POST -d "5"

HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:15:15 GMT
Content-Type: text/plain
Content-Length: 0
```

Riak's counters also support decrementing by posting a negative number instead of a positive number, however this example does not include doing so.

It also doesn't matter which node we send the increments to, they will all be accepted:

```
riak (master) $ curl -i http://localhost:10028/buckets/crdt_cookbook/counters/basho.com \
 -X POST -d "1"

HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:25:07 GMT
Content-Type: text/plain
Content-Length: 0
```

Now, we can see that this all worked, by grabbing the counter value using a GET to the same URL:

```
riak (master) $ curl -i http://localhost:10018/buckets/crdt_cookbook/counters/basho.com

HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:27:11 GMT
Content-Type: text/plain
Content-Length: 1

7

riak (master) $ curl -i http://localhost:10028/buckets/crdt_cookbook/counters/basho.com

HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:27:07 GMT
Content-Type: text/plain
Content-Length: 1

7
```

And hence you can see that all the counter increments were preserved, despite being sent to different hosts. You can also see that the increment of 5 worked correctly.

Next Up, we're going to find a large corpus of data, get it all into counter objects in Riak, and then do some analysis on the data using MapReduce.
