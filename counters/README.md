# Riak CRDT Cookbook: Counters

Riak KV master (at the time of writing) introduces Counters as a new opaque data type, so here we're going to walk through doing a few things with them using the files in this directory.

This cookbook should take you, the reader, from starting out, not having a clue about what they are and how they work, to being able to use them fully, even in Map-Reduce tasks.

##Let's Get Setup

First off, get the riak master branch setup, with a several-node dev cluster. Follow the [Riak Fast Track: Building a Development Environment](http://docs.basho.com/riak/latest/tutorials/fast-track/Building-a-Development-Environment/), but when installing use the "Installing from GitHub" section of [Installing Riak from Source](http://docs.basho.com/riak/latest/tutorials/installation/Installing-Riak-from-Source/) then continue building your development environment. 

## Baby Steps

First off, we're going to use Curl to set some things up. For the rest of this tutorial, I'm going to use a bucket called `crdt_cookbook`, for clarity.

Quick, Important Interlude: Counters require the bucket property `allow_mult` to be true, so we'll set that first. Don't worry about this, the whole point in CRDTs is that they cope with siblings completely. Note: I've pretty-printed the JSON below, in your console it probably won't look as neat

```
counters $ curl -i http://localhost:10018/buckets/crdt_cookbook/props \
 -X PUT -d '{"props":{"allow_mult":true}}' -H "Content-Type: application/json"

HTTP/1.1 204 No Content
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 15:15:55 GMT
Content-Type: application/json
Content-Length: 0

counters $ curl -i http://localhost:10018/buckets/crdt_cookbook/props
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
counters $ curl -i http://localhost:10018/buckets/crdt_cookbook/counters/basho.com \
 -X POST -d "1"

HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:15:15 GMT
Content-Type: text/plain
Content-Length: 0
```

This increments the counter at `<<"basho.com">>` in our bucket by one (it will also create the counter if it doesn't yet exist). To increment it by larger values, we can post different values to the counter like below. 

```
counters $ curl -i http://localhost:10018/buckets/crdt_cookbook/counters/basho.com \
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
counters $ curl -i http://localhost:10028/buckets/crdt_cookbook/counters/basho.com \
 -X POST -d "1"

HTTP/1.1 204 No Content
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:25:07 GMT
Content-Type: text/plain
Content-Length: 0
```

Now, we can see that this all worked, by grabbing the counter value using a GET to the same URL:

```
counters $ curl -i http://localhost:10018/buckets/crdt_cookbook/counters/basho.com

HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:27:11 GMT
Content-Type: text/plain
Content-Length: 1

7

counters $ curl -i http://localhost:10028/buckets/crdt_cookbook/counters/basho.com

HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Wed, 05 Jun 2013 16:27:07 GMT
Content-Type: text/plain
Content-Length: 1

7
```

And hence you can see that all the counter increments were preserved, despite being sent to different hosts. You can also see that the increment of 5 worked correctly.

## Loading some Example Data

Next Up, we're going to find a large corpus of data, get it all into counter objects in Riak, and then do some analysis on the data using MapReduce. 

For this stage, you'll need Ruby (any version after 1.9.2) installed, as well as the "bundler" rubygem.

[Bryce Kerley](https://twitter.com/BonzoESC) has kindly provided some data from the Miami user group's site google analytics. Per-day page-view stats are in the "Dataset" directory in csv files, so we'll use those to load our data into some Riak counters.

The easiest way to do so is to run the following:

```
counters $ bundle install

Using mime-types (1.23)
Using rest-client (1.6.7)
Using bundler (1.3.5)
Your bundle is complete!
Use `bundle show [gemname]` to see where a bundled gem is installed.

counters $ bundle exec ./load_data.rb

Loading the data from Dataset/*.csv into Riak.
Bucket: "crdt_cookbook"

.................................................................
Finished!

```

> Note: If anything goes wrong, run `bundle exec ./clear_counters.rb` which clears out the "crdt_cookbook" bucket completely.

And now we're ready to do some MapReduce!

## Map-Reducing our Data

Ok, so next up we're going to do some analysis of the data that's in our counters.

I've written a module, `mr_kv_counters.erl`, that contains the functions we're going to use. 

Firstly, let's run down the functions:

- `value/3`   - takes a riak object, and returns the pair {key, count}
- `sum/2`     - takes the output of `value/3`, and computes the total count
- `maximum/2` - takes the output of `value/3`, and computes the list of keys with the highest count
- `strip_date/2` - 

Now, before we can begin, we'll need to compile this module, and tell riak where to find it.

To compile this module, run the following command. It it works, it gives you no output:

```
counters $ erlc mr_kv_counters.erl
```

### Loading our module into Riak

> Note: In a production setting, you'd alter each node's configuration, and then restart each node, to load more code. We're going to cheat, to save time.

Next Up, we need to edit some of your riak cluster's configuration, via "app.config". First, get yourself in
a shell in the master riak checkout you made earlier.

Now, attach a console to the cluster by running `dev/dev1/bin/riak attach`. From now on, the commands will be executed in the Erlang Shell.
  
So, at the Erlang Shell, enter everything after the `>`, not forgetting the `.` at the end which is obligatory:

```
(dev1@127.0.0.1)1> rpc:multicall(code, add_patha, ["/path/to/riak_crdt_cookbook/counters"]).

{[true,true,true,true],[]}

(dev1@127.0.0.1)2> m(mr_kv_counters).

Module mr_kv_counters compiled: Date: June 7 2013, Time: 13.52
Compiler options:  [{outdir,"/path/to/riak_crdt_cookbook/counters"}]
Object file: /path/to/riak_crdt_cookbook/counters/mr_kv_counters.beam
Exports:
         maximum/2
         module_info/0
         module_info/1
         strip_date/2
         sum/2
         value/3
ok

```

To escape this prompt, hit `<Ctrl>-g`, then enter "q" at the "user switch command" prompt and press return. You should get back to your original shell if you do this.
  
### Map-Reducing time!

Now, we're ready to go map-reducing!

First, open up "mr_sum.json" just to see what the query looks like. Firstly, the string for "input" 
specifies which bucket to fetch keys from. This is inefficient in production, but is perfect for
this example, as it's really simple. The list in "query" specifies an ordered list of steps for
the map-reduce engine to perform. This one firstly maps with `value/3` and then reduces with `sum/2`.

> Note: Providing a bucket name to a MapReduce task is very inefficient (it causes a keyscan).  In production, there are other, better ways to select bucket-key pairs as inputs, including secondary indexes.

This should give us a total. Let's see:

```
counters $ curl -i http://localhost:10018/mapred \
 -X POST -H "Content-Type: application/json" -d @mr_sum.json
 
HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Fri, 07 Jun 2013 12:47:11 GMT
Content-Type: application/json
Content-Length: 13

{"total":160}
```

Fantastic! If you don't get the same total, check for partitions, or errors when you ran `bundle exec ./load_data.rb`.

And now for our `max/2` function. The mapreduce query is specified in "mr_max.json". It looks almost exactly like
what we specified in "mr_sum.json", only with a different reduce phase. 

Let's see what it gives us:

```
counters $ curl -i http://localhost:10018/mapred \
 -X POST -H "Content-Type: application/json" -d @mr_max.json

HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Fri, 07 Jun 2013 12:53:31 GMT
Content-Type: application/json
Content-Length: 39

{"20130605!www.meetup.com/miamirb/":14}
```

Right, so now we see a slight issue. I had the keys include both the date (8 digits before the "!") and
the url (everything after the !). This data shows us that the most visited page in any single 24 hours was
the miamirb front page on the 5th of June, 2013. 

### Advanced Map-Reducing

However, while this is useful, we might want to do something a little more complex. While you can do anything
you want (within reason) with this data, I'm just going to split off the date, and see which page gets the most 
visits over the entire period.

To do this, we actually write another *reduce* phase. Yes, a little annoying, but map phases can only accept
lists of bucket-key pairs as inputs, not arbitrary data. Don't worry too much, what we are up to will become clear.

Go back and read the source of `strip_date/2` in "mr_kv_counters.erl". You'll see we just take the list of key-count pairs,
strip off the start of the key until the "!" (the first 9 chars), then return the rest of the key with the count 
for future steps. If there's no "!" in the key, we leave it as is.

```
counters $ curl -i http://localhost:10018/mapred \
 -X POST -H "Content-Type: application/json" -d @mr_max_overall.json

HTTP/1.1 200 OK
Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
Date: Fri, 07 Jun 2013 13:56:42 GMT
Content-Type: application/json
Content-Length: 30

{"www.meetup.com/miamirb/":39}

```

## Wrap-Up

So, we're done. Fantastic, thanks for reading! 

Here are some links for where you can explore various concepts like this further.

- TODO: Links

