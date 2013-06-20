#!/bin/sh

node=localhost:10018       # dev1@127.0.0.1
other_node=localhost:10028 # dev2@127.0.0.1

function storeInRiak {
    curl -X PUT "http://$node/buckets/crdt_cookbook/keys/$1" -H "Content-Type: $2" --data-binary @$1
}

curl -s -o jquery-2.0.0.min.js http://ajax.googleapis.com/ajax/libs/jquery/2.0.0/jquery.min.js

storeInRiak counters_demo.html 'text/html'
storeInRiak jquery-2.0.0.min.js 'application/javascript'

curl "http://$node/buckets/crdt_cookbook/counters/test" -X POST -d "1"
curl "http://$node/buckets/crdt_cookbook/counters/test" -X POST -d "-1"

erlc counters_demo.erl

echo "Done. 
Example Page 1: http://$node/buckets/crdt_cookbook/keys/counters_demo.html
Example Page 2: http://$other_node/buckets/crdt_cookbook/keys/counters_demo.html

To Demo a Partition:

1. Attach a shell to your riak cluster
   $ dev/dev1/bin/riak attach

2. To load the code (if you haven't already, in the MapReduce step)
   1> rpc:multicall(code, add_patha, [\"/path/to/riak_crdt_cookbook/counters\"
   
3. To Partition: 
   2> Healer = counters_demo:part(['dev3@127.0.0.1'], \"newcookie\").
   
4. Do some increments and decrements on both pages above. 
   Make sure they eventually show different values
   
5. To Heal:
   3> Healer().
   
6. Watch as both counters update to keep all writes from both windows. 
   They should now both be on the same value.

";