#!/usr/bin/env ruby

require "rest_client"

require "json"
require "cgi"
require "pp"

$bucket = "crdt_cookbook"

$riak_host = "localhost:10018"

threads = []

# Escape fn
def e(str)
  CGI.escape(str)
end

puts <<-EOM
Deleting all the counters from Riak
Bucket: "#{$bucket}"

EOM

keys_response = RestClient.get "http://#{$riak_host}/buckets/#{e $bucket}/keys?keys=true",
                               accept: :json
exit 1 unless keys_response.code == 200

json = JSON.parse keys_response

json["keys"].each do |key|
  threads << Thread.start do
    RestClient.delete "http://#{$riak_host}/buckets/#{e $bucket}/keys/#{e key}"
    $stdout.print "."
  end
end

threads.map(&:join)

puts <<-EOM

Counters All Deleted.

They may take a few seconds to be removed from disk. 
The link below will show an empty list of keys when complete.

http://#{$riak_host}/buckets/#{e $bucket}/keys?keys=true 

EOM