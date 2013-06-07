#!/usr/bin/env bundle exec ruby

require "rest_client"

require "json"
require "csv"
require "cgi"

$bucket = "crdt_cookbook"

$riak_hosts = [
  "localhost:10018",
  "localhost:10028",
  "localhost:10038",
  "localhost:10048",
]

# Escape fn
def e(str)
  CGI.escape(str)
end

# Basic Increment operation
def increment(counter, amount=1)
  RestClient.post "http://#{$riak_hosts.sample}/buckets/#{e $bucket}/counters/#{e counter}",
                  "#{amount}"
end

# Set allow_mult to true
RestClient.put "http://#{$riak_hosts.first}/buckets/#{e $bucket}/props", 
               {props: {allow_mult: true}}.to_json,
               content_type: :json

threads = []

puts <<-EOM
Loading the data from Dataset/*.csv into Riak.
Bucket: "#{$bucket}"

EOM

Dir["Dataset/Analytics*.csv"].each do |file|
  threads << Thread.start do 
    date = file.match(/\d{8}/)[0]
    CSV.foreach(file) do |row|
      if row[0] && row[0].match(/\A\/miamirb\//i)
        page, count, *_ = row
        increment "#{date}!www.meetup.com#{page}", count
        $stdout.print "."
      end
    end
  end
end

threads.map(&:join)

puts
puts "Finished!"