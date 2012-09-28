#!/usr/bin/env ruby

if ARGV.length == 0
  puts "usage: ./deploy RIAK_VERSION"
  exit(0)
end

`rm -rf build`
ENV['RIAK_VERSION'] = ARGV[0]
ENV['DEPLOY'] = 'true'
exec "bundle exec middleman build"
