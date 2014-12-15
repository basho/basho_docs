#!/usr/bin/env ruby

if ENV['AWS_CLOUDFRONT_DIST_ID'] == '' || ENV['AWS_S3_BUCKET'] == ''
  puts "AWS_CLOUDFRONT_DIST_ID and AWS_S3_BUCKET env vars are required"
  exit(0)
end

ENV['RIAK_DOCS_LANG'] = 'en'

`rm -rf build`
envs = {}
if ARGV.length > 0
  ENV['RIAK_VERSION'] = ARGV[0]
  ENV['RIAKCS_VERSION'] = ARGV[1] || ARGV[0]
  ENV['RIAKEE_VERSION'] = ARGV[2] || ARGV[0]
  envs = {
  	'RIAK_VERSION' => ENV['RIAK_VERSION'],
  	'RIAKCS_VERSION' => ENV['RIAKCS_VERSION'],
    'RIAKEE_VERSION' => ENV['RIAKEE_VERSION']
  }
end
envs['DEPLOY'] = ENV['DEPLOY'] = 'true'
exec envs, "bundle exec middleman build"
