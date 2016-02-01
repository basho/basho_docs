#!/usr/bin/env ruby

require 'optparse'

envs = {}
envs['RIAK_DOCS_LANG'] = 'en'
envs['DEPLOY'] = "true"
only = []

OptionParser.new do |opt|
  opt.on('-r', '--riak RIAK_VERSION') { |o| envs['RIAK_VERSION'] = o; only += ['riak'] }
  opt.on('-c', '--riakcs RIAKCS_VERSION') { |o| envs['RIAKCS_VERSION'] = o; only += ['riakcs'] }
  opt.on('-d', '--dataplatform DATAPLATFORM_VERSION') { |o| envs['DATAPLATFORM_VERSION'] = o; only += ['dataplatform'] }
  opt.on('-t', '--riakts RIAKTS_VERSION') { |o| envs['RIAKTS_VERSION'] = o; only += ['riakts'] }
  opt.on('-q', '--dry-run', 'Build, but don\'t deploy to S3') { |o| envs['DEPLOY'] = "false" }
  opt.on('-h', 'Display this help message') { puts opt; exit }
end.parse!

only_s = only.join(",")
envs['ONLY'] = only.join(",") unless only.empty?

`rm -rf build`

if (envs['DEPLOY'] == "true" && (
    "#{ENV['AWS_ACCESS_KEY_ID']}" == '' ||
    "#{ENV['AWS_CLOUDFRONT_DIST_ID']}" == '' ||
    "#{ENV['AWS_S3_BUCKET']}" == '' ||
    "#{ENV['AWS_SECRET_ACCESS_KEY']}" == ''))
  puts "Required env vars for this script:"
  puts "    AWS_ACCESS_KEY_ID"
  puts "    AWS_CLOUDFRONT_DIST_ID"
  puts "    AWS_S3_BUCKET"
  puts "    AWS_SECRET_ACCESS_KEY"
  puts ""
end

system envs, "bundle exec middleman build"
