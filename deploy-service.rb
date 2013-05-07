#!/usr/bin/env ruby
require 'rubygems'
require 'bundler/setup'
require 'sinatra'
require 'json'

$running = false
post '/deploy' do
  if ENV['AWS_CLOUDFRONT_DIST_ID'] == '' || ENV['AWS_S3_BUCKET'] == ''
    puts "AWS_CLOUDFRONT_DIST_ID and AWS_S3_BUCKET env vars are required"
    return "AWS_CLOUDFRONT_DIST_ID and AWS_S3_BUCKET env vars are required"
  end
  ENV['RIAK_DOCS_LANG'] = 'en'
  ENV['DEPLOY'] = 'true'
  return "no data" unless params[:payload]
  push = JSON.parse(params[:payload])
  return "in use" if $running
  if push["ref"] == "refs/heads/publish"
    Thread.new do
      $running = true
      puts "running background deploy"
      # pull from private
      puts `git pull origin publish`
      `rm -rf build`
      puts `bundle exec middleman build`
      puts "completed background deploy"
      $running = false
    end
    "deploying"
  end
  ""
end