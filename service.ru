require 'sinatra'
set :env, :production
disable :run

require './deploy-service.rb'

run Sinatra::Application
