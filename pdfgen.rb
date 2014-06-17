#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'rubygems'
require 'yaml'

global_nav = 'source/languages/en/global_nav.yml'
nav = YAML.load_file(global_nav)['riak']

$start_here = nav[0]['sub']
$dev_docs = nav[1]['sub']
$ops_docs = nav[2]['sub']
$theory_docs = nav[3]['sub']
$community_docs = nav[4]['sub']
$apis_docs = nav[5]['sub']

