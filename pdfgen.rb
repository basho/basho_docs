#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'rubygems'
require 'yaml'
require 'redcarpet'

class String
	def del!(regex)
		gsub!(regex, '')
	end
end

markdown_toc_parser = Redcarpet::Markdown.new(Redcarpet::Render::HTML_TOC.new(),
	:fenced_code_blocks => true,
  :autolink => true,
  :no_intra_emphasis => true,
  :space_after_headers => false
)

markdown_parser = Redcarpet::Markdown.new(Redcarpet::Render::HTML.new(),
  :fenced_code_blocks => true,
  :autolink => true,
  :no_intra_emphasis => true,
  :space_after_headers => false
)

$html_string = ""
$toc_string = ""
Dir['source/languages/en/riak/**/*.md'].each do |file|
	raw_file_contents = File.read(file)
	raw_file_contents.gsub!(/\/images/, 'source/images')
	raw_file_contents.del!('http://b.vimeocdn.com/ts/135/477/135477978_200.jpg')
	raw_file_contents.del!('http://basho.comsource/images/riak-on-drugs.jpg')
	$html_string << markdown_parser.render(raw_file_contents)
	$toc_string << markdown_parser.render(raw_file_contents)
end

html = $html_string

final_string = """<html>
  <head>
    <title>The Riak Documentation</title>
  </head>
  <body>
    #{html}
  </body>
</html>"

File.write('docs.html', final_string)