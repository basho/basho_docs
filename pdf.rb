#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'rubygems'
require 'pdfkit'
require 'fileutils'

include FileUtils

$html_dir = Dir.open()

riak_html_dir = Dir.open("build/riak/latest")
raw_html_content = String.new
Dir.glob("build/riak/latest/**/*.html").each do |file|
	file_contents = File.read(file)
	raw_html_content.concat(file_contents)
end

kit = PDFKit.new(raw_html_content, :page_size => 'Letter')
# kit.stylesheets << "build/css/standalone/version-bar.css"
pdf = kit.to_file("riak.pdf")