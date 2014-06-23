#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require 'rubygems'
require 'redcarpet'
require 'fileutils'
require 'erb'
require 'yaml'

OUTPUT_DIR = 'rendered'

include FileUtils

$here = File.expand_path(File.dirname(__FILE__))

# Quick hack to make it easier to delete regexes from strings
class String
  def del!(regex)
    gsub!(regex, '')
  end
end

def gen_pdf

  # Convert the files index into an ordered_markdown_files Array

  ordered_markdown_files = Array.new
  File.read('2.0-index.txt').each_line.to_a.each do |filename|
    ordered_markdown_files.push(filename.del!("\n"))
  end

  def command_exists?(command)
    if File.executable?(command) then
      return command
    end
    ENV['PATH'].split(File::PATH_SEPARATOR).map do |path|
      cmd = "#{path}/#{command}"
      File.executable?(cmd) || File.executable?("#{cmd}.exe") || File.executable?("#{cmd}.cmd")
    end.inject{|a, b| a || b}
  end

  def replace(string, &block)
    string.instance_eval do
      alias :s :gsub!
      instance_eval(&block)
    end
    string
  end

  def verbatim_sanitize(string)
    string.gsub('\\', '{\textbackslash}').
      gsub('~', '{\textasciitilde}').
      gsub(/([\$\#\_\^\%])/, '\\\\' + '\1{}')
  end

  def pre_pandoc(string, config)
    replace(string) do
      # Pandoc discards #### subsubsections - this hack recovers them
      # be careful to try to match the longest sharp string first
      s %r{\#\#\#\#\# (.*?)$}, 'PARAGRAPH: \1'
      s %r{\#\#\#\# (.*?)$}, 'SUBSUBSECTION: \1'
      s %r{\#\#\# (.*?)$}, 'SUBSECTION: \1'
      s %r{\<h5\>(.*?)\<\/h5\>}, 'PARAGRAPH: \1'
      s %r{\<h4\>(.*?)\<\/h4\>}, 'SUBSUBSECTION: \1'
      s %r{\<h3\>(.*?)\<\/h3\>}, 'SUBSECTION: \1'

      # s %r{\<aside.*?\>.*?\<h3\>(.+?)\<\/h3\>(.+?)\<\/aside\>}im, "ASIDE: \\1\n\\2\nENDASIDE"
      s %r{\<aside.*?\>(.+?)\<\/aside\>}im, "ASIDE: \\1\n:ENDASIDE"

      # Process figures
      # s /^\!\[(.*?)\]\(\.\.\/(.*?\/decor\/drinks.*?)\)/, 'DEC2: \2'
      # s /^\!\[(.*?)\]\(\.\.\/(.*?\/decor\/.*?)\)/, 'DEC: \2'
      # s /^\!\[(.*?)\]\(\.\.\/assets\/(.*?)\.\w{3}\)/, 'FIG: \2 --- \1'
    end
  end

  def post_pandoc(string, config)
    replace(string) do
      # Reformat for the book documentclass as opposed to article
      s '\section', '\chap'
      s '\sub', '\\'
      s /SUBSUBSECTION: (.*)/, '\subsubsection{\1}'
      s /SUBSECTION: (.*)/, '\subsection{\1}'
      s /PARAGRAPH: (.*)/, '\paragraph{\1}'

      # replace asides
      # s /\<aside.*?\>\s*\<h3\>(.+?)\<\/h3\>(.+?)\<\/aside\>/, "\\begin{aside}\n\\begin{center}\n\\emph{\1}\n\\end{center}\n\2\n\\end{aside}"
      s /ASIDE: (.+?)\:ENDASIDE/m, "\\begin{aside}\n\\1\\end{aside}"

      # Enable proper cross-reference
      s /#{config['fig'].gsub(/\s/, '\s')}\s*(\d+)\-\-(\d+)/, '\imgref{\1.\2}'
      s /#{config['tab'].gsub(/\s/, '\s')}\s*(\d+)\-\-(\d+)/, '\tabref{\1.\2}'
      s /#{config['prechap'].gsub(/\s/, '\s')}\s*(\d+)(\s*)#{config['postchap'].gsub(/\s/, '\s')}/, '\chapref{\1}\2'

      # Miscellaneous fixes
      # s /DEC: (.*)/, "\\begin{wrapfigure}{r}{.3\\textwidth}\n  \\includegraphics[scale=1.0]{\\1}\n\\end{wrapfigure}"
      s /DEC: (.*)/, "\\begin{wrapfigure}{r}{.4\\textwidth}\n  \\includegraphics[scale=1.0]{\\1}\n\\end{wrapfigure}"
      s /DEC2: (.*)/, "\\begin{wrapfigure}{r}{.65\\textwidth}\n  \\includegraphics[scale=1.0]{\\1}\n\\end{wrapfigure}"
      s /FIG: (.*?) \-\-\- (.*)/, '\img{\1}{\2}'
      s '\begin{enumerate}[1.]', '\begin{enumerate}'
      s /(\w)--(\w)/, '\1-\2'
      s /``(.*?)''/, "#{config['dql']}\\1#{config['dqr']}"

      # Typeset the maths in the book with TeX
      s '\verb!p = (n(n-1)/2) * (1/2^160))!', '$p = \frac{n(n-1)}{2} \times \frac{1}{2^{160}}$)'
      s '2\^{}80', '$2^{80}$'
      s /\sx\s10\\\^\{\}(\d+)/, '\e{\1}'

      # Convert inline-verbatims into \texttt (which is able to wrap)
      s /\\verb(\W)(.*?)\1/ ,'\\texttt{\2}'

      # Make Tables 2-1..2-3 actual tables
      s /\\begin\{verbatim\}\n(([^\t\n]+\t.*?\n)+)(([^\t\n]*)\n)?\\end\{verbatim\}/ do
        $cap = $4
        "\\begin{table}[ht!]
          \\refstepcounter{tab}
          \\centering
          \\label{tab:\\thetab}
          \\begin{tabular}{p{2.75cm}p{8.25cm}}
            \\toprule\n" <<
            verbatim_sanitize($1).
              gsub(/^([^\n\t]+)\t/, '{\footnotesize\texttt{\1}} & ').
              gsub(/(\n)/, '\\\\\\\\\1').
              sub(/\{\\footnotesize\\texttt(.*?)\n/, '{\1\midrule ').
              concat("
            \\bottomrule
          \\end{tabular}
          \\textbf{\\caption{#{$cap}}}
        \\end{table}")
      end

      # char13 doesn't render right with pandoc
      s /\{\\char13\}/, "'"
      s /(\d+)\\\^\{\}(\d+)/, "$\\1^{\\2}$"

      # hack fixs of bad bash renderer
      s /we\ execute /, "we execute \\linebreak[4]"

      # Shaded verbatim block
      s /(\\begin\{verbatim\}.*?\\end\{verbatim\})/m, '\begin{shaded}\1\end{shaded}'
      # s /\\begin\{shaded\}(.*?)\\end\{shaded\}/im, '\1'
      # s /\\begin\{highlighting\}(?:\[\])?(.*?)\\end\{highlighting\}/im, '\1'
    end
  end

  missing = ['pandoc', 'xelatex'].reject{|command| command_exists?(command)}
  unless missing.empty?
    puts "Missing dependencies: #{missing.join(', ')}."
    puts "Install these and try again."
    exit
  end

  $config = YAML.load_file("#$here/tex.yml")
  template = ERB.new(File.read("#$here/template.tex"))

  config = $config['default'].merge($config[lang]) rescue $config['default']

  print "\tAssembling Markdown string...\n"

  markdown = String.new

  ordered_markdown_files.each do |file|
    file_contents = File.read(file)

    metadata = file_contents.match(/---(.*)---/)

    p metadata


    file_contents.gsub!(/\/images/, '../source/images')
    markdown << file_contents
    markdown << "\n\n"
  end

  print "\tParsing Markdown...\n"

  latex = IO.popen('pandoc -p --no-wrap -f markdown -t latex', 'w+') do |pipe|
    pipe.write(markdown)

    # pipe.write(pre_pandoc(markdown, config))
    pipe.close_write
    post_pandoc(pipe.read, config)
  end

  puts "done\n"
  print "\tCreating riak-docs.tex...\n"

  dir = "#$here/rendered"

  File.open("#{dir}/riak-docs.tex", 'w') do |file|
    file.write(template.result(binding))
  end

  puts "done\n"
  puts "Running XeTex... "

  abort = false

  3.times do |i|
    print "\t\tAttempting pass #{i + 1}. This could be a while... "

    IO.popen("xelatex -papersize=letter -output-directory=\"#{dir}\" \"#{dir}/riak-docs.tex\" 2>&1") do |pipe|
      unless $DEBUG
        if $_[0..1]=='! '
          puts "failed with:\n\t\t\t#{$_.strip}"
          puts "\tConsider running this again with --debug"
          abort = true
        end while not abort and pipe.gets
      else
        STDERR.print while pipe.gets rescue abort = true
      end
    end
    break if abort
    puts "done"
  end
end


gen_pdf
