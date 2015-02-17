require 'rubygems'
require 'yaml'
require 'redcarpet'

class String
  def del!(regex)
    gsub!(regex, '')
  end
end

def generate_latex
  
  ordered_files_list = File.read('2.0-index.txt').each_line.to_a

  markdown_string = String.new

  ordered_files_list.each do |file|
    filename = file.del!("\n")
    raw_file_contents = File.read(filename)
    # raw_file_contents.del!(/---\ntitle(.*)---/m)
    raw_file_contents.gsub!(/\/images/, '../source/images')
    markdown_string << raw_file_contents
  end

  File.write('all.md', markdown_string)

  p markdown_string

  # `pandoc -p --no-wrap -f markdown -t latex all.md -o final.tex`

  # `xelatex -papersize=letter -output-directory="." final.tex`
end

generate_latex