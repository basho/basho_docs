require 'rubygems'
require 'yaml'
require 'redcarpet'

class String
  def del!(regex)
    gsub!(regex, '')
  end
end

$all_markdown_files = '../source/languages/en/riak/**/*.md'

def gen_html

  markdown_parser = Redcarpet::Markdown.new(Redcarpet::Render::HTML.new(),
    :fenced_code_blocks => true,
    :autolink => true,
    :no_intra_emphasis => true,
    :space_after_headers => false
  )

  html_string = String.new
  
  # Point the converted HTML file to the proper CSS file
  css_head_string = "<link rel=\"stylesheet\" media=\"screen\" type=\"text/css\" href=\"../css/style.css\" />"

  html_string << "<html><head>#{css_head_string}<title>The Riak Docs</title></head><body>"

  html_string << markdown_parser.render(File.read('build/index.md'))

  # Parse the files.txt list into an array
  ordered_files_list = File.read('files.txt').each_line.to_a

  ordered_files_list.each do |file|
    filename = file.gsub("\n", '')
    raw_file_contents = File.read(filename)
    raw_file_contents.del!(/---(.*)---/m)
    raw_file_contents.gsub!(/\/images/, '../source/images')
    html_string << markdown_parser.render(raw_file_contents)
  end

  html_string << "</body></html>"

  # Write that string to docs.html
  puts 'Generating an HTML file at /build/docs.html...'
  File.write('build/docs.html', html_string)
end

def generate_latex
  # TODO
end

def generate_pdf
  gen_html

  # gen_latex

  # Use Pandoc to generate the final PDF
  puts 'Generating PDF...'
  `pandoc build/docs.html -o docs.pdf --latex-engine=xelatex`

  puts 'Generation complete!'
end

generate_pdf