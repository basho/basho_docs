require 'rubygems'
require 'yaml'
require 'redcarpet'

class String
  def del!(regex)
    gsub!(regex, '')
  end
end

# Directories
$here = File.expand_path(File.dirname(__FILE__))

# Gather configuration (none yet)
# $config = YAML.load_file('#$here/config.yml')

# List of pages not to render (TODO)
$dont_render = ['release-notes.md']

$markdown_parser = Redcarpet::Markdown.new(Redcarpet::Render::HTML.new(),
  :fenced_code_blocks => true,
  :autolink => true,
  :no_intra_emphasis => true,
  :space_after_headers => false
)

def generate_pdf
  # Which sections to render (TODO)
  dirs_to_render = ['dev', 'ops', 'theory']

  $html_string = String.new
  $html_string << $markdown_parser.render(File.read('build/index.md'))

  Dir['../source/languages/en/riak/**/*.md'].each do |file|
    if !($dont_render.include? file)
      raw_file_contents = File.read(file)
      raw_file_contents.del!(/---(.*)---/m)
      raw_file_contents.gsub!(/\/images/, '../source/images')
      $html_string << $markdown_parser.render(raw_file_contents)
    end
  end

  final_string = "<html><head><title>The Riak Docs</title></head><body>#{$html_string}</body></html>"

  puts 'Generating an HTML file at /build/docs.html...'
  File.write('build/docs.html', final_string)

  puts 'Generating PDF...'

  # Bust out Pandoc
  `pandoc build/docs.html -o docs.pdf --latex-engine=xelatex`

  puts 'Generation complete!'
end

generate_pdf