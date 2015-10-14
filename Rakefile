### Basho Docs' Rakefile
#
# This Rakefile is to be invoked with the `rake` Ruby Gem. It's best if the that
# Gem is installed using Bundler and the included Gemfile.
#
# This file will act as the canonical builder for the JavaScript/CoffeeScript
# and CSS/SCSS 'dynamic' files, which are compiled into static/js/ and
# static/css/ respectively.

#TODO<drew.pirrone.brusse@gmail>: Make sure the above description is correct and
# complete.

$css_source = "./dynamic/css"
$css_dest   = "./static/css"
$js_source  = "./dynamic/js"
$js_dest    = "./static/js"
$cache_dir  = "./dynamic/.cache"

### Rake directory definitions
directory "#{$js_dest}"
directory "#{$css_dest}"
directory "#{$cache_dir}"


######################################################################
### Rake Namespace and Task definitions
#TODO<drew.pirrone.brusse@gmail>: Add in HTML rules to invoke Hugo?


########
# Clean
task      :clean => ['clean:js', 'clean:css']
namespace :clean do
  #TODO<drew.pirrone.brusse@gmail>: These `rm -rf`s are maybe a bit much? Should
  # we be more precise with what we delete (and, if so, how)?
  task :js do FileUtils.rm_rf($js_dest); end
  task :css do FileUtils.rm_rf($css_dest); end
end


########
# Build
task      :build => ['clean', 'build:js', 'build:css']
namespace :build do
  #FIXME<drew.pirrone.brusse@gmail>: Implement a production version of the JS
  # compile process, and set this to `false`.
  task :js => "#{$js_dest}" do compile_js(debug: true); end
  task :css => "#{$css_dest}" do compile_scss(debug: false); end

  ################
  # Build : Debug
  task      :debug => ['clean', 'build:debug:js', 'build:debug:css']
  namespace :debug do
    task :js => "#{$js_dest}" do compile_js(debug: true); end
    task :css => "#{$css_dest}" do compile_scss(debug: true); end
  end
end


########
# Watch
task      :watch do sh 'bundle exec guard -g css js'; end
namespace :watch do

  task :js  do sh 'bundle exec guard -g js'; end
  task :css do sh 'bundle exec guard -g css'; end

  ################
  # Watch : Debug
  task      :debug => ['clean'] do sh 'bundle exec guard -g debug_js debug_css'; end
  namespace :debug do
    task :js  do sh 'bundle exec guard -g debug_js'; end
    task :css do sh 'bundle exec guard -g debug_css'; end
  end
end


######################################################################
### Helper/Compilation functions
def compile_js(debug: false)
  require 'sprockets'
  require 'erb'
  require 'yaml'
  require 'coffee-script'

  env = Sprockets::Environment.new(".")
  env.append_path 'dynamic/js'

  # This will produce a zipped array-of-arrays in the form,
  #     [[souce1, dest1], [source2, dest2], ..., [sourceN, destN]]
  # where the source files are all fo the .js, .coffee, and .erb files in the
  # root of $js_source, and the dest files are all equivalently-named .js files.
  src_dst_list = (Dir.glob("#{$js_source}/*.js")
      .concat Dir.glob("#{$js_source}/*.coffee")
      .concat Dir.glob("#{$js_source}/*.erb")
    ).map do |file|
      src = File.basename file
      # We have to use gsub here (rather than `basename(source, ".*")`) because
      # e.g. 'source.coffee.erb' would turn into 'source.coffee', and we want
      # exactly 'source'.
      dst = "#{$js_dest}/#{src.gsub(/\..*/, "")}.js"
      [src, dst]
    end

  for src, dst in src_dst_list
    File.open(dst, 'w') { |file| file.write env[src].to_s }
    #TODO<drew.pirrone.brusse@gmail>: Add some logging in here
  end
end

def compile_scss(debug: false)
  # This code was largely inspired by this SO question
  # http://stackoverflow.com/questions/25399962/compass-from-ruby-sasscompiler-not-found
  # with addtional code and configuraion options drawn from:
  # https://github.com/Compass/compass/blob/350bcaa544f594bca972aaa29a9bdfddceee5d4f/cli/lib/compass/exec/global_options_parser.rb
  # https://github.com/Compass/compass/blob/350bcaa544f594bca972aaa29a9bdfddceee5d4f/cli/lib/compass/exec/project_options_parser.rb

  #NOTE<drew.pirrone.brusse@gmail>: I made the choice here to start from the
  # above SO question and code snippets to make this call using the undocumented
  # Ruby API of Compass rather than building a shell call out of a Ruby string
  # and passing that to the OS layer. I don't know if it's the right choice, but
  # it's the choice I made. Hopefully no one gets bit by it.

  #TODO<drew.pirrone.brusse@gmail>: Now that we have the above JS compilation
  # steps working with Sprockets, should we go back and use Sprockets here too?
  require 'compass'
  require 'compass/sass_compiler'
  require 'sass-css-importer'

  Compass.configuration.add_import_path "#{$css_source}/imports"
  configs = {}
  configs[:project_path]  = "."
  configs[:http_path]     = "/"
  configs[:sass_path]     = $css_source
  configs[:css_path]      = $css_dest
  configs[:cache_path]    = $cache_dir
  configs[:images_path]   = "static/images/"
  configs[:output_style]  = debug ? :nested : :compressed
  configs[:line_comments] = debug ? true    : false
  Compass.add_configuration(configs, "basho_docs_configs")

  # This will grab every .scss file in the $css_source directory, and run them
  # through Compass, generating equivalently-named .css files in the static/css
  # directory. We should try to keep the number of compiled sources to a minimum
  # though, and simply add new/parsed scss/css files to `all.scss`.
  compiler = Compass.sass_compiler({
    :only_sass_files => Dir.glob("#{$css_source}/*.scss")
  })
  compiler.compile!
end
