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

require './libs/basho_rake_deploy'
require './libs/basho_downloads_gen'

$css_source = "./dynamic/css"
$css_dest   = "./static/css"
$js_source  = "./dynamic/js"
$js_dest    = "./static/js"
$cache_dir  = "./dynamic/.cache"
$hugo_dest  = "./public" # Should always be set to `publishdir` from config.yml

### Rake directory definitions
directory "#{$js_dest}"
directory "#{$css_dest}"
directory "#{$cache_dir}"

#TODO<drew.pirrone.brusse@gmail>: Check to make sure Hugo's version is >= 0.15


######################################################################
### Rake Namespace and Task definitions

Rake::TaskManager.record_task_metadata = true
##########
# Default
task :default do
  puts("Basho Documentation Generate System Usage:")
  puts("")
  Rake::application.options.show_tasks = :tasks  # this solves sidewaysmilk problem
  Rake::application.options.show_task_pattern = //
  Rake::application.display_tasks_and_comments
end;


########
# Clean
desc      "Clean previous builds"
task      :clean => ['clean:js', 'clean:css']
namespace :clean do
  #TODO<drew.pirrone.brusse@gmail>: These `rm -rf`s are maybe a bit much? Should
  # we be more precise with what we delete (and, if so, how)?
  task :js do
    # Ignoring files from standalone/, as they're from the Middleman content
    js_file_list = Dir["#{$js_dest}/**/**"].reject { |f| f =~ /standalone/ }
    js_file_list.each do |f|
      log_dir_deletion(f)
      FileUtils.rm(f)
    end
  end
  task :css do
    # Ignoring files from standalone/, as they're from the Middleman content
    css_file_list = Dir["#{$css_dest}/**/**"].reject { |f| f =~ /standalone/ }
    css_file_list.each do |f|
      log_dir_deletion(f)
      FileUtils.rm(f)
    end
  end
  task :hugo do
    log_dir_deletion($hugo_dest)
    FileUtils.rm_rf($hugo_dest)
  end
end


########
# Build
desc      "Compile & Compress JS, Compile & Compress CSS"
task      :build => ['clean', 'build:js', 'build:css']
desc      "Compile Markdown content to #{$hugo_dest}"
task      :hugo  => ['clean:hugo', 'build:hugo']
namespace :build do
  task :js => "#{$js_dest}" do compile_js(debug: false); end
  task :css => "#{$css_dest}" do compile_css(debug: false); end

  desc "Build all statically-generated Hugo content"
  task :hugo => ['clean:hugo'] do sh "hugo -d #{$hugo_dest}"; end

  desc "Shorthand for `rake build; rake watch:hugo` (Note: exits w/ an error)"
  task :watch => ['build:js', 'build:css', 'watch:hugo']

  ################
  # Build : Debug
  desc      "Compile Human-Readable JS, Compile Human-Readable CSS, Build Hugo"
  task      :debug => ['build:debug:js', 'build:debug:css', 'build:hugo']
  namespace :debug do
    desc "Compile Human-Readable JS"
    task :js => "#{$js_dest}" do compile_js(debug: true); end

    desc "Compile Human-Readable CSS"
    task :css => "#{$css_dest}" do compile_css(debug: true); end
  end
end


########
# Watch
desc      "Run Guard on JS and CSS"
task      :watch do sh 'bundle exec guard -g css js'; end
namespace :watch do

  task :js  do sh 'bundle exec guard -g js'; end
  task :css do sh 'bundle exec guard -g css'; end

  #TODO<drew.pirrone.brusse@gmail>: Add in some way to specify ip/port.
  desc "Run Hugo Server"
  task :hugo do sh "hugo server --ignoreCache=true"; end

  ################
  # Watch : Debug
  desc      "Run Guard on JS and CSS in Debug Mode"
  task      :debug => ['clean'] do sh 'bundle exec guard -g debug_js debug_css'; end
  namespace :debug do
    task :js  do sh 'bundle exec guard -g debug_js'; end
    task :css do sh 'bundle exec guard -g debug_css'; end
  end
end


#########
# Deploy
desc      "Build and deploy static artifacts"
task      :deploy => [
                      'clean',
                      'deploy:fetch_archived_content',
                      'build:js',
                      'build:css',
                      'build:hugo'
                     ] do do_deploy(); end
namespace :deploy do
  task :immediately_and_unsafely do do_deploy(); end
  task :fetch_archived_content do do_fetch_archived_content(); end
end

# Generate download.yaml metadata.
#   This task should be run every time a new package is placed onto the
#   downloads server, and the updated file(s) should be tracked in Git.
desc "Generate package URI information"
task :gen_download_info do generate_download_yaml(); end


######################################################################
### Helper/Compilation functions

# Helper function that will print "    deleting #{dir_name}" to the console, and
# color the "deleting" text red.
def log_dir_deletion(dir_name)
  red = "\033[31m"
  nc  = "\033[0m" # no color
  print "    #{red}deleting#{nc} #{dir_name}\n"
end

def compile_js(debug: false)
  require 'sprockets'
  require 'yaml'
  require 'uglifier'

  env = Sprockets::Environment.new(".")
  env.append_path $js_source

  if (debug)
    #TODO<drew.pirrone.brusse@gmail>: This doesn't make anything more readable.
    #    I don't know how to -- or if we can -- improve readability of js that's
    #    been run through Sprockets...
    # env.js_compressor = Uglifier.new(
    #   :output => {
    #     :comments => :all,
    #     # :preserve_line => true
    #   },
    #   :mangle => false,
    #   :compress => false)
  else
    env.js_compressor = Uglifier.new(
      :output => {
        :comments => :none
      })
  end

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
    log_js_write(dst)
  end
end

# Helper function that will print "    write #{file_name}" to the console, and
# color the "write" text green. This is to match the Compass log output on file
# writes.
def log_js_write(file_name)
  green = "\033[32m"
  nc    = "\033[0m" # no color
  print "    #{green}write#{nc} #{file_name}\n"
end

def compile_css(debug: false)
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
