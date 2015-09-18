# Rules:
#  build
#  build:css
#  build:js
#  build:debug
#  build:debug:css
#  build:debug:js
#  clean
#  clean:css
#  clean:js
#  watch
#  watch:css
#  watch:js
#  watch:debug
#  watch:debug:css
#  watch:debug:js


$css_source = "./dynamic/css"
$css_dest   = "./dynamic/compiled/css"
$js_source  = "./dynamic/js"
$js_dest    = "./dynamic/compiled/js"
$cache_dir  = "./dynamic/.cache"


#TODO(Drew): These are maybe a bit much? Be more specific about what's deleted?
task      :clean => ['clean:js', 'clean:css']
namespace :clean do

  task :js do
    FileUtils.rm_rf($js_dest)
  end

  task :css do
    FileUtils.rm_rf($css_dest)
  end
end


task      :build => ['build:js', 'build:css']
namespace :build do

  #TODO(Drew): This just points to the debug version for now. That's... bad.
  #            Finalize the debug version, implement a production version.
  task :js => 'build:debug:js'

  task :css do
    compass_compile({
      :output_style  => :compressed,
      :line_comments => false
    })
  end

  task      :debug => ['clean', 'build:debug:js', 'build:debug:css']
  namespace :debug do

    directory "#{$js_dest}"
    task :js => "#{$js_dest}" do
      require 'sprockets'
      require 'erb'
      require 'yaml'
      require 'coffee-script'

      env = Sprockets::Environment.new(".")
      env.append_path 'dynamic/js'

      #TODO(Drew): We're just hardcoding the two JS files that we want for the
      #            time being. That's an _awful_ solution, and these files
      #            should be rolled into one, buuuut this is a quick first-pass.
      #            **Fix this!**
      File.open("#{$js_dest}/all.js", 'w') { |file| file.write env['all.js'].to_s }
      File.open("#{$js_dest}/version-bar.coffee.erb", 'w') { |file| file.write env['version-bar.js'].to_s }
    end

    task :css do
      compass_compile({
        :output_style  => :nested,
        :line_comments => true
      })
    end
  end
end


task      :watch do sh 'bundle exec guard -g css js'; end
namespace :watch do

  task :js  do sh 'bundle exec guard -g js'; end
  task :css do sh 'bundle exec guard -g css'; end

  task      :debug do sh 'bundle exec guard -g debug_js debug_css'; end
  namespace :debug do

    task :js  do sh 'bundle exec guard -g debug_js'; end
    task :css do sh 'bundle exec guard -g debug_css'; end
  end
end



# Helper functions
def compass_compile(configs)
   # This code was largely inspired by this SO question
    # http://stackoverflow.com/questions/25399962/compass-from-ruby-sasscompiler-not-found
    # with addtional code and configuraion options drawn from:
    # https://github.com/Compass/compass/blob/350bcaa544f594bca972aaa29a9bdfddceee5d4f/cli/lib/compass/exec/global_options_parser.rb
    # https://github.com/Compass/compass/blob/350bcaa544f594bca972aaa29a9bdfddceee5d4f/cli/lib/compass/exec/project_options_parser.rb
    # NOTE(Drew): I made the choice here to start from the above SO question
    #            and code snippets to make this call using the undocumented
    #            Ruby API of Compass rather than building a shell call out
    #            of a Ruby string and passing that to the OS layer. I don't
    #            know if it's the right choice, but it's the choice I made.
    #            Hopefully no one gets bit by it.
    require 'compass'
    require 'compass/sass_compiler'
    require 'sass-css-importer'

    Compass.configuration.add_import_path "#{$css_source}/imports"
    configs[:project_path]  = "."
    configs[:http_path]     = "/"
    configs[:sass_path]     = $css_source
    configs[:css_path]      = $css_dest
    configs[:cache_path]    = $cache_dir
    Compass.add_configuration(configs, "basho_docs_configs")
    compiler = Compass.sass_compiler({
        :only_sass_files => Dir.glob("#{$css_source}/*.scss")
      })
    compiler.compile!
end
