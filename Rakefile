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
$css_dest   = "./dynamic/compiled"


task      :clean => ['clean:js', 'clean:css']
namespace :clean do

  task :js do
    puts "clean:js not yet implemented."
  end

  #TODO(Drew): This is maybe a bit much? Can we be more specific?
  task :css do
    FileUtils.rm_rf($css_dest)
  end
end


task      :build => ['build:js', 'build:css']
namespace :build do

  task :js do
    puts "build:js not yet implemented."
  end

  task :css do
    compass_compile({
      :output_style  => :compressed,
      :line_comments => false
    })
  end

  task      :debug => ['clean', 'build:debug:js', 'build:debug:css']
  namespace :debug do

    task :js do
      puts "build:debug:js not yet implemented."
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

    Compass.configuration.add_import_path "#{$css_source}/imports"
    configs[:project_path]  = "."
    configs[:http_path]     = "/"
    configs[:sass_path]     = $css_source
    configs[:css_path]      = $css_dest
    configs[:cache_path]    = "#{$css_source}/.sass-cache"
    Compass.add_configuration(configs, "basho_docs_configs")
    compiler = Compass.sass_compiler({
        :only_sass_files => Dir.glob("#{$css_source}/*.scss")
      })
    compiler.compile!
end
