namespace :css do
    source = "./dynamic/css"
    dest   = "./dynamic/compiled"

    ##########
    # CSS Compressed build
    ##########
    desc "Compile all required CSS"
    task :build do
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

        Compass.configuration.add_import_path "#{source}/imports"
        Compass.add_configuration({
                :project_path  => ".",
                :http_path     => "/",
                :output_style  => :compressed,
                :line_comments => false,
                :sass_path     => source,
                :css_path      => dest,
                :cache_path    => "#{source}/.sass-cache",
            }, "basho_docs_configs")
        compiler = Compass.sass_compiler({
                :only_sass_files => Dir.glob("#{source}/*.scss")
            })
        compiler.compile!
    end

    ##########
    # CSS Debug build
    ##########
    desc "Compile all scss with the Nested style, and load all css into /static/css"
    task :debug do
        require 'compass'
        require 'compass/sass_compiler'

        #TODO(Drew): This is largely a duplicate of the above rule. Split it out
        #            into it's own function.
        Compass.configuration.add_import_path "#{source}/imports"
        Compass.add_configuration({
                :project_path  => ".",
                :http_path     => "/",
                :output_style  => :nested,
                :line_comments => true,
                :sass_path     => source,
                :css_path      => dest,
                :cache_path    => "#{source}/.sass-cache",
            }, "basho_docs_configs")
        compiler = Compass.sass_compiler({
                :only_sass_files => Dir.glob("#{source}/*.scss")
            })
        compiler.compile!
    end

    ##########
    # CSS Clean
    ##########
    desc "Delete generated CSS files"
    task :clean do
        #TODO(Drew): This is maybe a bit much to clear generated files. Consider
        #            working out how to get the below dead code to work?
        FileUtils.rm_rf(dest)
        # Dir.foreach(dest) { |f|
        #     if "#{f}" != '.' && "#{f}" != '..'
        #         File.delete(File.join(dest, f))
        #         puts "Cleaning #{File.join(dest, f)}"
        #     end
        # }
    end

    ##########
    # CSS Watch
    ##########
    desc "Spin up a Watchr process to monitor changes in the CSS files"
    task :watch do
        puts "The CSS Watchr is not yet implemented."
    end
end # of namespace :css
task :css => 'css:build'

