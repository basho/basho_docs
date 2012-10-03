require 'fileutils'

module Rack; end

# A hack to aid development. Treat versioned paths as non-versioned
module Rack::Middleman
  class VersionRouter
    def initialize(app, options = {})
      @app = app
      @options = options
    end

    def call(env)
      unless $production
        %w{PATH_INFO REQUEST_PATH REQUEST_URI}.each{|v| alter_route(env, v) }
      end
      status, @headers, @body = @app.call(env)
      [status, @headers, @body]
    end

    def alter_route(env, var)
      unless env[var] =~ /\/(riak[^\/]*?\/[\d\.]+)\/?(index\.html)?$/
        env[var] = env[var].sub(/\/(?:riak[^\/]*?|shared)\/[\d\.]+/, '') if env.include?(var)
      else
        env[var] = "/#{$1}/index.html"
      end
    end
  end
end

module VersionifyPaths
  class << self
    def registered(app)
      app.after_configuration do
        sitemap.register_resource_list_manipulator(
          :versionify,
          VersionifyPathsManipulator.new(self),
          false
        )
      end
    end
    alias :included :registered
  end

  class VersionifyPathsManipulator
    def initialize(app)
      @app = app
    end

    def manipulate_resource_list(resources)
      resources.each do |resource|
        path = resource.destination_path
        next if path =~ /^riak[^\/]*\/[\d\.]+\/index\.html/
        resource.destination_path = path.sub(/^(riak[^\/]*)\//, '')
      end
    end
  end
end
::Middleman::Extensions.register(:versionify, VersionifyPaths)



module VersionDirs
  class << self
    def registered(app)
      app.after_configuration do
        puts "== Cleanup before build"
        FileUtils.rm_rf("./build")
      end

      app.after_build do |builder|
        def get_project(file)
          File.readlines(file).join.to_s =~ /\<meta\s+content\s*\=\s*["']project["']\s+name\s*\=\s*["']([^"']+)["']/
          $1 || "riak"
        end

        def copy(f, dest)
          puts " moving %s" % dest
          puts dest.sub(/\/[^\/]+\.\w+$/, '')
          FileUtils.mkdir_p(dest.sub(/\/[^\/]+\.\w+$/, ''))
          FileUtils.cp_r(f, dest) #, :verbose => true)
        end

        def change_version_to_latest(f)
          f.sub(/(riak[^\/\-]*?\/)[^\/]+(\/.*?)/, '\1latest\2')
        end

        def cleanup(dir)
          entries = Dir.entries(dir) - [".", ".."]
          subdirs = entries.map { |f| File.join(dir, f) }.select { |f| File.directory? f }
          subdirs.each { |subdir| cleanup(subdir) }

          entries = Dir.entries(dir) - [".", ".."]
          if entries.empty?
            puts "deleting #{dir}"
            Dir.rmdir dir
          end
        end

        puts "shuffling to version dirs"

        Dir['./build/**/*'].each do |f|
          next if File.directory?(f)

          # a copy of the full site to reflect the latest values

          # anything under images, js, css goes to "shared"
          if f =~ /^\.\/build\/(?:images|js|css|fonts)\//
            # upload shared for all given project versions
            $versions.values.uniq.each do |version|
              # key = f.sub(/\.\/build\//, "shared/#{version}/")
              move_to = f.sub(/\.\/build\//, "./build/shared/#{version}/")
              copy(f, move_to)
              FileUtils.rm(f)
            end
            next
          # favico, root index stay put
          elsif f == "./build/favicon.ico" || f =~ /^\.\/build\/index\.html$/
            next
          # project root files should also copy to latest
          elsif f =~ /\/riak[^\/]*?\/[\d\.]+\/index\.html?$/
            copy(f, change_version_to_latest(f))
            next
          else
            project = get_project(f)
            version = $versions[project.to_sym]
            move_to = f.sub(/\.\/build\//, "./build/#{project}/#{version}/")
          end

          copy(f, move_to)

          # copy twice... once under version, and once with "latest"
          copy(f, change_version_to_latest(move_to))
          FileUtils.rm(f)
        end

        # remove all empty directories
        cleanup("./build")
      end
    end
    alias :included :registered
  end
end

::Middleman::Extensions.register(:version_dirs, VersionDirs)
