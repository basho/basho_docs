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
      unless env[var] =~ %r"\/((?:#{projects_regex}|shared)\/[^\/]+)\/?(index\.html)?$"
        if env.include?(var)
          env[var] = env[var].sub(%r"\/(?:#{projects_regex}|shared)\/([\d\.]+(?:rc\d+|pre\d+|beta\d+)?|latest)", '')
          # set as current page version
          version = $1 == 'latest' ? nil : $1
          SitemapRenderOverride.current_version = version
        end
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
        next if path =~ /^(?:#{projects_regex})\/[\d\.]+(?:rc\d+|pre\d+|beta\d+)?\/index\.html/
        resource.destination_path = path.sub(/^(#{projects_regex})\//, '')
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
          File.readlines(file).join.to_s =~ /\<meta\s+content\s*\=\s*["']([^"']+)["']\s+name\s*\=\s*["']project["']/
          $1 || "riak"
        rescue
          "riak"
        end

        def copy(f, dest)
          puts " moving %s" % dest
          # puts dest.sub(/\/[^\/]+\.\w+$/, '')
          FileUtils.mkdir_p(dest.sub(/\/[^\/]+\.\w+$/, ''))
          FileUtils.cp_r(f, dest) #, :verbose => true)
        end

        def change_version_to_latest(f)
          f.sub(/((?:#{projects_regex})\/)[^\/]+(\/.*?)/, '\1latest\2')
        end

        # If we're rendering the current version, then generate the latest
        def include_latest?(project)
          versions ||= YAML::load(File.open('data/versions.yml'))
          versions['currents'][project] == $versions[project.to_sym]
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
          project = get_project(f)

          # leave it
          if f =~ /^\.\/build\/.+?\/standalone\//
            next
          # anything under images, js, css goes to "shared"
          elsif f =~ /^\.\/build\/(?:images|data)\//
            # upload shared for all given project versions
            # $versions.values.uniq.each do |version|
            $only_versions.each do |version|
              # key = f.sub(/\.\/build\//, "shared/#{version}/")
              move_to = f.sub(/\.\/build\//, "./build/shared/#{version}/")
              copy(f, move_to)
            end
            FileUtils.rm(f)
            next
          elsif f =~ /^\.\/build\/(?:fonts|js|css)\//
            $versions.each do |proj, version|
              next unless $only_versions.include?(version)
              move_to = f.sub(/\.\/build\//, "./build/#{proj.to_s}/#{version}/")
              copy(f, move_to)
            end
            FileUtils.rm(f)
            next
          # favico, root index stay put
          elsif f == "./build/favicon.ico"
            next
          elsif f =~ /^\.\/build\/index\.html$/
            # if we don't include the latest, delete this
            next if include_latest?(project)
          # project root files should also copy to latest
          elsif f =~ /\/(?:#{projects_regex})\/[\d\.]+(?:rc\d+|pre\d+|beta\d+)?\/index\.html?$/
            if include_latest?(project)
              copy(f, change_version_to_latest(f))
            end
            next
          else
            version = $versions[project.to_sym]
            move_to = f.sub(/\.\/build\//, "./build/#{project}/#{version}/")
          end

          copy(f, move_to)

          project ||= get_project(f)
          if include_latest?(project)
            # copy twice... once under version, and once with "latest"
            copy(f, change_version_to_latest(move_to))
          end
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
