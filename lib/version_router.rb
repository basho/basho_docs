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
