module Rack; end

# A hack to aid development. Treat versioned paths as non-versioned
module Rack::Middleman
  class VersionRouter
    def initialize(app, options = {})
      @app = app
      @options = options
    end

    def call(env)
      %w{PATH_INFO REQUEST_PATH REQUEST_URI}.each{|v| alter_route(env, v) }
      status, @headers, @body = @app.call(env)
      [status, @headers, @body]
    end

    def alter_route(env, var)
      # HACK to deal with the riak*-index name change
      if env[var] =~ /\/(riak[^\/]*?)\/[^\/]+\/?(index\.html)?$/
        env[var].sub!($2, '') if $2
        env[var] += "#{$1}-index" if $1
      end
      env[var] = env[var].sub(/\/(?:riak[^\/]*?|shared)\/[^\/]+/, '') if env.include?(var)
    end
  end
end
