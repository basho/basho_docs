module Rack; end

# A hack to aid development. Treat versioned paths as non-versioned
module Rack::Middleman
  class VersionRouter
    def initialize(app, options = {})
      @app = app
      @options = options
    end

    def call(env)
      env["PATH_INFO"] = env["PATH_INFO"].sub(/\/riak\/[^\/]+/, '') if env.include? "PATH_INFO"
      env["REQUEST_PATH"] = env["REQUEST_PATH"].sub(/\/riak\/[^\/]+/, '') if env.include? "REQUEST_PATH"
      env["REQUEST_URI"] = env["REQUEST_URI"].sub(/\/riak\/[^\/]+/, '') if env.include? "REQUEST_URI"
      status, @headers, @body = @app.call(env)
      [status, @headers, @body]
    end
  end
end
