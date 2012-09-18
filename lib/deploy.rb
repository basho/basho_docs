require 'rubygems'
require 'aws/s3'
require 'hmac'
require 'hmac-sha1'
require 'net/https'
require 'base64'

module Middleman
  module Features
  end
end

module ::Middleman::Features::Deploy
  S3_BUCKET = 'riakdocstest'
  ACCESS_KEY_ID = ENV['RIAK_DOCS_ACCESS_KEY']
  SECRET_ACCESS_KEY = ENV['RIAK_DOCS_SECRET_KEY']
  CLOUD_DIST_ID = ''

  class << self
    def registered(app)
      app.after_build do
        # dig into the file to see what project it belongs under
        def get_project(file)
          # puts file
          File.readlines(file).join.to_s =~ /\<meta\s+content\s*\=\s*["']project["']\s+name\s*\=\s*["']([^"']+)["']/
          $1 || "riak"
        end

        puts "moving to S3"
        files = Dir['./build/**/*']

        AWS::S3::Base.establish_connection!(:access_key_id => ACCESS_KEY_ID, :secret_access_key => SECRET_ACCESS_KEY)

        def upload(key, f)
          attrs = {
            :access => :public_read,
            'Cache-Control' => 'max-age=315360000'
          }
          puts " upload %s" % key
          AWS::S3::S3Object.store(key, File.open(f), S3_BUCKET, attrs)
        end

        files.each do |f|
          next if File.directory?(f)

          # a copy of the full site to reflect the latest values

          # anything under images, js, css goes to "shared"
          if f =~ /^\.\/build\/(?:images|js|css)\//
            # upload shared for all given project versions
            $versions.values.uniq.each do |version|
              key = f.sub(/\.\/build\//, "shared/#{version}/")
              upload(key, f)
            end
            next
          elsif f == "./build/favicon.ico"
            key = "/favicon.ico"
          else
            project = get_project(f)
            version = $versions[project.to_sym]

            if f =~ /^\.\/build\/index\.html$/
              key = 'index.html'
            elsif f =~ /\/(riak[^\/\-]*?\/#{version}\/index\.html)$/
              key = $1
            else
              key = f.sub(/\.\/build\//, "#{project}/#{version}/")
            end
          end

          upload(key, f)

          # upload twice... once under version, and once with "latest"
          key = key.sub(/(riak[^\/\-]*?\/)[^\/]+(\/.*?)/, '\1latest\2')
          upload(key, f)
        end


        # puts "invalidate cloudfront"
        # paths = ""
        # files.each do |f|
        #   next if File.directory?(f)
        #   key = f.gsub(/\.\/tmp/, '')
        #   paths += "<Path>#{key}</Path>"
        # end

        # digest = HMAC::SHA1.new(SECRET_ACCESS_KEY)
        # digest << date = Time.now.utc.strftime("%a, %d %b %Y %H:%M:%S %Z")

        # uri = URI.parse("https://cloudfront.amazonaws.com/2010-08-01/distribution/#{CLOUD_DIST_ID}/invalidation")
        # req = Net::HTTP::Post.new(uri.path)
        # req.initialize_http_header({
        #   'x-amz-date' => date,
        #   'Content-Type' => 'text/xml',
        #   'Authorization' => "AWS %s:%s" % [ACCESS_KEY_ID, Base64.encode64(digest.digest)]
        # })
        # req.body = "<InvalidationBatch>#{paths}<CallerReference>ref_#{Time.now.utc.to_i}</CallerReference></InvalidationBatch>"

        # http = Net::HTTP.new(uri.host, uri.port)
        # http.use_ssl = true
        # http.verify_mode = OpenSSL::SSL::VERIFY_NONE
        # res = http.request(req)

        # puts res.body
        # puts res.code == '201' ? 'CloudFront reloaded' : "Failed #{res.code}"
      end
    end
    alias :included :registered
  end
end

$production = false

module ::Middleman::Features::ProductionCheck
  class << self
    def registered(app)
      $production = true
      raise "RIAK_VERSION required to deploy" unless $versions[:riak]
    end
  end
end
