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

module Middleman::Features::Deploy
  S3_BUCKET = 'riakdocstest'
  ACCESS_KEY_ID = ENV['RIAK_DOCS_ACCESS_KEY']
  SECRET_ACCESS_KEY = ENV['RIAK_DOCS_SECRET_KEY']
  CLOUD_DIST_ID = ''

  class << self
    def registered(app)
      app.after_build do
        puts "moving to S3"
        files = Dir['./build/**/*']

        AWS::S3::Base.establish_connection!(access_key_id: ACCESS_KEY_ID, secret_access_key: SECRET_ACCESS_KEY)

        files.each do |f|
          next if File.directory?(f)

          attrs = {
            :access => :public_read,
            'Cache-Control' => 'max-age=315360000'
          }

          # anything under images, js, css goes to "shared"
          if f =~ /^\.\/build\/(?:images|js|css)\//
            # TODO: upload for all project versions, if a distinction exists
            project = "riak"
            version = $versions[project.to_sym]
            key = f.sub(/\.\/build\//, "shared/#{version}/")
          else
            # TODO: dig into the file to see what project it belongs under
            project = "riak"
            version = $versions[project.to_sym]

            # HACK to deal with the riak*-index name change

            if f =~ /^\.\/build\/index.html\//
              # generate an index with fully absolute links
              key = 'index.html'
            elsif f =~ /^\.\/build\/riak[^\/\-]*?\-index.html\//
              key = "#{project}/#{version}/index.html"
            else
              key = f.sub(/\.\/build\//, "#{project}/#{version}/")
            end
          end
          puts " upload %s" % key

          # # compress files
          # if f =~ /\.(html|js|css)$/
          #   `gzip #{f} && mv #{f}.gz #{f}`
          #   attrs[:content_encoding] = 'gzip'
          # end

          AWS::S3::S3Object.store(key, File.open(f), S3_BUCKET, attrs)
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

module Middleman::Features::ProductionCheck
  class << self
    def registered(app)
      # $production = true
      raise "RIAK_VERSION required to deploy" unless $versions[:riak]
    end
  end
end
