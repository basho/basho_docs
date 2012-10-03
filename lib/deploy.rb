require 'rubygems'
require 'aws/s3'
require 'hmac'
require 'hmac-sha1'
require 'net/https'
require 'base64'

# ACCESS_KEY_ID = "AKIAIWN7MTGXYFI56OAQ"
# SECRET_ACCESS_KEY = "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"
ACCESS_KEY_ID = ENV['RIAK_DOCS_ACCESS_KEY']
SECRET_ACCESS_KEY = ENV['RIAK_DOCS_SECRET_KEY']
CLOUD_DIST_ID = 'E2Q6TQ5O0XT58T'
S3_BUCKETS = {:en => 'riakdocs.en'}

module S3Deploy
  class << self
    def registered(app)
      app.after_build do
        puts "moving to S3"

        AWS::S3::Base.establish_connection!(:access_key_id => ACCESS_KEY_ID, :secret_access_key => SECRET_ACCESS_KEY)

        # first check
        def upload(key, f, force=false)
          attrs = {
            :access => :public_read,
            'Cache-Control' => 'max-age=315360000'
          }
          puts " upload %s" % key
          AWS::S3::S3Object.store(key, File.open(f), S3_BUCKETS[:en], attrs)
        end

        Dir['./build/**/*'].each do |f|
          next if File.directory?(f)
          upload(f.sub(/\.\/build\//, ''), f)
        end
      end
    end
    alias :included :registered
  end
end

::Middleman::Extensions.register(:s3_deploy, S3Deploy)

module InvalidateCloudfront
  class << self
    def registered(app)
      app.after_build do
        puts "invalidate cloudfront"
        paths = ""
        count = 0
        Dir['./build/**/*'].each do |f|
          next if File.directory?(f)
          key = f.gsub(/\.\/build\//, '/')
          paths += "<Path>#{key}</Path>"
          count += 1
        end

        paths = "<Paths><Quantity>#{count}</Quantity><Items>#{paths}</Items></Paths>"

        digest = HMAC::SHA1.new(SECRET_ACCESS_KEY)
        digest << date = Time.now.utc.strftime("%a, %d %b %Y %H:%M:%S %Z")

        uri = URI.parse("https://cloudfront.amazonaws.com/2012-07-01/distribution/#{CLOUD_DIST_ID}/invalidation")
        req = Net::HTTP::Post.new(uri.path)
        req.initialize_http_header({
          'x-amz-date' => date,
          'Content-Type' => 'text/xml',
          'Authorization' => "AWS %s:%s" % [ACCESS_KEY_ID, Base64.encode64(digest.digest)]
        })
        body = "<InvalidationBatch xmlns=\"http://cloudfront.amazonaws.com/doc/2012-07-01/\">#{paths}<CallerReference>ref_#{Time.now.utc.to_i}</CallerReference></InvalidationBatch>"
        puts body
        req.body = body

        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true
        http.verify_mode = OpenSSL::SSL::VERIFY_NONE
        res = http.request(req)

        puts res.body
        puts res.code == '201' ? 'CloudFront reloaded' : "Failed #{res.code}"
      end
    end
    alias :included :registered
  end
end

::Middleman::Extensions.register(:invalidate_cloudfront, InvalidateCloudfront)

$production = false
module ::Middleman::Features::ProductionCheck
  class << self
    def registered(app)
      $production = true
      raise "RIAK_VERSION required to deploy" unless $versions[:riak]
    end
  end
end
