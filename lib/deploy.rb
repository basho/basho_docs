require 'rubygems'
require 'aws/s3'
require 'hmac'
require 'hmac-sha1'
require 'net/https'
require 'base64'

ACCESS_KEY_ID = ENV['AWS_ACCESS_KEY_ID']
SECRET_ACCESS_KEY = ENV['AWS_SECRET_ACCESS_KEY']
CLOUD_DIST_ID = 'E2Q6TQ5O0XT58T'
S3_BUCKETS = {:en => 'riakdocs.en'}
CF_BATCH_SIZE = 1000

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
          begin
            AWS::S3::S3Object.store(key, File.open(f), S3_BUCKETS[:en], attrs)
          rescue
            $stderr << "Failed to upload #{f}\n"
          end
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
        puts " == Invalidating CloudFront"
        
        # you can only invalidate in batches of 1000
        def invalidate(first=0, last=CF_BATCH_SIZE)
          paths = ""
          total = 0

          Dir['./build/**/*'].each do |f|
            next if File.directory?(f)
            total += 1
            next if total <= first
            key = f.gsub(/\.\/build\//, '/')
            paths += "<Path>#{key}</Path>"
            break if total >= last
          end

          count = (total < last ? total : last) - first
          
          paths = "<Paths><Quantity>#{count}</Quantity><Items>#{paths}</Items></Paths>"

          digest = HMAC::SHA1.new(SECRET_ACCESS_KEY)
          digest << date = Time.now.utc.strftime("%a, %d %b %Y %H:%M:%S %Z")

          uri = URI.parse("https://cloudfront.amazonaws.com/2012-07-01/distribution/#{CLOUD_DIST_ID}/invalidation")
          header = {
            'x-amz-date' => date,
            'Content-Type' => 'text/xml',
            'Authorization' => "AWS %s:%s" % [ACCESS_KEY_ID, Base64.encode64(digest.digest)]
          }
          # p header
          req = Net::HTTP::Post.new(uri.path)
          req.initialize_http_header(header)
          body = "<InvalidationBatch xmlns=\"http://cloudfront.amazonaws.com/doc/2012-07-01/\">#{paths}<CallerReference>ref_#{Time.now.utc.to_i}</CallerReference></InvalidationBatch>"
          # puts body
          req.body = body

          http = Net::HTTP.new(uri.host, uri.port)
          http.use_ssl = true
          http.verify_mode = OpenSSL::SSL::VERIFY_NONE
          res = http.request(req)

          puts res.code == '201' ? "CloudFront reloading #{count}" : "Failed #{res.code}"
          puts res.body if res.code != '201'

          if first + count <= total
            invalidate(last, last+CF_BATCH_SIZE)
          end
        end

        invalidate()
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
