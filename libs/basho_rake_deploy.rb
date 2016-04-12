###########################
# Deploy rules and helpers

# Once the Hugo site has been fully and correctly generated, we can upload the
# updated and new -- and delete the no longer generated -- files to/from our S3
# bucket, and send out CloudFront invalidation requests to propagate those
# changes to Amazon's CDNs. To pull this off, we're going to re-implement rsyn--
# I mean, compare the MD5 sums (and existence) of all of the local objects
# with their S3 counterparts, and perform selective uploads. We'll use the
# upload list to then generate the invalidation request.
def do_deploy()
  require 'digest/md5'
  require 'aws-sdk'
  require 'progressbar'
  require 'simple-cloudfront-invalidator'
  require 'yaml'

  # Validation check for environment variables
  if (!ENV['AWS_S3_BUCKET']          ||
      !ENV['AWS_ACCESS_KEY_ID']      ||
      !ENV['AWS_SECRET_ACCESS_KEY']  ||
      !ENV['AWS_CLOUDFRONT_DIST_ID'] ||
      !ENV['AWS_HOST_NAME']            )
    puts("The below required environment variable(s) have not been defined.\n"\
         "Without them, the Deploy process cannot complete.\n"\
         "Please verify that they have been correctly defined.")
    puts("  * AWS_S3_BUCKET") if (!ENV['AWS_S3_BUCKET'])
    puts("  * AWS_ACCESS_KEY_ID") if (!ENV['AWS_ACCESS_KEY_ID'])
    puts("  * AWS_SECRET_ACCESS_KEY") if (!ENV['AWS_SECRET_ACCESS_KEY'])
    puts("  * AWS_CLOUDFRONT_DIST_ID") if (!ENV['AWS_CLOUDFRONT_DIST_ID'])
    puts("  * AWS_HOST_NAME") if (!ENV['AWS_HOST_NAME'])
    exit()
  end

  puts("========================================")
  puts("Beginning Deploy Process...")

  # Capture Hugo's config.yaml while we're in the same directory
  config_file = YAML.load_file('config.yaml')

  # Make sure we actually loaded a file, and didn't just set `config_file` to
  # the string "config.yaml".
  if (config_file == "config.yaml")
    Kernel.abort("ERROR: Could not find 'config.yaml'. Are you running Rake "\
                 "from the correct directory?")
  end

  # Move into the Hugo destination directory, so file names are only prefixed
  # with "./"
  Dir.chdir(File.join(Dir.pwd, "#{$hugo_dest}"))

  # Generate a list of every file in $hugo_dest, and `map` them into the form,
  #   [["«file_name»", "«md5sum»"], ... ]
  puts("  Aggregating Local Hash List...")
  local_file_list = Dir["./**/*"]
    .select { |f| File.file?(f) }
    .sort_by { |f| f }
    .map { |f|
      # The file names have a leading `./`. Strip those.
      [f[2..-1], Digest::MD5.file(f).hexdigest] }


  # Open up a connection to our S3 target bucket
  puts("  Opening S3 Connection...")
  aws_bucket = Aws::S3::Bucket.new(ENV['AWS_S3_BUCKET'], {
      :region            => "us-east-1",
      :access_key_id     => ENV['AWS_ACCESS_KEY_ID'],
      :secret_access_key => ENV['AWS_SECRET_ACCESS_KEY'],
    })

  # Fetch all objects from the remote, and `map` them into the form,
  #   [["«file_name»", "«md5sum»"], ... ]
  puts("  Fetching Remote Object List (this may take up to ~30 seconds)...")
  aws_file_list = aws_bucket
    .objects()
    .sort_by { |objs| objs.key }
    .map { |objs|
      # the etag (which is the md5sum) is wrapped in double-quotes. Strip those
      # by 'translating' them into empty strings.
      [objs.key, objs.etag.tr('"','')] }


  # Now that we have the two lists, we need to compare them and generate the
  # list of files we need to upload, and the list of files we need to delete.
  # To do this, we're going to use some brute force.
  puts("  Comparing Object Hashes...")
  new_list     = []
  updated_list = []
  delete_list  = []
  lcl_i = 0
  aws_i = 0
  lcl_len = local_file_list.length
  aws_len = aws_file_list.length
  progress = ProgressBar.new("   Hash check", lcl_len)
  while true
    # Check if we've reached the end of either list and should break
    break if (lcl_i == lcl_len || aws_i == aws_len)
    lcl_file_name = local_file_list[lcl_i][0]
    aws_file_name = aws_file_list[aws_i][0]

    # Compare the file/object names
    case lcl_file_name <=> aws_file_name
    when  0 # File names are identical
      # Compare md5sums. If they don't match, add the file to the updated list.
      if (local_file_list[lcl_i][1] != aws_file_list[aws_i][1])
        updated_list.push(lcl_file_name)
      end
      # In either case, increment both index variables
      lcl_i += 1; progress.inc
      aws_i += 1
    when -1 # Local file name sorts first...
      # The local file doesn't exist on AWS. Add it to the new list.
      new_list.push(lcl_file_name)
      # And only increment the local index variable.
      lcl_i += 1; progress.inc
    when  1 # AWS object name sorts first...
      # The AWS object doesn't (or no longer) exist in the locally built
      # artifacts. Schedule it for deletion.
      delete_list.push(aws_file_name)
      # And only increment the aws index variable.
      aws_i += 1
    end
  end

  # If we're not at the end of the local file list, we need to add any new files
  # to the new list.
  while (lcl_i < lcl_len)
    new_list.push(local_file_list[lcl_i][0])
    lcl_i += 1; progress.inc
  end

  # If we're not at the end of the aws object list, we need to add those file to
  # the delete list.
  while (aws_i < aws_len)
    delete_list.push(aws_file_list[aws_i][0])
    aws_i += 1
  end
  progress.finish

  puts("  Hash Check complete")
  puts("    #{new_list.length} new files, and #{updated_list.length} updated..")
  puts("    #{upload_list.length} files need to be uploaded to the remote")
  puts("    #{delete_list.length} files need to be deleted from the remote")


  # Upload the files in the upload updated and new lists, and delete the files
  # in the delete list.
  upload_list = updated_list + new_list
  if (upload_list.length > 0)
    puts("  Uploading files...")
    progress = ProgressBar.new("   Uploads", upload_list.length)
    upload_list.each { |obj_path|
      #TODO: Generate a log of the uploaded files?
      #TODO: Error checking.
      #TODO: Stop publishing read/write.
      aws_bucket.put_object({
          acl: "public-read-write",
          key: obj_path,
          body: File.open(obj_path)
        })
      progress.inc
    }
    progress.finish
  else
    puts("  No files to upload...")
  end

  if (delete_list.length > 0)
    delete_list.each_slice(1000).with_index do |slice, index|
      index_from = index * 1000
      index_to   = ((index+1)*1000) < delete_list.length ? ((index+1)*1000) : delete_list.length
      puts("  Requesting Batch Delete for objects #{index_from}--#{index_to}...")
      # Generate a Aws::S3::Types::Delete hash object.
      delete_hash = {
        delete: {
          objects: slice.map{ |f| { key: f } },
          quiet: false
        }
      }
      #TODO: Generate a log of the deleted files?
      begin
        response = aws_bucket.delete_objects(delete_hash)
      rescue Exception => e
        require 'pp'
        Kernel.abort("ERRROR: Batch Deletion returned with errors.\n"\
                     "        Delete Hash Object:\n"\
                     "#{pp(delete_hash)}\n"\
                     "        Error message:\n"\
                     "#{e.message}")
      end
      if (response.errors.length > 0)
        require 'pp'
        Kernel.abort("ERRROR: Batch Deletion returned with errors\n"\
                     "        Delete Hash Object:\n"\
                     "#{pp(delete_hash)}\n"\
                     "        Response Object:\n"\
                     "#{pp(response)}")
      end
    end
  else
    puts("  No files to delete...")
  end

  # Fetch and rewrite the S3 Routing Rules to make sure the 'latest' of every
  # project correctly re-route.
  puts("  Configuring S3 Bucket Website redirect rules...")

  # Open an S3 connection to the Bucket Website metadata
  aws_bucket_website = Aws::S3::BucketWebsite.new(ENV['AWS_S3_BUCKET'], {
      :region            => "us-east-1",
      :access_key_id     => ENV['AWS_ACCESS_KEY_ID'],
      :secret_access_key => ENV['AWS_SECRET_ACCESS_KEY'],
    })

  # Build the routing rules based on the config.yaml's 'project_descripts'. One
  # routing rule per project.
  routing_rules = []
  config_file['params']['project_descriptions'].each do |project, description|
    path          = description['path']
    archived_path = description['archived_path']
    ver           = description['latest']
    routing_rules.push(
      {
        :condition => { :key_prefix_equals       => "#{archived_path}/latest/" },
        :redirect  => { :replace_key_prefix_with => "#{path}/#{ver}/",
                        :host_name               => ENV['AWS_HOST_NAME'] }
      }
    )
    routing_rules.push(
      {
        :condition => { :key_prefix_equals       => "#{archived_path}/latest" },
        :redirect  => { :replace_key_prefix_with => "#{path}/#{ver}/",
                        :host_name               => ENV['AWS_HOST_NAME'] }
      }
    )
    routing_rules.push(
      {
        :condition => { :key_prefix_equals       => "#{path}/latest/" },
        :redirect  => { :replace_key_prefix_with => "#{path}/#{ver}/",
                        :host_name               => ENV['AWS_HOST_NAME'] }
      }
    )
    routing_rules.push(
      {
        :condition => { :key_prefix_equals       => "#{path}/latest" },
        :redirect  => { :replace_key_prefix_with => "#{path}/#{ver}/",
                        :host_name               => ENV['AWS_HOST_NAME'] }
      }
    )
  end
  #TODO: We need to implement some way of adding arbitrary routing rules. Maybe
  # add a section in config.yaml that's just a JSON string that we parse?
  riak_path = config_file['params']['project_descriptions']['riak_kv']['path']
  riak_ver  = config_file['params']['project_descriptions']['riak_kv']['latest']
  routing_rules.push(
    {
      :condition => { :key_prefix_equals       => "riakee/latest/" },
      :redirect  => { :replace_key_prefix_with => "#{riak_path}/#{riak_ver}/",
                      :host_name               => ENV['AWS_HOST_NAME'] }
    }
  )
  routing_rules.push(
    {
      :condition => { :key_prefix_equals       => "riakee/latest" },
      :redirect  => { :replace_key_prefix_with => "#{riak_path}/#{riak_ver}",
                      :host_name               => ENV['AWS_HOST_NAME'] }
    }
  )
  routing_rules.push(
    {
      :condition => { :key_prefix_equals       => "riakts/" },
      :redirect  => { :replace_key_prefix_with => "riak/ts/",
                      :host_name               => ENV['AWS_HOST_NAME'] }
    }
  )

  new_website_configuration = {
    :error_document => {:key => "404.html"},
    :index_document => aws_bucket_website.index_document.to_hash,
    :routing_rules  => routing_rules
  }

  aws_bucket_website.put({ website_configuration: new_website_configuration })

  # Invalidate any files that were deleted or modified.
  cf_client = SimpleCloudfrontInvalidator::CloudfrontClient.new(
      ENV['AWS_ACCESS_KEY_ID'], ENV['AWS_SECRET_ACCESS_KEY'],
      ENV['AWS_CLOUDFRONT_DIST_ID'])
  invalidation_list = updated_list + delete_list
  if (invalidation_list.length == 0)
    puts("  No files to invalidate...")
  elsif (invalidation_list.length < 500)
    # The invalidation list is sufficiently short that we can invalidate each
    # individual file
    invalidation_list.each_slice(100).with_index do |slice, index|
      index_from = (index*100)
      index_to   = ((index+1)*100) < delete_list.length ? ((index+1)*100) : delete_list.length
      puts("  Sending Invalidation Request for objects #{index_from}--#{index_to}...")
      cf_report = cf_client.invalidate(slice)
    end
  else
    # The invalidation list is large enough that we should skip getting charged
    # and invalidate the entire site.
    puts("  Sending Invalidation Request for the entire site (\"/*\")...")
    cf_report = cf_client.invalidate(['/*'])
  end

  puts("")
  puts("Deploy Process Complete!")
  puts("========================================")
end
