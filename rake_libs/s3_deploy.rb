###########################
# Deploy rules and helpers

$archive_name = "archived_docs.basho.com.tar.bz2"

def do_fetch_archived_content()
  # Fetch and extract the archived content that we want to survive from the
  # Middleman website.
  puts("Verifying archived content...")
  # Verify that the tar.bz2 is present.
  if (not File.file?(File.join(Dir.pwd, "#{$archive_name}")))
    if (`which wget`.empty?)
      # If we don't have wget. Error out.
      Kernel.abort("ERROR: #{$archive_name} was not found, and this system "\
                   "does not have access to `wget`.\n"\
                   "       Please either install `wget` and re-run this "\
                   "deploy, or manually download the file from the below "\
                   "address and place it into this directory.\n"\
                   "    http://s3.amazonaws.com/downloads.basho.com/documentation_content/#{$archive_name}")
    else
      # We have wget, but not the file. Fetch it.
      puts("  Using wget to fetch #{$archive_name} "\
           "(this may take some time)...")
      successful = system("wget http://s3.amazonaws.com/downloads.basho.com/documentation_content/#{$archive_name}")

      if (not successful)
        Kernel.abort("ERROR: Failed to get #{$archive_name}\n"\
                     "       Please download the file from the below "\
                     "address and copy it into this directory.\n"\
                     "    http://s3.amazonaws.com/downloads.basho.com/documentation_content/#{$archive_name}")
      end
    end
  end

  # Verify the file is correct via an md5sum, unless NO_CHECK has been set
  if (ENV['NO_CHECK'] == "True")
    puts("  Skipping #{$archive_name} sha1 check. Good luck.")
  else
    if (`which md5sum`.empty?)
      # We don't have md5sum, and we want to perform a check. Error out.
      Kernel.abort("ERROR: This system does not have `md5sum`, so the "\
                   "contents of #{$archive_name} cannot be verified.\n"\
                   "       Please install the md5sum tool (possibly named "\
                   "md5sha1sum).\n"\
                   "       You may also re-run this script after running "\
                   "`export NO_CHECK=\"True\"`, but it is **highly "\
                   "recommended** that you install `md5sum` instead.")
    end
    web_md5 = Net::HTTP.get("s3.amazonaws.com", "/downloads.basho.com/documentation_content/#{$archive_name}.md5").split(" ")[0]
    loc_md5 = `md5sum #{$archive_name}`.split(" ")[0]
    if (web_md5 != loc_md5)
      Kernel.abort("ERROR: Fetched #{$archive_name} does not match the "\
                   "expected md5sum.\n"\
                   "       Please remove the current #{$archive_name}, reset "\
                   "the contents of the static/ directory (`git clean -xdf "\
                   "static; git checkout -f static`), and re-run this script.")
    end
  end

  puts("Verifying archived content extraction...")
  puts("    Please note, this only checks for directories.\n"\
       "    If something went wrong with a previous extraction or if any "\
       "of the extracted files were modified, please run \`git clean "\
       "-xdf static/\` and re-run this deploy script.")
  #TODO: Consider if this is a good idea or not. I'm leaning towards not.
  should_extract = (
    (not File.exist?("static/css/standalone")) ||
    (not File.exist?("static/js/standalone"))  ||
    (not File.exist?("static/riak"))           ||
    (not File.exist?("static/riakcs"))         ||
    (not File.exist?("static/riakee"))         ||
    (not File.exist?("static/shared"))           )

  if (should_extract)
    puts("Extracting #{$archive_name} (this may take a lot of time)...")
    successful = system("tar -xjf #{$archive_name} -C static")

    if (not successful)
      Kernel.abort("ERROR: #{$archive_name} failed to extract.\n"\
                   "       I... actually don't know why. Not sure how to "\
                   "extract error messages from this system call.")
    end
  end
end

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

  start_time = Time.now.getutc.to_s.gsub(" ","_")
  dry_run = ENV['DRY_RUN'] != "False"

  puts("========================================")
  puts("Beginning Deploy Process...")
  puts("  As a Dry Run.") if (dry_run)

  log_dir = File.join(Dir.pwd, "deploy_logs")
  if (!File.directory?(log_dir))
    Dir.mkdir("#{log_dir}")
  end

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
    .reject { |objs| objs[0] == "last_deployed_time.txt" }

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

  upload_list = updated_list + new_list

  new_files_log = File.join(log_dir, "#{start_time}.new.txt")
  updated_files_log = File.join(log_dir, "#{start_time}.updated.txt")
  deleted_files_log = File.join(log_dir, "#{start_time}.deleted.txt")

  puts("  Hash Check complete")
  puts("    #{new_list.length} new files (logged to #{new_files_log})")
  puts("    #{updated_list.length} updated files (logged to #{updated_files_log})")
  puts("    #{upload_list.length} files need to be uploaded to the remote")
  puts("    #{delete_list.length} files need to be deleted from the remote (logged to #{deleted_files_log})")
  File.open(new_files_log, "w+") do |f|
    f.puts(new_list)
  end
  File.open(updated_files_log, "w+") do |f|
    f.puts(upload_list)
  end
  File.open(deleted_files_log, "w+") do |f|
    f.puts(delete_list)
  end

  if (dry_run)
    puts("")
    puts("Dry Run Complete.")
    puts("Pending changes Logged to #{log_dir}/.")
    puts("========================================")
    exit(0)
  end

  # Upload the files in the upload updated and new lists, and delete the files
  # in the delete list.
  if (upload_list.length > 0)
    puts("  Uploading files...")
    progress = ProgressBar.new("   Uploads", upload_list.length)
    upload_list.each { |obj_path|
      object_ext = File.extname(obj_path)
      object_descriptor = {
        key: obj_path,
        body: File.open(obj_path),
        acl: "public-read"
      }
      case object_ext
        when ".html"; object_descriptor[:content_type] = "text/html"
        when ".txt";  object_descriptor[:content_type] = "text/plain"
        when ".css";  object_descriptor[:content_type] = "text/css"
        when ".js";   object_descriptor[:content_type] = "application/javascript"
        when ".xml";  object_descriptor[:content_type] = "application/xml"

        when ".eot";  object_descriptor[:content_type] = "font/eot"
        when ".ttf";  object_descriptor[:content_type] = "font/ttf"
        when ".svg";  object_descriptor[:content_type] = "image/svg+xml"
        when ".woff"; object_descriptor[:content_type] = "application/font-woff"

        when ".gif";  object_descriptor[:content_type] = "image/gif"
        when ".jpg";  object_descriptor[:content_type] = "image/jpeg"
        when ".jpeg"; object_descriptor[:content_type] = "image/jpeg"
        when ".png";  object_descriptor[:content_type] = "image/png"
        when ".ico";  object_descriptor[:content_type] = "image/x-icon"
        when ".pdf";  object_descriptor[:content_type] = "application/pdf"

        # when ".eps";     object_descriptor[:content_type] = ""
        # when ".graffle"; object_descriptor[:content_type] = ""
        # when ".logo";    object_descriptor[:content_type] = ""
      end
      #TODO: Error checking.
      aws_bucket.put_object(object_descriptor)
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
      puts("  Requesting Batch Delete for objects #{index_from}-#{index_to}...")
      # Generate a Aws::S3::Types::Delete hash object.
      delete_hash = {
        delete: {
          objects: slice.map{ |f| { key: f } },
          quiet: false
        }
      }
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
      index_to   = ((index+1)*100) < invalidation_list.length ? ((index+1)*100) : invalidation_list.length
      puts("  Sending Invalidation Request for objects #{index_from}-#{index_to}...")
      cf_report = cf_client.invalidate(slice)
    end
  else
    # The invalidation list is large enough that we should skip getting charged
    # and invalidate the entire site.
    puts("  Sending Invalidation Request for the entire site (\"/*\")...")
    cf_report = cf_client.invalidate(['/*'])
  end

  # Write a dummy file to mark the time of the last successful deploy.
  aws_bucket.put_object({
    acl: "public-read-write",
    key: "last_deployed_time.txt",
    body: "#{Time.new.utc} -- #{`git rev-parse HEAD`[0..6]}",
    content_type: "text/plain"
  })


  puts("")
  puts("Deploy Process Complete!")
  puts("========================================")
end
