###########################
# Deploy rules and helpers

# Once the static/ directory has been fully and correctly generated, we can
# upload the updated and new -- and delete the no longer generated -- files
# to/from our S3 bucket, and send out CloudFront invalidation requests to
# propagate those changes to Amazon's CDNs. To pull this off, we're going to re-
# implement rsyn---I mean, compare the MD5 sums (and existence) of all of the
# local objects with their S3 counterparts, and perform selective uploads. We'll
# use the upload list to then generate the invalidation request.
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

  # Move into the static/ directory, so file names lead with "./static"
  Dir.chdir(File.join(Dir.pwd, "static"))

  # Generate a list of every built file in the form of
  # '["<FILE_NAME> <MD5SUM>", ... ]'
  puts("  Aggregating Local Hash List...")
  local_file_list = Dir["./**/*"]
    .select{ |f| File.file?(f) }
    .sort_by{ |f| f }
    .map{ |f|
      # The file names have a leading `./`. Strip those.
      [f[2..-1], Digest::MD5.file(f).hexdigest] }


  # Open up a connection to our S3 target bucket
  puts("  Opening S3 Connection...")
  aws_bucket = Aws::S3::Bucket.new(ENV['AWS_S3_BUCKET'], {
      :region            => "us-east-1",
      :access_key_id     => ENV['AWS_ACCESS_KEY_ID'],
      :secret_access_key => ENV['AWS_SECRET_ACCESS_KEY'],
    })

  # Fetch all objects, and filter out all of the files from the old Middleman
  # deploy. (See below `should_ignore` for details)
  # After the filter, generate a list of '["<FILE_NAME> <MD5SUM>", ... ]'
  puts("  Filtering Remote Object List...")
  # Fetching the actual count of objects in S3 takes 15 seconds---as long as
  # the .reject fold itself. We're just going to use an estimate here, and track
  # the actual as well. After we're doing updating, we can warn if the estimate
  # is sufficiently off.
  aws_obj_count_estimate = 33000
  aws_obj_count_actual = 0
  progress = ProgressBar.new("   Objects", aws_obj_count_estimate)
  aws_object_list = aws_bucket
    .objects()
    .reject{ |objs| # AWS ObjectSummary
      progress.inc
      aws_obj_count_actual += 1
      should_ignore(objs.key) }
    .sort_by{ |objs| objs.key }
    .map{ |objs|
      # the etag (which is the md5sum) is wrapped in double-quotes. Strip those
      # by 'translating' them into empty strings.
      [objs.key, objs.etag.tr('"','')] }
  progress.finish


  # Now that we have the two lists, it's time to parse through them and generate
  # lists of files we need to upload, and lists of files we need to delete.
  # To do this, we're... going to use some brute force.
  puts("  Comparing Object Hashes...")
  upload_list = []
  delete_list = []
  lcl_i = 0
  aws_i = 0
  lcl_len = local_file_list.length
  aws_len = aws_object_list.length
  progress = ProgressBar.new("   Hash check", lcl_len)
  while true
    # Check if we've reached the end of either list and should break
    break if (lcl_i == lcl_len || aws_i == aws_len)
    lcl_file_name = local_file_list[lcl_i][0]
    aws_file_name = aws_object_list[aws_i][0]
    if (should_ignore(lcl_file_name))
      Kernel.abort("ERROR: We are attempting to upload the file "\
                   "\"#{lcl_file_name}\" which is among the set of locked "\
                   "files from the Legacy Middleman build.\n"\
                   "       Please verify that this file is among those you "\
                   "want to upload and, if so, modify the \`do_deply\` "\
                   "function of this Rakefile such that it is not included in "\
                   "the \`should_ignore\` list.")
    end
    #### binding.pry if ((lcl_file_name <=> aws_file_name) != 0)
    # Compare the file/object names
    case lcl_file_name <=> aws_file_name
    when  0 # File names are identical
      # Compare md5sums, if they don't match, add the file to the upload list.
      if (local_file_list[lcl_i][1] != aws_object_list[aws_i][1])
        upload_list.push(lcl_file_name)
      end
      # In either case, increment the indexers
      lcl_i += 1; progress.inc
      aws_i += 1
    when -1 # Local file name sorts first...
      # The local file doesn't exist on AWS. Add it to the upload list, and
      # increment only the local index variable.
      upload_list.push(lcl_file_name)
      lcl_i += 1; progress.inc
    when  1 # AWS object name sorts first...
      # The AWS object doesn't (no longer) exists in the locally built
      # artifacts. Schedule it for deletion, and increment only the aws index.
      delete_list.push(aws_file_name)
      aws_i += 1
    end
  end

  # If we're not at the end of the local file list, we need to add any new files
  # to the upload list.
  while (lcl_i < lcl_len)
    upload_list.push(local_file_list[lcl_i][0])
    lcl_i += 1; progress.inc
  end

  # If we're not at the end of the aws object list, we need to add those file to
  # the delete list.
  while (aws_i < aws_len)
    delete_list.push(aws_object_list[aws_i][0])
    aws_i += 1
  end
  progress.finish

  puts("  Hash Check complete")
  puts("    #{upload_list.length} files need to be uploaded to the remote")
  puts("    #{delete_list.length} files need to be deleted from the remote")


  # Upload the files in the upload list and delete the files in the delete list.
  if (upload_list.length > 0)
    puts("  Uploading files...")
    progress = ProgressBar.new("   Uploads", upload_list.length)
    upload_list.each{ |obj_path|
      #TODO: Generate a log of the uploaded files?
      if (aws_bucket.object(obj_path).upload_file(obj_path) != true)
        #TODO: Maybe this should also be a `Kernel.abort`?
        #TODO: Probably want to send this out on STDERR.
        puts("ERROR: Failed to upload #{obj_path}!")
      end
      progress.inc
    }
    progress.finish
  else
    puts("  No files to upload...")
  end

  if (delete_list.length > 0)

    delete_list.each_slice(1000).with_index do |slice, index|
      puts("  Requesting Batch Delete for objects #{index*1000}--#{(index+1)*1000}...")
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
    name = description['name']
    ver  = description['latest']
    routing_rules.push(
      {
        :condition => { :key_prefix_equals       => "#{name}/latest/" },
        :redirect  => { :replace_key_prefix_with => "#{name}/#{ver}/",
                        :host_name               => ENV['AWS_HOST_NAME'] }
      }
    )
  end
  #TODO: Consider implementing some way of adding arbitrary routing rules. Maybe
  # allow for a section in config.yaml that's just a JSON string that we parse?
  #TODO: Figure out if we're correctly routing from archived content /latest/
  # pages to the new site. I don't believe we currently are.
  new_website_configuration = {
    :error_document => aws_bucket_website.error_document.to_hash,
    :index_document => aws_bucket_website.index_document.to_hash,
    :routing_rules  => routing_rules
  }

  aws_bucket_website.put({ website_configuration: new_website_configuration })

  # Invalidate any files that were modified --- either uploaded or deleted
  invalidation_list = upload_list + delete_list
  if (invalidation_list.length > 0)
    #FIXME: Since we're still in development, and I've had some issues with the
    #       deploy process (also b/c Amazon charges per invalidation, _not_ per
    #       file invalidated), I'm going to be lazy here and just invalidate the
    #       entire S3 bucket.
    #       This should be fixed after we start using this deploy script.
    if (false)
      cf_client = SimpleCloudfrontInvalidator::CloudfrontClient.new(
          ENV['AWS_ACCESS_KEY_ID'], ENV['AWS_SECRET_ACCESS_KEY'],
          ENV['AWS_CLOUDFRONT_DIST_ID'])
      invalidation_list.each_slice(100).with_index do |slice, index|
        puts("  Sending Invalidation Request for objects #{index*100}--#{(index+1)*100}...")
        cf_report = cf_client.invalidate(slice)
      end
    else
      cf_client = SimpleCloudfrontInvalidator::CloudfrontClient.new(
          ENV['AWS_ACCESS_KEY_ID'], ENV['AWS_SECRET_ACCESS_KEY'],
          ENV['AWS_CLOUDFRONT_DIST_ID'])
      puts("  Being lazy and sending Invalidation Request for \"/*\"...")
      cf_report = cf_client.invalidate(['/*'])
    end
  else
    puts("  No files to invalidate...")
  end

  puts("")
  puts("Deploy Process Complete!")
  puts("========================================")


  # Bookkeeping check to alert if our file estimates are too far off.
  acceptable = (aws_obj_count_estimate * 0.10)
  if ((aws_obj_count_actual - aws_obj_count_estimate).abs > acceptable)
    puts("")
    puts("")
    puts("DEVS: The estimated AWS object count (#{aws_obj_count_estimate}) is "\
         " more than 10% off of actual (#{aws_obj_count_actual}).\n"\
         "      Please consider modifying the \`aws_obj_count_estimate\` "\
         " variable accordingly.")
  end
end

# This function takes an object path, and returns `true` if it's in the set of
# old Middleman-generated files that we don't want to modify.
# This set includes...
#   * BingSiteAuth.xml
#   * google59ea6be0154fb436.html
#   * css/standalone/*
#   * js/standalone/*
#   * riak/1.*.*/
#   * riak/2.0.*/
#   * riak/2.1.*/
#   * riakcs/1.*.*/
#   * riakcs/2.0.*/
#   * riakcs/2.1.*/
#   * riakee/*
#   * shared/*
#
#TODO: Abstract this out to data, so we can more easilly manage what should be
#      ignored.
def should_ignore(obj_path)
  return (obj_path =~ /^BingSiteAuth.xml$/             ||
          obj_path =~ /^google59ea6be0154fb436\.html$/ ||
          obj_path =~ /^css\/standalone\//             ||
          obj_path =~ /^js\/standalone\//              ||
          obj_path =~ /^riakee\//                      ||
          obj_path =~ /^shared\//                      ||
          obj_path =~ /^riakcs\//                      ||
          obj_path =~ /^riak\/\d\.\d\.\d/                )
end
