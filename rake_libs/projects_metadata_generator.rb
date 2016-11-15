#############################
# Projects Metadata Generator
#
# This file will act as the intermediary for translating the project
# descriptions that are part of the config.yaml `params.project_descriptions`
# metadata map into something that can be queried by JavaScript running on the
# live site.
#
# This implementation, at least, will translate that data into a JSON file that
# will be built into the final public/ directory and uploaded to our S3 bucket.
# From there the JS responsible for building the version-selector will request
# the generated file and build out the version list and salient links.
#
# See the Project Descriptions map in config.yaml for further details.


$TARGET_FILE = "static/data/project_descriptions.json"


def generate_projects_metadata()
  require 'net/http'
  require 'json'
  require 'yaml'

  # The hash we'll be writing out.
  version_sets_hash = {}

  # JSON explicitly disallows comments, so we're going to add a `__comment`
  # member that give a quick description of the file.
  version_sets_hash[:__comment] = "This file was automatically generated "    \
                                  "using `rake generate_projects_metadata`. " \
                                  "See the Project Descriptions entry in "    \
                                  "config.yaml for more information."


  config_file = YAML.load_file("config.yaml")
  if (config_file == "config.yaml")
    Kernel.abort("ERROR: Could not find and read 'config.yaml'. Are you "\
                 "running Rake from the correct directory?")
  end

  config_file['params']['project_descriptions'].each do |project, description|
    version_sets_hash[project.to_sym] = project_hash = {}

    project_hash[:project_name]  = description["project_name"]
    project_hash[:path]          = description["path"]
    project_hash[:archived_path] = description["archived_path"]
    project_hash[:releases]      = description["releases"]
    project_hash[:latest]        = description["latest"]
    project_hash[:lts]           = description["lts"]           if description["lts"]
    project_hash[:archived_url]  = description["archived_url"]  if description["archived_url"]
  end

  puts "Opening \"#{$TARGET_FILE}\" for writing"
  File.open($TARGET_FILE, 'w') do |f|
    f.write(version_sets_hash.to_json)
  end
  puts "Version sets JSON metadata generation complete."
  puts ""
end



# If this file is being run directly, go ahead and generate the versions data.
if __FILE__ == $0
  generate_projects_metadata()
end
