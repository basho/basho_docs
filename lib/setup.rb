Encoding.default_external = 'utf-8'

LANGUAGE = (ENV['RIAK_DOCS_LANG'] || 'en').to_sym
puts "RIAK_DOCS_LANG=#{LANGUAGE}"
I18n.locale = I18n.default_locale = LANGUAGE
I18n.load_path = Dir[File.expand_path(File.join(File.dirname(__FILE__), '..', 'source', 'languages', LANGUAGE.to_s, "#{LANGUAGE.to_s}.yml"))]

$versions = {}
$only_projects = []
$only_versions = []
version_file = YAML::load(File.open('data/versions.yml'))
for proj, vs in version_file['currents']
  proj = proj.upcase
  vs = ENV["#{proj}_VERSION"] if ENV["#{proj}_VERSION"].present?
  ENV["#{proj}_VERSION"] = vs if ENV["#{proj}_VERSION"].blank? 
  $only_projects << proj.downcase
  $only_versions << vs
  $versions[proj.downcase.to_sym] = vs
  puts "#{proj}_VERSION=#{ENV["#{proj}_VERSION"]}"
end

if ENV['ONLY']
  $only_projects = ENV['ONLY'].split(/,/).uniq
  $only_versions = $only_projects.map{|pr|
    $versions[pr.to_sym]
  }.uniq
end

$default_project = 'riak'

%w{versionify production_check index sitemap_render_override
  helpers downloads notifier}.each do |lib|
  require File.join(File.dirname(__FILE__), lib)
end
