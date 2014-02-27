Encoding.default_external = 'utf-8'

LANGUAGE = (ENV['RIAK_DOCS_LANG'] || 'en').to_sym
puts "RIAK_DOCS_LANG=#{LANGUAGE}"
I18n.locale = I18n.default_locale = LANGUAGE
I18n.load_path = Dir[File.expand_path(File.join(File.dirname(__FILE__), '..', 'source', 'languages', LANGUAGE.to_s, "#{LANGUAGE.to_s}.yml"))]

$versions = {}
$only_projects = []
$only_versions = []
version_file = YAML::load(File.open('data/versions.yml'))
$gen_projects = version_file['gen_projects'].map{|r| r.to_sym}
for proj, vs in version_file['currents']
  proj = proj.upcase
  proj_sym = proj.downcase.to_sym
  vs = ENV["#{proj}_VERSION"] if ENV["#{proj}_VERSION"].present?
  ENV["#{proj}_VERSION"] = vs if ENV["#{proj}_VERSION"].blank? 
  $only_projects << proj_sym if $gen_projects.include?(proj_sym)
  $only_versions << vs if $gen_projects.include?(proj_sym)
  $versions[proj_sym] = vs
  puts "#{proj}_VERSION=#{ENV["#{proj}_VERSION"]}"
end

if ENV['ONLY']
  $only_projects = ENV['ONLY'].split(/,/).uniq
  $only_versions = $only_projects.map{|pr|
    $versions[pr.to_sym]
  }.uniq
end

$only_versions.uniq!
$default_project = 'riak'

%w{versionify production_check index sitemap_render_override
  helpers downloads notifier}.each do |lib|
  require File.join(File.dirname(__FILE__), lib)
end
