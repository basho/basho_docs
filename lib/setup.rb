Encoding.default_external = 'utf-8'

LANGUAGE = (ENV['RIAK_DOCS_LANG'] || 'en').to_sym
puts "RIAK_DOCS_LANG=#{LANGUAGE}"
I18n.locale = I18n.default_locale = LANGUAGE
I18n.load_path = Dir[File.expand_path(File.join(File.dirname(__FILE__), '..', 'source', 'languages', LANGUAGE.to_s, "#{LANGUAGE.to_s}.yml"))]

$versions = {}
version_file = YAML::load(File.open('data/versions.yml'))
for proj, vs in version_file['currents']
  proj = proj.upcase
  vs = ENV["#{proj}_VERSION"] if ENV["#{proj}_VERSION"].present?
  ENV["#{proj}_VERSION"] = vs if ENV["#{proj}_VERSION"].blank? 
  $versions[proj.downcase.to_sym] = vs
  puts "#{proj}_VERSION=#{ENV["#{proj}_VERSION"]}"
end

$default_project = 'riak'

%w{versionify production_check index sitemap_render_override
  duals helpers downloads notifier}.each do |lib|
  require File.join(File.dirname(__FILE__), lib)
end
