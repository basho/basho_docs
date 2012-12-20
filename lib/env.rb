LANGUAGE = (ENV['RIAK_DOCS_LANG'] || 'en').to_sym
puts "RIAK_DOCS_LANG=#{LANGUAGE}"
I18n.locale = I18n.default_locale = LANGUAGE

if ENV['RIAK_VERSION'].blank? || ENV['RIAK_VERSION'] !~ /[\d\.]+/
  versions = YAML::load(File.open('data/versions.yml'))
  for proj, vs in versions['currents']
    proj = proj.upcase
    ENV["#{proj}_VERSION"] = vs
    puts "#{proj}_VERSION=#{ENV["#{proj}_VERSION"]}"
  end
end

$versions = {
  :riak => ENV['RIAK_VERSION'].presence,
  :riakcs => ENV['RIAKCS_VERSION'].presence || ENV['RIAK_VERSION'].presence,
  :riakee => ENV['RIAKEE_VERSION'].presence || ENV['RIAK_VERSION'].presence
}
