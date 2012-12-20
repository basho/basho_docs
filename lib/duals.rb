require './lib/rocco'

# this is not optimal. Hook it into the "watch" mechanism
puts "== Generating DUAL Pages"
for api in Dir.glob("**/*.api")
  r = Rocco.new(api, [], :language => 'bash', :template_file => './source/layouts/api.mustache')
  File.open(api.sub(/\.api$/, '.html.erb'), 'w') do |html|
    html.write(r.to_html)
  end
end
for api in Dir.glob("**/*.roc")
  r = DocRocco.new(api, [], :language => 'bash', :template_file => './source/layouts/roc.mustache')
  File.open(api.sub(/\.roc$/, '.html.erb'), 'w') do |html|
    html.write(r.to_html)
  end
end
