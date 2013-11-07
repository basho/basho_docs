ROOT_URL = ENV['INDEX_URL']

def make_riak_path(resource, project)
  url = resource.url.gsub(/(^\/)|(\.html$)/, '/')
  url = "#{project}/latest#{url}" unless url =~ %r"#{project}\/\d+[^/]*?\/$"
  url
end

# we only index html pages
def build_yokozuna_index(resources)
  count = 0
  language = I18n.locale || 'en'
  for resource in resources
    next unless resource.url =~ /\.(?:html)|\/$/
    next if resource.url =~ /keywords/
    next if %w{/404.html /index.html}.include?(resource.url)

    metadata = resource.metadata[:page] || {}

    project = metadata['project'] || $default_project
    version = $versions[project.to_sym]

    metadata['path'] = make_riak_path(resource, project)

    key = ("en/" + metadata['path'].to_s).gsub(/[\/]+/, '%2F')

    body = File.read(resource.source_file)

    # remove the top metadata
    body.sub!(/^\-{3}$.*?^\-{3}$/m, '')
    body = metadata['title'].to_s + "\n\n" + body
    body.gsub!(/^\#{1,6}(.*?)$/, '\1. ')
    body.gsub!(/^\s*\*\s*(.*?)$/, '\1. ')
    body.gsub!(/\[\[([^|]+?)\]\]/, '\1')
    body.gsub!(/\[\[(.+?)\|.+?\]\]/, '\1')
    body.gsub!(/\[(.+?)\]\(.*?\)/, '\1')
    # strip out version tags. replace with a common strip_versions method
    body.gsub!(/\{\{[#\/]?([^}]+?)\}\}/, '\1')
    body.gsub!(/["\n\r]|\`{3}/m, ' ')
    body.gsub!(/\s+/m, ' ')

    data = {
        'title_s' => metadata['title'],
        'project_s' => project,
        'doctype_s' => metadata['document'],
        'audience_s' => metadata['audience'],
        'keywords_ss' => metadata['keywords']||[],
        'path_s' => metadata['path'],
        'language_s' => language,
        'version_s' => version,
        'body_en' => body
    }.to_json

    command = <<-CURL
    curl -XPUT '#{ROOT_URL}/#{key}' \
    -H 'content-type:application/json' \
    --data-binary @-<<\\YNFCM
#{data}
YNFCM
    CURL

    p project
    p project == 'java'
    if project == 'java'
        command = <<-CURL
        curl -XDELETE '#{ROOT_URL}/#{key}'
        CURL
    end

    puts "  Indexing #{key}"
    %x"#{command}"
  end
end

# yz_extractor:register("application/riakdoc", yz_riakdoc_extractor).