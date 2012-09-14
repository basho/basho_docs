
ROOT_URL = "http://localhost:8091/riak/docs"

def make_riak_key(resource)
  resource.url.gsub(/(^\/)|(\.html$)/, '').gsub(/[\/\-\.]+/, '_')
end

# we only index html pages
def build_yokozuna_index(resources)
  for resource in resources
    next unless resource.url =~ /\.(?:html)$/
    next if %w{/404.html /index.html}.include?(resource.url)

    metadata = resource.metadata[:page] || {}
    metadata['url'] = resource.url.sub(/\.html/, '/')

    key = make_riak_key(resource)
    data = File.read(resource.source_file)

    command = <<-CURL
    curl '#{ROOT_URL}/#{key}' \
    -H 'content-type:text/plain' -X PUT --data-binary @-<<\\YNFCM
    #{data}
    YNFCM
    CURL

    puts "  Indexing #{key}"
    %x"#{command}"
  end
end