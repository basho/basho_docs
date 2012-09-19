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
    body = File.read(resource.source_file)
    data = body

    # # remove the top metadata
    # data.sub!(/^\-{3}$.*?^\-{3}$/m, '')

    # xml = Builder::XmlMarkup.new(:target=>'')
    # # xml.instruct!(:xml, :encoding => "UTF-8")
    # xml.add do |add|
    #   add.doc do |doc|
    #     doc.field({:name=>'title'}, metadata['title'])
    #     doc.field({:name=>'project'}, metadata['project'])
    #     doc.field({:name=>'doctype'}, metadata['document'])
    #     doc.field({:name=>'index'}, metadata['index'] == 'true')
    #     doc.field({:name=>'audience'}, metadata['audience'])
    #     doc.field({:name=>'keywords'}, (metadata['keywords']||[]).join(','))
    #     doc.field({:name=>'body'}, body)
    #   end
    # end
    # data = xml.target!

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