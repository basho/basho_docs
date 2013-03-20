# ROOT_URL = "http://ec2-54-242-92-147.compute-1.amazonaws.com:8098/riak/riakdoc2"
ROOT_URL = "http://localhost:8091/riak/rdt1"

def make_riak_key(resource, project)
  url = resource.url.gsub(/(^\/)|(\.html$)/, '/')
  url = "/#{project}/latest#{url}"
  url.gsub(/[\/]+/, '%2F')
end

# we only index html pages
def build_yokozuna_index(resources)
  count = 0
  for resource in resources
    next unless resource.url =~ /\.(?:html)|\/$/
    next if resource.url =~ /keywords/
    next if %w{/404.html /index.html}.include?(resource.url)

    metadata = resource.metadata[:page] || {}
    metadata['url'] = resource.url.sub(/\.html/, '/')

    key = make_riak_key(resource, metadata['project'] || $default_project)
    body = File.read(resource.source_file)
    data = body

    # remove the top metadata
    # data.sub!(/^\-{3}$.*?^\-{3}$/m, '')
    # data = metadata['title'].to_s + data

    # inject into header yaml
    url = metadata['url']

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

    # application/riakdoc

    # -H 'X-Riak-Title:#{metadata['title']}' \
    # -H 'content-type:text/plain' \

    # strip out keywords if empty
    data.sub!(/keywords\:\s*\[\]/, '')

    command = <<-CURL
    curl -XPUT '#{ROOT_URL}/#{key}' \
    -H 'content-type:application/riakdoc' \
    --data-binary @-<<\\YNFCM
#{data}
    YNFCM
    CURL

    # if (count += 1) == 4
    #   puts command
    #   return
    # end

    puts "  Indexing #{key}"
    %x"#{command}"
  end
end
# yz_extractor:register("application/riakdoc", yz_riakdoc_extractor).