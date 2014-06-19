def generate_index_markdown
  markdown_string = String.new
  markdown_string.concat("Welcome to the Riak Docs!\n")
  riak_nav = YAML.load_file('source/languages/en/global_nav.yml')['riak']
  Array(0..riak_nav.length - 1).each do |section|
    markdown_string.concat("\n## #{riak_nav[section]['title']}\n\n")
    subsections = riak_nav[section]['sub']
    subsections.each do |subsection|
			if subsection.is_a? String
				markdown_string.concat("#{subsection}\n")
			elsif subsection.is_a? Hash
				subsection['sub'].each do |ss|
					if ss.is_a? String
						markdown_string.concat("#{ss}\n")
					end
				end
			end
		end
	end
	p markdown_string
	File.write('index.md', markdown_string)
end
generate_index_markdown