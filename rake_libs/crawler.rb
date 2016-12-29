require "mechanize"
require "open-uri"
require "find"
require "pp"
require "progressbar"
require "yaml"

class Crawler

  # The root URL HTTP requests are made to.
  # Defaults to http://localhost:1313/.
  # @return #<String>
  attr_reader :base_url

  # The directory of pages to crawl.
  # Defaults to './content' or all pages.
  # @return #<String>
  attr_reader :target_dir

  # Mechanize agent used to crawl web pages.
  # @return #<Mechanize>
  attr_reader :agent

  def initialize(directory="./content")
    @target_dir = directory
    @base_url = "http://localhost:1313/"
    @agent = Mechanize.new
  end

  # Generate list of pages to crawl and convert filenames into URls.
  # @return #<Array[<URI::HTTP>]>
  def queue
    @queue ||= content_file_paths.map { |e| url_from_file_name(e) }
  end

  # Get page from a given URL
  # @return #<Mechanize::Page>
  def get_page(url)
    @agent.get(url)
  end

  # Make head request to URL
  # @return #<Mechanize::Page>
  def check_page(url)
    @agent.head(url)
  end

  # Returns all links from a given page.
  # Each link hash contains the original href [:href],
  # and a resolved URI [:resolved_url] to solve relative URL issues.
  # @return #<Array[#<Hash>]>
  def get_links(page)
    page.links.delete_if { |e|
      e.href == nil
    }.map { |e| {href: e.href, resolved_url: e.resolved_uri} }.compact.uniq
  end

  # Get list of markdown pages in the @target_dir, ignores drafts.
  # @return #<Array[#<String>]>
  def content_file_paths
    Find.find(@target_dir).map {|path|
      FileTest.file?(path) && (path =~ /\.md?$/) ? path : nil
    }.reject{|path| path.nil? }.reject{|path|
      path.include?("_reference-links") }.reject{|path|
        YAML.load_file(path)["draft"]}
  end

  # Convert markdown filename into URL.
  # @return #<URI::HTTP>
  def url_from_file_name(filename)
    url = filename.sub!("./content", "").sub!(".md", "")
    URI.join(@base_url, url)
  end

  # Crawls all pages in the queue and checks each for 404s.
  def run
    queue.each do |url|
      page = get_page(url)
      links = get_links(page)
      puts "#{page.uri}"
      links.each do |link|
        # To avoid HTTP 429 code
        if !link[:resolved_url].to_s.include?(@base_url)
          sleep 1
        end
        begin
          response = check_page(link[:resolved_url])
        rescue Mechanize::ResponseCodeError => error
          response = error
          puts "-- #{error.response_code} -- #{link[:href]}"
        end
      end
      puts "\n"
    end
  end
end
