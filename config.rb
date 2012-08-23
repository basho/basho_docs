### 
# Compass
###

# Susy grids in Compass
# First: gem install compass-susy-plugin
# require 'susy'

# Change Compass configuration
# compass_config do |config|
#   config.output_style = :compact
# end

###
# Page options, layouts, aliases and proxies
###

# Per-page layout changes:
# 
# With no layout
# page "/path/to/file.html", :layout => false
# 
# With alternative layout
# page "/path/to/file.html", :layout => :otherlayout
# 
# A path which all have the same layout
# with_layout :admin do
#   page "/admin/*"
# end

# Proxy (fake) files
# page "/this-page-has-no-template.html", :proxy => "/template-file.html" do
#   @which_fake_page = "Rendering a fake page with a variable"
# end

# Register the FML plugin to middleman
module Middleman::Renderers::FAQML
  def registered(app)
    # FAQML is not included in the default gems,
    # but we'll support it if available.
    begin
      # Load gem
      require "faqml"

      app.before_configuration do
        template_extensions :fml => :html
      end

      # Setup FAQML options to work with partials
      ::FAQML::Engine.set_default_options(
        :buffer    => '@_out_buf', 
        :generator => ::Temple::Generators::StringBuffer
      )
    rescue LoadError
    end
  end
end
Middleman::Application.register Middleman::Renderers::FAQML

# HACK: patch until we can ensure FML is a valid current_resource
# module Middleman::CoreExtensions::DefaultHelpers::Helpers
#   def current_resource
#     sitemap.find_resource_by_destination_path(current_path) || sitemap.resources.first
#   end
# end

class ::Middleman::Sitemap::Resource
  alias_method :old_render, :render

  def extract_name(path)
    path.to_s.scan(/([^\/]+)(?:\/|\.\w+)$/).first.first
  end

  def format_name(name)
    name.to_s.downcase.gsub(/\s/, '-')
  end

  def sitemap_pages
    $sitemap_pages = {}
    store.resources.each do |resource|
      name = format_name(extract_name(resource.url))
      $sitemap_pages[name] = resource.url
      title = resource.metadata[:page]["title"]
      next if title.blank?
      title = format_name(title)
      $sitemap_pages[title] = resource.url
    end
    $sitemap_pages
  end

  def render(opts={}, locs={}, &block)
    data = old_render
    $sitemap_pages ||= sitemap_pages
    # name = format_name(extract_name(self.url))
    data.gsub!(/\[\[([^\]]+?)(?:\|([^\]]+))?\]\]/m) do
      link_name = $2 || $1
      link_label = $1 || link_name
      anchor = nil
      link_name, anchor = link_name.split('#', 2) if link_name.include?('#')
      link_url = $sitemap_pages[format_name(link_name)]
      # heuristic that an unfound url, is probably not a link
      # if link_url.blank? && $1 == $2 && !link_name.include?('.')
      if link_url.blank? && link_name.scan(/[.\/]/).empty?
        "[[#{link_label}]]"
      else
        link_url ||= link_name
        link_url += '#' + anchor unless anchor.blank?
        "<a href=\"#{link_url}\">#{link_label}</a>"
      end
    end
    data
  end

end

#############
# override tha languages to manage versions!?

def build_keyword_pages
  keyword_pages = {}
  sitemap.resources.each do |resource|
    keywords = resource.metadata[:page]["keywords"]
    next if keywords.blank?
    keywords.each do |keyword|
      (keyword_pages[keyword] ||= []) << resource
    end
  end
  keyword_pages
end

ready do
  $keyword_pages ||= build_keyword_pages
  $keyword_pages.each do |keyword, pages|
    page "/keywords/#{keyword}.html", :proxy => "/keywords.html", :ignore => true do
      @keyword = keyword
      @pages = pages
    end
  end
end


###
# Helpers
###

helpers do
  # get all pages that match this page's keywords
  def similar_pages(page)
    return [] if page.blank?
    $keyword_pages ||= build_keyword_pages
    keywords = page.metadata[:page]["keywords"] || []
    pages = Set.new
    for keyword in keywords
      pages += $keyword_pages[keyword] || []
    end
    pages.delete_if{|g| g.url == page.url }.to_a
  end
end

# Automatic image dimensions on image_tag helper
# activate :automatic_image_sizes

set :css_dir, 'css'
set :js_dir, 'js'
set :images_dir, 'images'

set :markdown_engine, :redcarpet
set :markdown, :fenced_code_blocks => true,
               # :autolink => true, 
               :smartypants => true,
               :with_toc_data => true

# Build-specific configuration
configure :build do
  # For example, change the Compass output style for deployment
  activate :minify_css
  
  # Minify Javascript on build
  activate :minify_javascript
  
  # Enable cache buster
  # activate :cache_buster
  
  # Use relative URLs
  # activate :relative_assets
  
  # Compress PNGs after build
  require "middleman-smusher"
  activate :smusher
  
  # Or use a different image path
  # set :http_path, "/Content/images/"
end
