require 'aws/s3'
require 'versionomy'
require './lib/version_router'
require './lib/faqml'
require './lib/deploy'
require './lib/sitemap_render_override'

# TODO: ignore pages that are less than this version
$versions = { :riak => ENV['RIAK_VERSION'].presence }

use Rack::Middleman::VersionRouter if $versions[:riak].present?


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

# page "/tutorials/*", :layout => 'layouts/layout'

# Register the FML plugin to middleman
Middleman::Application.register Middleman::Renderers::FAQML


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


  # generate versions
  # sitemap.pages.group_by {|p| p.data["category"] }.each do |category, pages|
  #   page "/categories/#{category}.html", :proxy => "category.html" do
  #     @category = category
  #     @pages = pages
  #   end
  # end
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

  def version_bar(project)
    versions = YAML::load(File.open('versions.yml'))
    versions[project.to_s] || []
  end

  def is_version?(project, version)
    version == $versions[project.to_sym]
  end

  def api_index(api_dir_name)
    apis_path = current_path.sub(/(\w+\.html)$/, '')
    dir = sitemap.find_resource_by_destination_path("#{apis_path}#{api_dir_name}/index.html").source_file.sub(/([^\/]+)$/, '')
    groups = {}
    for file in Dir.glob("#{dir}*")
      page = sitemap.find_resource_by_path(sitemap.file_to_path(file))
      metadata = page.metadata[:page] || {}
      next if metadata["index"].to_s =~ /true/i
      (groups[metadata["group_by"]] ||= []) << page
    end
    groups
  end


  def build_nav(section, depth=1)
    nav = "<ul class=\"depth-#{depth}\">"
    section.each do |sub|
      if sub.class == String
        nav += "<li>#{sub}</li>"
      else
        nav += "<li><h4>#{sub['title']}</h4>#{build_nav(sub['sub'], depth+1)}</li>"
      end
    end
    nav += "</ul>"
    nav
  end

end

# Automatic image dimensions on image_tag helper
# activate :automatic_image_sizes

# require 'rack/codehighlighter'
# require "pygments"
# use Rack::Codehighlighter, 
#   :pygments,
#   :element => "pre>code",
#   :pattern => /\A:::([-_+\w]+)\s*\n/,
#   :markdown => true

set :css_dir, 'css'
set :js_dir, 'js'
set :images_dir, 'images'

set :markdown_engine, :redcarpet
set :markdown, :fenced_code_blocks => true,
               :smartypants => true,
               :tables => true,
               :no_intra_emphasis => true,
               :lax_html_blocks => true
               # :autolink => true,
               # :with_toc_data => true

activate :directory_indexes

# Build-specific configuration
configure :build do
  activate Middleman::Features::ProductionCheck

  # For example, change the Compass output style for deployment
  activate :minify_css
  
  # Minify Javascript on build
  activate :minify_javascript
  
  # Enable cache buster
  # activate :cache_buster
  
  # Use relative URLs
  # activate :relative_assets
  
  # Compress PNGs after build
  # require "middleman-smusher"
  # activate :smusher
  
  # Or use a different image path
  # set :http_path, "/Content/images/"

  activate Middleman::Features::Deploy
end
