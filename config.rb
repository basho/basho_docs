require 'aws/s3'
require 'versionomy'
require './lib/versionify'
require './lib/faqml'
require './lib/rocco'
require './lib/deploy'
require './lib/index'
require './lib/sitemap_render_override'

DEFAULT_VERSION = '1.2.0'

$versions = {
  :riak => ENV['RIAK_VERSION'].presence,
  :riakcs => ENV['RIAKCS_VERSION'].presence || ENV['RIAK_VERSION'].presence,
  :riakee => ENV['RIAKEE_VERSION'].presence || ENV['RIAK_VERSION'].presence
}

use Rack::Middleman::VersionRouter #if $versions[:riak].present?

# this is not optimal. Hook it into the "watch" mechanism
puts "== Generating API"
for api in Dir.glob("**/*.api")
  r = Rocco.new(api, [], :language => 'bash', :template_file => './source/layouts/api.mustache') #{File.read(api)}
  File.open(api.sub(/\.api$/, '.html.erb'), 'w') do |html|
    html.write(r.to_html)
  end
end
for api in Dir.glob("**/*.roc")
  r = DocRocco.new(api, [], :language => 'bash', :template_file => './source/layouts/roc.mustache') #{File.read(api)}
  File.open(api.sub(/\.roc$/, '.html.erb'), 'w') do |html|
    html.write(r.to_html)
  end
end

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

page "/404.html", :directory_index => false

# Proxy (fake) files
# page "/this-page-has-no-template.html", :proxy => "/template-file.html" do
#   @which_fake_page = "Rendering a fake page with a variable"
# end

# page "/tutorials/*", :layout => 'layouts/layout'

# Register the FML plugin to middleman
Middleman::Application.register Middleman::Renderers::FAQML
# Middleman::Application.register Middleman::Renderers::Rocco


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

  # for api in Dir.glob("**/*.api")
  #   page api.sub(/\.?\/?source/, '').sub(/\.api$/, '.html'), :layout => false
  # end

  # for api in Dir.glob("**/*.roc")
  #   page api.sub(/\.?\/?source/, '').sub(/\.roc$/, '.html'), :layout => false
  # end

  if ENV.include?('INDEX')
    puts "== Indexing"
    build_yokozuna_index(sitemap.resources)
  end
end

###
# Helpers
###

helpers do
  include ::SitemapRenderOverride

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

  def current_version(default_proj='riak')
    $versions[(data.page.project || default_proj).to_sym] || DEFAULT_VERSION
  end

  def project_version_path(page)
    project = (page.metadata[:page] || {})['project'] || 'riak'
    version = current_version(project)
    current_project = (current_page.metadata[:page] || {})['project'] || 'riak'
    url = page.url.sub(/\.html/, '/')
    if project != current_project
      url = "/#{project}/#{version}#{url}"
    end
    url
  end

  def version_bar(project)
    versions = YAML::load(File.open('data/versions.yml'))
    versions[project.to_s] || []
  end

  def is_version?(project, version)
    version == $versions[project.to_sym]
  end

  def api_index(api_dir_name)
    apis_path = current_path.sub(/(\w+\.html)$/, '')
    # apis_path = "/references/apis/"
    dir = sitemap.find_resource_by_destination_path("#{apis_path}#{api_dir_name}/index.html").source_file.sub(/([^\/]+)$/, '')
    groups = {}
    for file in Dir.glob("#{dir}*")
      next if file =~ /(?:html|:api)$/
      page = sitemap.find_resource_by_path(sitemap.file_to_path(file))
      metadata = page.metadata[:page] || {}
      next if metadata["index"].to_s =~ /true/i
      (groups[metadata["group_by"]] ||= []) << page
    end
    groups
  end

  def current_projects()
    projects = {}
    data.versions.each do |project, versions|
      projects[project] = {
        :deployment => $versions[project.to_sym],
        :latest => versions.last.last
      }
    end
    projects
  end

  # used to convert global nav wiki links into real links
  # TODO: extract this into reuable method
  def wiki_to_link(wiki_link)
    link_found = ($wiki_links ||= {})[wiki_link]
    return link_found if link_found
    if wiki_link =~ (/\[\[([^\]]+?)(?:\|([^\]]+))?\]\]/)
      link_name = $2 || $1
      link_label = $1 || link_name
      anchor = nil
      link_name, anchor = link_name.split('#', 2) if link_name.include?('#')
      sitemap_key = format_name(link_name)
      link_data = sitemap_pages[sitemap_key] || {}
      # heuristic that an unfound url, is probably not a link
      link_url = link_data[:url]
      unless link_url.blank? && link_name.scan(/[.\/]/).empty?
        # no html inside of the link or label
        link_label.gsub!(/\<[^\>]+\>/, '_')
        link_url ||= link_name
        link_url += '#' + anchor unless anchor.blank?
        link_url.gsub!(/\<[^\>]+\>/, '_')
        return $wiki_links[wiki_link] = {:name => link_label, :url => link_url, :key => sitemap_key}
      end
    end
    {}
  end

  # generate reverse breadcrumbs
  def build_breadcrumbs(parent, searching)
    parent.each do |child|
      if child.class == String
        link_data = wiki_to_link(child)
        return [{}] if searching == link_data[:key]
      elsif child.include?('title')
        link_data = wiki_to_link(child['title'])
        if (response = build_breadcrumbs(child['sub'], searching)).present?
          return [link_data] + response
        elsif searching == link_data[:key]
          return [{}]
        end
      end
    end
    []
  end

  def bread_crumbs(page)
    return [] if page.blank?
    page_key = sitemap_page_key(page)
    project = page.metadata[:page]["project"] || 'riak'
    build_breadcrumbs(data.global_nav[project], page_key)
  end

  def build_nav(section, depth=1)
    active = false
    nav = ''
    section.each do |sub|
      if sub.class == String
        link_data = wiki_to_link(sub)
        current_link = link_data[:url] == current_page.url
        active ||= current_link
        nav += "<li#{current_link ? ' class="active current"' : ''}>#{sub}</li>"
      else
        nested, sub_active = build_nav(sub['sub'], depth+1)
        link_data = wiki_to_link(sub['title'])
        current_link = link_data[:url] == current_page.url
        active ||= sub_active || current_link
        active_class = active ? ' class="active"' : ''
        current_class = current_link ? ' class="active current"' : ''
        nav += "<li#{active_class}><h4#{current_class}><span>#{sub['title']}</span></h4>#{nested}</li>"
      end
    end
    nav = "<ul class=\"depth-#{depth} #{active ? 'active' : ''}\">" + nav + "</ul>"
    [nav, active]
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
activate :versionify

%w{riak riakcs riakee}.each do |project|
  version = $versions[project.to_sym] || DEFAULT_VERSION
  page "/#{project}/#{version}/index.html", :proxy => "/#{project}-index.html", :directory_index => false, :ignore => true
end

activate :cache_buster
activate :relative_assets

# Build-specific configuration
configure :build do
  activate Middleman::Features::ProductionCheck

  activate :minify_css
  activate :minify_javascript
  # activate :gzip
  
  # activate :cache_buster
  # activate :relative_assets
  
  # Compress PNGs after build
  # require "middleman-smusher"
  # activate :smusher
  
  ignore "source/images/layout/*.png"

  activate :version_dirs

  if ENV.include?('DEPLOY')
    activate :s3_deploy
    activate :invalidate_cloudfront
  end

end
