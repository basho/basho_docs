require 'aws/s3'
require 'versionomy'
%w{env org versionify faqml deploy index
  sitemap_render_override duals helpers downloads}.each do |lib|
  require "./lib/#{lib}"
end

# since we're using ERB to dynamically generate this, MM assumes it's html
page "/js/standalone/version-bar.js", :proxy => "js/standalone/version-bar.html", :directory_index => false, :ignore => true
page "/404.html", :directory_index => false
$versions.each do |project, version|
  page "/#{project}/#{version}/index.html", :proxy => "/#{project}-index.html", :directory_index => false, :ignore => true
end

ready do
  $keyword_pages ||= BashoDocsHelpers.build_keyword_pages(sitemap)
  $keyword_pages.each do |keyword, pages|
    page "/keywords/#{keyword}.html", :proxy => "/keywords.html", :ignore => true do
      @keyword = keyword
      @pages = pages
    end
  end

  if ENV.include?('INDEX')
    puts "== Indexing"
    build_yokozuna_index(sitemap.resources)
  end
end

helpers BashoDocsHelpers

# Automatic image dimensions on image_tag helper
# activate :automatic_image_sizes

# set language
set :css_dir, 'css'
set :js_dir, 'js'
set :images_dir, 'images'
set :markdown_engine, :redcarpet
set :markdown, :fenced_code_blocks => true,
               :smartypants => true,
               :tables => true,
               :no_intra_emphasis => true,
               :lax_html_blocks => true

use Rack::Middleman::VersionRouter

activate :faqml
activate :directory_indexes
activate :versionify
activate :i18n
activate :cache_buster
activate :relative_assets

# Build-specific configuration
configure :build do
  activate :production_check
  activate :minify_css
  activate :minify_javascript
  # activate :gzip
  
  ignore "source/images/layout/*.png"

  activate :version_dirs

  # populate the downloads_gen data file
  if $production
    begin
      puts "== Populating Downloads Details"
      Downloads.pull_data('riak', $versions[:riak])
    rescue
      $stderr.puts "  Details download failed"
    end
  end


  if ENV.include?('DEPLOY')
    activate :s3_deploy
    activate :invalidate_cloudfront
  end

end
