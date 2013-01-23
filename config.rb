require './lib/setup'

# since we're using ERB to dynamically generate this, MM assumes it's html
page "/js/standalone/version-bar.js", :proxy => "js/standalone/version-bar.html", :directory_index => false, :ignore => true
page "/404.html", :directory_index => false


Dir["source/languages/*"].each do |language|
  language.sub!("source/languages/", '')
  ignore "/languages/#{language}/*.yml"
  next if language == I18n.locale.to_s
  ignore "/languages/#{language}/*"
  ignore "/languages/#{language}/**/*"
end

ready do
  $keyword_pages ||= BashoDocsHelpers.build_keyword_pages(sitemap)
  $keyword_pages.each do |keyword, pages|
    page "/keywords/#{keyword}/index.html", :proxy => "/keywords.html", :directory_index => true, :ignore => true do
      @keyword = keyword
      @pages = pages
    end
  end

  language = I18n.locale.to_s
  Dir["source/languages/#{language}/**/*"].each do |proxy|
    proxy = proxy.sub(/\.html.*?$/, '.html').sub(/\.(md|slim)/, '.html')
    next if proxy !~ /\.html$/
    proxy.sub!(/source\//, '')
    new_path = proxy.sub(%r"languages\/#{language}\/", '')
    if new_path =~ /(riak.*?)\-index\.html/
      project = $1
      version = $versions[project.to_sym]
      page "/#{project}/#{version}/index.html", :proxy => proxy, :directory_index => false, :ignore => true
    else
      new_path.gsub(/^\/?(riak.*?)\//, '/')
      new_path.gsub!(/\.html$/, '/index.html') unless proxy =~ /index\.html$/
      page new_path, :proxy => "/#{proxy}", :directory_index => false, :ignore => true
    end
  end

  if ENV.include?('INDEX')
    puts "== Indexing"
    build_yokozuna_index(sitemap.resources)
  end
end


helpers BashoDocsHelpers

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

activate :release_notes
activate :faqml
activate :versionify
activate :i18n, :langs => [I18n.locale]
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
    activate :s3_deploy do |s3|
      s3.access_key_id = ENV['AWS_ACCESS_KEY_ID']
      s3.secret_access_key = ENV['AWS_SECRET_ACCESS_KEY']
      s3.bucket = ENV['AWS_S3_BUCKET']
    end
    activate :invalidate_cloudfront do |cf|
      cf.access_key_id = ENV['AWS_ACCESS_KEY_ID']
      cf.secret_access_key = ENV['AWS_SECRET_ACCESS_KEY']
      cf.distribution_id = ENV['AWS_CLOUDFRONT_DIST_ID']
    end
  end
end
