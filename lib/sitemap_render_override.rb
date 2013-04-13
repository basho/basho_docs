require 'versionomy'
require 'coderay'
require 'cgi'

module SitemapRenderOverride

  @@_current_version = nil
  
  def self.current_version=(v)
    @@_current_version = v
  end

  def self.current_version
    @@_current_version
  end

  def sitemap_pages
    return $sitemap_pages if $sitemap_pages
    $sitemap_pages = {}
    source = defined?(store) ? store : sitemap
    source.resources.each do |resource|
      # we only want "wiki" links, not images, etc
      next unless resource.url =~ /(html|[\/])$/
      name = format_name(extract_name(resource.url))
      project = resource.metadata[:page]["project"]
      value = {:url => resource.url, :project => project}
      $sitemap_pages[name] ||= value
      title = resource.metadata[:page]["title"]
      next if title.blank?
      title = format_name(title)
      $sitemap_pages[title] = value
    end
    $sitemap_pages
  end

  def sitemap_page_key(page)
    name = format_name(page.metadata[:page]["title"]).presence
    name ||= format_name(extract_name(page.url))
    name
  end

  def extract_name(path)
    path.to_s.scan(/([^\/]+)(?:\/|\.\w+)$/u).first.first
  rescue
    path
  end

  def format_name(name)
    name.to_s.downcase.gsub(/[\s\/?]|(&mdash;)/, '-').gsub(/\-+/, '-')
  end

  def in_version_range?(range, version)
    range = range.gsub(/\s/, '')
    
    # greater than range
    if range =~ /\+$/ || range =~ /^\>/
      if range.sub!(/(?:\>\=)|\+/, '')
        return version >= Versionomy.parse(range)
      # # drop bottom range and compare
      # elsif range.sub!(/\>\~/, '')
      else
        range.sub!(/[>]/, '')
        return version > Versionomy.parse(range)
      end
    # less than range
    elsif range =~ /\-$/ || range =~ /^\</
      if range.sub!(/(?:\<\=)/, '')
        return version <= Versionomy.parse(range)
      # # drop bottom range and compare
      # elsif range.sub!(/\<\~/, '')
      else
        range.sub!(/[<]|[-]/, '')
        return version < Versionomy.parse(range)
      end
    # between range
    elsif range =~ /.+?\-.+?/
      a, b = range.split('-')
      return Versionomy.parse(a) >= version && version <= Versionomy.parse(b)
    end
    Versionomy.parse(range) == version
  end

  # prepends X directories from the top, eg:
  # trim_dir_depth('/a', 2) => '../../a'
  def prepend_dir_depth(path, dir_depth)
    ('../' * dir_depth) + path.sub(/^[\/]/, '')
  end

  def dir_depth(path)
    # puts path
    depth = path.sub(/[^\/]+\.\w+$/, '').split('/').size - 1
    depth = 0 if path =~ /\/((?:#{projects_regex})\/[^\/]+)\/?(index\.html)?$/
    depth <= 0 ? 0 : depth
  end

  # replace [[...]] with local links, wiki-style
  def wiki_links!(data)
    data.gsub!(/\[\[([^\]]+?)(?:\|([^\]]+))?\]\]/um) do
      link_name = $2 || $1
      link_label = $1 || link_name
      anchor = nil
      link_name, anchor = link_name.split('#', 2) if link_name.include?('#')
      link_data = $sitemap_pages[format_name(link_name)] || {}
      # heuristic that an unfound url, is probably not a link
      link_url = link_data[:url]
      if link_url.blank? && link_name !~ /^([.]?\/|https?\:)/
        "[[#{link_label}]]"
      else
        # no html inside of the link or label
        link_label.gsub!(/\<[^\>]+\>/u, '_')
        link_url ||= link_name
        link_url += '#' + anchor unless anchor.blank?
        link_url.gsub!(/\<[^\>]+\>/u, '_')
        link_project = link_data[:project] || $default_project
        "<a href=\"#{link_url}\" class=\"#{link_project}\">#{link_label}</a>"
      end
    end
  end

  def strip_versions!(data)
    project = (metadata[:page]["project"] || $default_project).to_sym
    
    version_str = SitemapRenderOverride.current_version || $versions[project]

    if version_str
      # Ignore rcX if this is a pre-release
      version_str = version_str.sub(/rc\d+/i, '')
      version = Versionomy.parse(version_str)

      # if it's a different version, remove the entire block
      data.gsub!(/\{\{\#([^\}]+)\}\}(.*?)\{\{\/(\1)\}\}/m) do
        liversion, block = $1, $2
        liversion = liversion.sub(/\&lt\;/, '<').sub(/\&gt\;/, '>')
        if in_version_range?(liversion, version)
          # nested version block
          block.gsub(/\{\{\#([^\}]+)\}\}(.*?)\{\{\/(\1)\}\}/m) do
            liversion2, block2 = $1, $2
            liversion2 = liversion2.sub(/\&lt\;/, '<').sub(/\&gt\;/, '>')
            if in_version_range?(liversion2, version)
              block2
            else
              ''
            end
          end
        else
          ''
        end
      end

      # if it's in a list in a different version, remove the entire <li></li>
      # data.gsub!(/(\<li(?:\s[^\>]*?)?\>.*?)\{\{([^\}]+?)\}\}(.*?<\/li\>)/) do
      data.gsub!(/(\<li(?:\s[^\>]*?)?\>(?:(?!\<li).)*?)\{\{([^\}]+?)\}\}(.*?<\/li\>)/m) do
        startli, liversion, endli = $1, $2, $3
        liversion = liversion.sub(/\&lt\;/, '<').sub(/\&gt\;/, '>')
        if liversion =~ /^(?:[\<\>][\=]?)?[\d\.\-rc]+?[\+\-]?$/
          in_version_range?(liversion, version) ? startli + endli : ''
        else
          startli + endli
        end
      end

      data.gsub!(/(\<tr(?:\s[^\>]*?)?\>(?:(?!\<tr).)*?)\{\{([^\}]+?)\}\}(.*?<\/tr\>)/m) do
        startli, liversion, endli = $1, $2, $3
        liversion = liversion.sub(/\&lt\;/, '<').sub(/\&gt\;/, '>')
        if liversion =~ /^(?:[\<\>][\=]?)?[\d\.\-rc]+?[\+\-]?$/
          in_version_range?(liversion, version) ? startli + endli : ''
        else
          startli + endli
        end
      end
    end
  end

  def extract_classes(anchor)
    (anchor.scan(/class\s*\=\s*['"]([^'"]+)['"]/).first || []).first.to_s.split
  end

  # replace all absolute links with localized links
  # except in the case of cross projects
  def localize_links!(data)
    depth_to_root = dir_depth(url)
    project = (metadata[:page]["project"] || 'riak').to_sym
    version_str = $versions[project]

    # data.gsub!(/(\<a\s.*?href\s*\=\s*["'])(\/[^"'>]+)(["'][^\>]*?>)/m) do
    data.gsub!(/\<a\s+([^\>]*?)\>/mu) do
      anchor = $1
      
      href = (anchor.scan(/href\s*\=\s*['"]([^'"]+)['"]/u).first || []).first.to_s

      # XXX: This is a terrible way to get the # links in the API to work
      if url =~ /\/http\/single/ || url =~ /\/references\/dynamo/
        if href =~ /^\#/
          next "<a #{anchor}>"
        end
      end

      # if this is data, make absolute, not relative
      if href =~ /^\/data\/(.+)$/
        url = "/shared/#{version_str}/data/#{$1}"
        next "<a #{anchor.gsub(href, url)}>"
      end

      match_index = href =~ /^\/(#{projects_regex})\/[\d\.rc]+\/$/

      # force the root page to point to the latest projcets
      if $production && project == :root && match_index
        "<a href=\"/#{$1}/latest/\">"
      # /riak*/version/ links should be relative, unless they cross projects
      elsif match_index #href =~ /^\/(riak[^\/]*?)\/[\d\.rc]+\/$/
        if ($1 || $default_project).to_sym == project
          url = prepend_dir_depth('', depth_to_root)
          "<a #{anchor.gsub(href, url)}>"
        else
          "<a #{anchor}>"
        end
      # keep it the same
      elsif version_str.blank? || href.blank? || href =~ /^http[s]?\:/
        "<a #{anchor}>"
      elsif href =~ /^\/index\.html$/
        "<a #{anchor}>"
      else
        classes = extract_classes(anchor)

        link_project = ($versions.keys.find{|proj| classes.include?(proj.to_s)} || $default_project).to_sym

        # make it absolute if outside this project, otherwise relative
        if link_project != project
          # proj_str = $versions[link_project] || version_str
          # url = "/#{link_project}/#{proj_str}#{href}"
          url = "/#{link_project}/latest#{href}"
        elsif classes.include?('versioned')
          next "<a #{anchor}>"
        else
          url = prepend_dir_depth(href, depth_to_root)
        end
        "<a #{anchor.gsub(href, url)}>"
      end
    end

    # shared resources (css, js, images, etc) are put under /shared/version
    if version_str || project == :root
      version_str ||= $versions[:riak]
      data.gsub!(/(\<(?:script|link)\s.*?(?:href|src)\s*\=\s*["'])([^"'>]+)(["'][^\>]*>)/mu) do
        base, href, cap = $1, $2, $3
        href.gsub!(/\.{2}\//, '')
        href = "/" + href unless href =~ /^\//
        
        # A better way to extract this file?
        if href =~ /\/standalone/
          "#{base}#{href}#{cap}"
        else
          "#{base}/shared/#{version_str}#{href}#{cap}"
        end
      end

      data.gsub!(/(\<img\s.*?src\s*\=\s*["'])([^"'>]+)(["'][^\>]*>)/mu) do
        base, href, cap = $1, $2, $3
        if href =~ /^http[s]?\:/
          "#{base}#{href}#{cap}"
        else
          href.gsub!(/\.{2}\//, '')
          href = "/" + href unless href =~ /^\//
          "#{base}/shared/#{version_str}#{href}#{cap}"
        end
      end
    end
  end

  def colorize_code!(data)
    data.gsub!(/\<pre(?:\s.*?)?\>\s*\<code(?:\s.*?)?(class\s*\=\s*["'][^"'>]+["'])?[^\>]*\>(.*?)\<\/code\>\s*<\/pre\>/mu) do
      code = $2
      given_code_type = $1.to_s.sub(/class\s*\=\s*["']([^"'>]+)["'][^\>]*/u, "\\1")
      code_type = (given_code_type.presence || :text).to_s.to_sym
      # these are unfortunate hacks to deal with an incomplete coderay
      code_type = code_type == :bash ? :php : code_type
      code_type = code_type == :erlang ? :python : code_type
      code = CodeRay.scan(CGI.unescapeHTML(code), code_type).div #(:css => :class)
      code
    end
  end

  def process_data!(data)
    $sitemap_pages ||= sitemap_pages

    # process the generated html
    wiki_links!(data)
    strip_versions!(data)
    localize_links!(data)
    colorize_code!(data)
  rescue => e
    $stderr.puts e
  ensure
    return data
  end
end


class ::Middleman::Sitemap::Resource
  include SitemapRenderOverride

  alias_method :old_render, :render

  # accepts the rendered data, and then does some crazy shit to it
  def render(opts={}, locs={}, &block)
    data = old_render(opts, locs, &block)
    process_data!(data)
  end
end
