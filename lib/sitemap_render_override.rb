class ::Middleman::Sitemap::Resource
  alias_method :old_render, :render

  def extract_name(path)
    path.to_s.scan(/([^\/]+)(?:\/|\.\w+)$/).first.first
  rescue
    path
  end

  def format_name(name)
    name.to_s.downcase.gsub(/[\s\/?]|(&mdash;)/, '-').gsub(/\-+/, '-')
  end

  def sitemap_pages
    $sitemap_pages = {}
    store.resources.each do |resource|
      # we only want "wiki" links, not images, etc
      next unless resource.url =~ /(html|[\/])$/
      name = format_name(extract_name(resource.url))
      $sitemap_pages[name] = resource.url
      title = resource.metadata[:page]["title"]
      next if title.blank?
      title = format_name(title)
      $sitemap_pages[title] = resource.url
    end
    $sitemap_pages
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
      if range.sub!(/(?:\<\=)|\-/, '')
        return version <= Versionomy.parse(range)
      # # drop bottom range and compare
      # elsif range.sub!(/\<\~/, '')
      else
        range.sub!(/[<]/, '')
        return version < Versionomy.parse(range)
      end
    # between range
    elsif range =~ /.+?\-.+?/
      a, b = range.split('-')
      return Versionomy.parse(a) >= version && version <= Versionomy.parse(b)
    end
    Versionomy.parse(range) == version
  end

  # replace [[...]] with local links, wiki-style
  def wiki_links!(data)
    data.gsub!(/\[\[([^\]]+?)(?:\|([^\]]+))?\]\]/m) do
      link_name = $2 || $1
      link_label = $1 || link_name
      anchor = nil
      link_name, anchor = link_name.split('#', 2) if link_name.include?('#')
      link_url = $sitemap_pages[format_name(link_name)]
      # heuristic that an unfound url, is probably not a link
      if link_url.blank? && link_name.scan(/[.\/]/).empty?
        "[[#{link_label}]]"
      else
        # no html inside of the link or label
        link_label.gsub!(/\<[^\>]+\>/, '_')
        link_url ||= link_name
        link_url += '#' + anchor unless anchor.blank?
        link_url.gsub!(/\<[^\>]+\>/, '_')
        "<a href=\"#{link_url}\">#{link_label}</a>"
      end
    end
  end

  def strip_versions!(data)
    project = (metadata[:page]["project"] || 'riak').to_sym
    if version_str = $versions[project]
      version = Versionomy.parse(version_str)

      # if it's a different version, remove the entire block
      data.gsub!(/\(\#\[([^\]]+)\]\)(.*?)\(\/\[([^\]]+)\]\)/m) do
        in_version_range?($1, version) ? $2 : ''
      end

      # if it's in a list in a different version, remove the entire <li></li>
      data.gsub!(/(\<li.*?\>.*?)\(\[([^\]]+)\]\)(\.*?<\/li\>)/m) do
        in_version_range?($2, version) ? $1 + $3 : ''
      end
    end
  end

  # replace all absolute links with localized links
  def localize_links!(data)
    depth_to_root = dir_depth(url)
    data.gsub!(/(\<a\s.*?href\s*\=\s*["'])(\/[^"'>]+)(["'][^\>]*>)/m) do
      # yuck.
      if $2.start_with?("/riak/")
        "#{$1}#{$2}#{$3}"
      # elsif $2.start_with?("/riakcs/")
      #   "#{$1}#{$2}#{$3}"
      else
        "#{$1}#{prepend_dir_depth($2, depth_to_root)}#{$3}"
      end
    end


    # shared resources (css, js, images, etc) are put under /shared/version
    project = (metadata[:page]["project"] || 'riak').to_sym
    if version_str = $versions[project]
      data.gsub!(/(\<(?:script|link)\s.*?(?:href|src)\s*\=\s*["'])(\/[^"'>]+)(["'][^\>]*>)/m) do
        "#{$1}/shared/#{version_str}#{$2}#{$3}"
      end

      data.gsub!(/(\<img\s.*?src\s*\=\s*["'])(\/[^"'>]+)(["'][^\>]*>)/m) do
        "#{$1}/shared/#{version_str}#{$2}#{$3}"
      end
    end
  end

  # prepends X directories from the top, eg:
  # trim_dir_depth('/a', 2) => '../../a'
  def prepend_dir_depth(path, dir_depth)
    ('../' * dir_depth) + path.sub(/^[\/]/, '')
  end

  def dir_depth(path)
    depth = path.sub(/[^\/]+\.\w+$/, '').split('/').size - 1
    depth <= 0 ? 0 : depth
  end

  def render(opts={}, locs={}, &block)
    data = old_render(opts, locs, &block)
    $sitemap_pages ||= sitemap_pages

    # process the generated html
    wiki_links!(data)
    strip_versions!(data)
    localize_links!(data)

    data
  end
end
