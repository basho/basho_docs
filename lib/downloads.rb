require 'net/http'
require 'uri'
require 'json'

class Downloads
  BASE_DOWNLOAD_URL = "http://s3.amazonaws.com/downloads.basho.com"
  UNITS = %w{B KB MB GB TB}.freeze

  def initialize(project, version)
    @project = project
    @version = version
    @data = Downloads.data(project, version)
  end

  def [](key)
    @data[key]
  end

  def each(&block)
    @data.each(&block)
  end

  def files?(os_data)
    os_data["versions"].values.map{|v|
      v['arch'].values
    }.map{|v|
      v.map{|x| x['file']}.compact
    }.flatten.compact.present?
  end

  def file_url(os, os_version, file, static=nil)
    root_version = @version.sub(/\.\w+$/, '')
    os_path = os == 'source' ? '' : "#{os}/#{os_version}/"
    return static if static.present?
    "#{BASE_DOWNLOAD_URL}/#{@project}/#{root_version}/#{@version}/#{os_path}#{file}"
  end

  def file_size(number)
    if number.to_i < 1024
      exponent = 0
    else
      max_exp = UNITS.size - 1
      exponent = (Math.log(number) / Math.log(1024)).to_i
      exponent = max_exp if exponent > max_exp
      number /= 1024 ** exponent
    end

    "#{number} #{UNITS[exponent]}"
  end

  def self.pull_data(project, version)
    # don't pull for older versions, since they have a weird format
    return if project != 'riak-cs-control' && version.sub(/\.\w+$/, '').to_f <= 1.1
    downloads_gen = YAML::load(File.open('data/downloads_gen.yml'))
    version_data = load_from_s3(project, version)
    (downloads_gen[project] ||= {})[version] = version_data
    File.open('data/downloads_gen.yml', 'w+') do |f| f.write(downloads_gen.to_yaml) end
  end

  def self.data(project, version)
    @data ||= {}
    if @data.include?(project) && @data[project].include?(version)
      return @data[project][version]
    end
    default_data = YAML::load(File.open('data/downloads_defaults.yml'))['default']['default']
    version_data = YAML::load(File.open('data/downloads_gen.yml'))[project][version]
    default_version_data = (YAML::load(File.open('data/downloads_defaults.yml'))[project]||{})[version] || {}
    (@data[project]||={})[version] = rmerge(rmerge(default_data, version_data), default_version_data)
  end

  def self.load_from_s3(project, version)
    root_version = version.sub(/\.\w+$/, '')
    base_uri = URI(BASE_DOWNLOAD_URL)
    
    oss = {}

    # get Operating Systems
    os_ary = []
    source = nil
    begin
      oss_resp = Net::HTTP.get_response(base_uri.host, "#{base_uri.path}/#{project}/#{root_version}/#{version}/index.json")
      oss_json = extractJSON(oss_resp.body)
      os_ary = typeOnly('dir', oss_json)

      # get source from root dir
      source = fileData(oss_json)
    rescue
      $stderr.puts "Cannot find downloads for version #{version} at #{BASE_DOWNLOAD_URL}"
      return
    end

    for os in os_ary
      oss[os] ||= {'versions' => {}}
      os_resp = Net::HTTP.get_response(base_uri.host, "#{base_uri.path}/#{project}/#{root_version}/#{version}/#{os}/index.json")
      os_json = extractJSON(os_resp.body)
      osvs = typeOnly('dir', os_json)

      for osv in osvs
        oss[os]['versions'][osv] = {'arch' => {}}

        osv_resp = Net::HTTP.get_response(base_uri.host, "#{base_uri.path}/#{project}/#{root_version}/#{version}/#{os}/#{osv}/index.json")
        osv_json = extractJSON(osv_resp.body)
        files = fileData(osv_json)

        arch = {}
        for file, data in files
          arch_key = 'unknown'
          if file =~ /amd64/
            arch_key = 'amd64'
          elsif file =~ /x86_64/
            arch_key = 'x86_64'
          elsif file =~ /i386_64/
            arch_key = 'i386_64'
          elsif file =~ /i386/
            arch_key = 'i386'
          elsif file =~ /src/
            arch_key = 'src'
          end
          arch[arch_key] = data
        end

        oss[os]['versions'][osv]['arch'] = arch
      end
    end

    if source.present?
      oss['source'] = {'versions' => {'any' => {'arch' => {'src' => source.values.first}}}}
    end

    oss
  end

  def self.extractJSON(body)
    json = body.sub(/[^\{]+/, '').strip.sub(/[;]$/, '')
    JSON.parse(json)
  end

  def self.typeOnly(type, json)
    keys = []
    json.each do |key, data|
      next if data['type'] != type
      keys << key
    end
    keys
  end

  def self.fileData(json)
    fileData = {}
    json.each do |key, data|
      next if data['type'] != 'file'
      file = data['url'].to_s.scan(/[^\/]+$/).first
      if file.to_s =~ /\.sha$/
        key = file.sub(/\.sha/, '')
        (fileData[key] ||= {})['csum'] = file
        fileData[key]['static_csum'] = data['staticLink']
        fileData[key]['csum_size'] = data['size']
      else
        (fileData[file] ||= {})['file'] = file
        fileData[file]['static_file'] = data['staticLink']
        fileData[file]['file_size'] = data['size']
      end
    end
    fileData
  end

  def self.rmerge(first, second)
    merger = proc {|key,v1,v2| Hash === v1 && Hash === v2 ? v1.merge(v2, &merger) : v2 }
    first.merge(second, &merger) rescue {}
  end
end