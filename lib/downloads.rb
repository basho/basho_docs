require 'net/http'
require 'uri'
require 'json'

class Downloads
  @base_url = "http://s3.amazonaws.com/downloads.basho.com"

  def initialize(project, version)
    @data = Downloads.data(project, version)
    p @data
  end

  def [](key)
    @data[key]
  end

  def each(&block)
    @data.each(&block)
  end

  def self.data(project, version)
    return @data if defined?(@data)
    default_data = YAML::load(File.open('data/downloads_defaults.yml'))[project]['default']
    version_data = load_from_s3(project, version)
    # version_data = YAML::load(File.open('data/downloads_gen.yml'))[project][version]
    @data = rmerge(version_data, default_data)
  end

  def self.load_from_s3(project, version)
    root_version = version.sub(/\.\w+$/, '')
    base_uri = URI(@base_url)
    
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
      $stderr.puts "Cannot find downloads for version #{version} at #{@base_url}"
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
        fileData[key]['csum_size'] = data['size']
      else
        (fileData[file] ||= {})['file'] = file
        fileData[file]['file_size'] = data['size']
      end
    end
    fileData
  end

  def self.rmerge(first, second)
    merger = proc {|key,v1,v2| Hash === v1 && Hash === v2 ? v1.merge(v2, &merger) : v2 }
    first.merge(second, &merger)
  end
end