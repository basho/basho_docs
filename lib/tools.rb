require 'versionomy'

def projects_regex
  @projects_regex ||= $versions.keys.sort{|a,b| b.length <=> a.length}.map{|j| j.to_s.gsub(/(\W)/){'\\'+$1}}.join("|")
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
