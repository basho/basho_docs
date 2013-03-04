
def projects_regex
  @projects_regex ||= $versions.keys.sort{|a,b| b.length <=> a.length}.map{|j| j.to_s.gsub(/(\W)/){'\\'+$1}}.join("|")
end