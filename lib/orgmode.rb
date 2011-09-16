require 'org-ruby'

class OrgMode < Nanoc3::Filter
  identifier :orgmode

  def run(content, params={})
    OrgRuby::Parser.new(content).to_html
  end
end
