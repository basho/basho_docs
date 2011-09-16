require 'pygments'

# Inspired by Albino and Gollum, this filter strips out blocks that
# start and end with "```" and then passes them to Pygments to be
# code-highlighted.
class Pygmentize < Nanoc3::Filter
  identifier :pygmentize

  def run(content, params={})
    content.gsub(/^``` ?([^\r\n]+)?\r?\n(.+?)\r?\n```\r?$/m) do
      Pygments.highlight($2, :lexer => $1, :formatter => "html")
    end    
  end
end
