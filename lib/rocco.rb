# rocco doesn't like redcarpet 2.0, but I do. so patching it
Markdown = RedcarpetCompat unless defined? Markdown
require 'rocco'

class DocRocco < Rocco

  def highlight(blocks)
    docs_blocks, code_blocks = blocks

    # Pre-process Docblock @annotations.
    docs_blocks = docblock(docs_blocks) if @options[:docblocks]

    # Combine all docs blocks into a single big markdown document with section
    # dividers and run through the Markdown processor. Then split it back out
    # into separate sections.
    markdown = docs_blocks.join("\n\n##### DIVIDER\n\n")
    docs_html = process_markdown(markdown).split(/\n*<h5>DIVIDER<\/h5>\n*/m)

    # Combine all code blocks into a single big stream with section dividers and
    # run through either `pygmentize(1)` or <http://pygments.appspot.com>
    span, espan = '<span class="c.?">', '</span>'
    if @options[:comment_chars][:single]
      front = @options[:comment_chars][:single]
      divider_input  = "\n\n#{front} DIVIDER\n\n"
      divider_output = Regexp.new(
        [ "\\n*",
          span,
          Regexp.escape(CGI.escapeHTML(front)),
          ' DIVIDER',
          espan,
          "\\n*"
        ].join, Regexp::MULTILINE
      )
    else
      front = @options[:comment_chars][:multi][:start]
      back  = @options[:comment_chars][:multi][:end]
      divider_input  = "\n\n#{front}\nDIVIDER\n#{back}\n\n"
      divider_output = Regexp.new(
        [ "\\n*",
          span, Regexp.escape(CGI.escapeHTML(front)), espan,
          "\\n",
          span, "DIVIDER", espan,
          "\\n",
          span, Regexp.escape(CGI.escapeHTML(back)), espan,
          "\\n*"
        ].join, Regexp::MULTILINE
      )
    end

    code_stream = code_blocks.join(divider_input)

    code_html = code_stream

    # code_html =
    #   if pygmentize?
    #     highlight_pygmentize(code_stream)
    #   else
    #     highlight_webservice(code_stream)
    #   end

    # Do some post-processing on the pygments output to split things back
    # into sections and remove partial `<pre>` blocks.
    code_html = process_markdown(code_html).
      split("<h1>DIVIDER</h1>").
      # split("# DIVIDER").
      map { |code| code.sub(/\n?<div class="highlight"><pre>/m, '') }.
      map { |code| code.sub(/\n?<\/pre><\/div>\n/m, '') }

    # Lastly, combine the docs and code lists back into a list of two-tuples.
    docs_html.zip(code_html)
  end

end
