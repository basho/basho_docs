require 'org-ruby'

# UGLY MONKEY-PATCH TO GET MULTIPLE HEADER ROWS
module Orgmode
  class Parser
    def initialize(lines, offset=0)
      if lines.is_a? Array then
        @lines = lines
      elsif lines.is_a? String then
        @lines = lines.split("\n")
      else
        raise "Unsupported type for +lines+: #{lines.class}"
      end

      @custom_keywords = []
      @headlines = Array.new
      @current_headline = nil
      @header_lines = []
      @in_buffer_settings = { }
      @options = { }
      mode = :normal
      previous_line = nil
      # table_header_set = false
      @lines.each do |text|
        line = Line.new text, self

        case mode
        when :normal

          if (Headline.headline? line.line) then
            @current_headline = Headline.new line.line, self, offset
            @headlines << @current_headline
          else
            # If there is a setting on this line, remember it.
            line.in_buffer_setting? do |key, value|
              store_in_buffer_setting key, value
            end
            if line.table_separator? then
              if previous_line and previous_line.paragraph_type == :table_row #and !table_header_set
                previous_line.assigned_paragraph_type = :table_header
                # table_header_set = true
              end
            end
            # table_header_set = false if !line.table?
            mode = :code if line.begin_block? and line.block_type.casecmp("EXAMPLE") == 0
            mode = :src_code if line.begin_block? and line.block_type.casecmp("SRC") == 0
            mode = :block_comment if line.begin_block? and line.block_type == "COMMENT"
            mode = :property_drawer if line.property_drawer_begin_block?
            if (@current_headline) then
              @current_headline.body_lines << line
            else
              @header_lines << line
            end
          end

        when :block_comment

          if line.end_block? and line.block_type == "COMMENT"
            mode = :normal
          else
            line.assigned_paragraph_type = :comment
          end

        when :code

          # As long as we stay in code mode, force lines to be either blank or paragraphs.
          # Don't try to interpret structural items, like headings and tables.
          if line.end_block? and line.code_block?
            mode = :normal
          else
            line.assigned_paragraph_type = :paragraph unless line.blank?
          end
          if (@current_headline) then
            @current_headline.body_lines << line
          else
            @header_lines << line
          end

        when :src_code

          if line.end_block? and line.code_block?
            mode = :normal
          else
            line.assigned_paragraph_type = :src
          end
          if (@current_headline) then
            @current_headline.body_lines << line
          else
            @header_lines << line
          end

        when :property_drawer

          if line.property_drawer_end_block?
            mode = :normal
          else
            line.assigned_paragraph_type = :property_drawer unless line.blank?
          end
          if (@current_headline) then
            @current_headline.body_lines << line
          else
            @header_lines << line
          end
        end                     # case

        previous_line = line
      end                       # @lines.each
    end
  end
end
