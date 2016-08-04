# Taken from https://gist.github.com/lukebayes/215270

##################################
# MIT LICENSE
# Copyright (C) 2012 Luke Bayes
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


require 'erb'

# DynamicAccessors source file found here: http://gist.github.com/215257
require File.dirname(__FILE__) + '/dynamic_accessors'

module Sprout # :nodoc

  # Rake task that makes it stupid-easy to render ERB templates
  # to disk. Just add parameters to the yielded object, and
  # they will be available to your Template.
  #
  #   erb_resolver 'config/SomeFile.xml' do |t|
  #     t.param1 = 'value'
  #     t.other_param = 'other value'
  #     t.another_param = ['a', 'b', 'c']
  #     t.template = 'config/SomeFile.xml.erb' # Optional - will automatically look here...
  #   end
  #
  class ERBResolver < Rake::FileTask
    include DynamicAccessors

    # File to create from the rendered
    # ERB template. This value will
    # default to the Task name in order
    # to be consistent with other FileTasks
    attr_accessor :output

    # Path to the input ERB template. This
    # value will default to the value of
    # "#{ERBResolver.output}.erb"
    attr_accessor :template

    def initialize(name, app) # :nodoc:
      super
    end

    def define
    end

    def prepare
      prepare_prerequisites
    end

    def execute(*args)
      super
      content = nil
      File.open(template, 'r') { |f| content = f.read }
      result = ERB.new(content, nil, '>').result(binding)
      File.open(output, 'w') { |f| f.write(result) }
      puts ">> Created ERB output at: #{output} from: #{template}"
    end

    def self.define_task(args, &block)
      t = super
      if(t.is_a?(ERBResolver))
        yield t if block_given?
        t.define
        t.prepare
      end
      return t
    end

    def template
      @template ||= "#{output}.erb"
    end

    def output
      @output ||= self.name
    end

    protected

    def prepare_prerequisites
      prerequisites << file(template)
      CLEAN.add(output)
    end

  end
end

def erb_resolver(args, &block)
  Sprout::ERBResolver.define_task(args, &block)
end
