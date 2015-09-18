# Taken from https://gist.github.com/lukebayes/215257

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
#
#
# DynamicAccessors provides support for
# simply setting and getting values of any kind from
# any object that includes it.
#
#   require 'dynamic_accessors'
#
#   class Foo
#     include DynamicAccessors
#   end
#
#   foo = Foo.new
#   foo.bar = "Hello World"
#   foo.friends = ['a', 'b', 'c']
#   puts "foo.bar: #{foo.bar}"
#   puts "foo.friends: #{foo.friends.join(', ')}"

module DynamicAccessors

  def initialize(*params)
    super
    @missing_params_hash = Hash.new
  end

  def method_missing(method, *params, &block)
    if(method.to_s.match(/=$/))
      method = method.to_s.gsub('=', '').to_sym
      return @missing_params_hash[method] = params.shift
    else
      return @missing_params_hash[method] if @missing_params_hash.keys.collect(&:to_sym).include?(method)
    end
    super
  end
end
