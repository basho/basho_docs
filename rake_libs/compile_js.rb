def compile_js(debug: false)
  require 'sprockets'
  require 'uglifier'

  env = Sprockets::Environment.new(".")
  env.append_path $js_source

  if (debug)
    #TODO<drew.pirrone.brusse@gmail>: This doesn't make anything more readable.
    #    I don't know how to -- or if we can -- improve readability of js that's
    #    been run through Sprockets...
    # env.js_compressor = Uglifier.new(
    #   :output => {
    #     :comments => :all,
    #     # :preserve_line => true
    #   },
    #   :mangle => false,
    #   :compress => false)
  else
    env.js_compressor = Uglifier.new(
      :output => {
        :comments => :none
      })
  end

  # This will produce a zipped array-of-arrays in the form,
  #     [[souce1, dest1], [source2, dest2], ..., [sourceN, destN]]
  # where the source files are all fo the .js, .coffee, and .erb files in the
  # root of $js_source, and the dest files are all equivalently-named .js files.
  src_dst_list = (Dir.glob("#{$js_source}/*.js")
      .concat Dir.glob("#{$js_source}/*.coffee")
      .concat Dir.glob("#{$js_source}/*.erb")
    ).map do |file|
      src = File.basename file
      # We have to use gsub here (rather than `basename(source, ".*")`) because
      # e.g. 'source.coffee.erb' would turn into 'source.coffee', and we want
      # exactly 'source'.
      dst = "#{$js_dest}/#{src.gsub(/\..*/, "")}.js"
      [src, dst]
    end

  for src, dst in src_dst_list
    File.open(dst, 'w') { |file| file.write env[src].to_s }
    log_write(dst)
  end
end
