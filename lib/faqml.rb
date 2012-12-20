
module Middleman::Renderers::FAQML
  class << self
    def registered(app)
      begin
        require "faqml"
        app.before_configuration do
          template_extensions :fml => :html
        end
        # Setup FAQML options to work with partials
        ::FAQML::Engine.set_default_options(
          :buffer    => '@_out_buf', 
          :generator => ::Temple::Generators::StringBuffer
        )
      rescue LoadError
        puts "Error Loading FAQML"
      end
    end
    alias :included :registered
  end
end

::Middleman::Extensions.register(:faqml, Middleman::Renderers::FAQML)
