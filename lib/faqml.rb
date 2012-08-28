
module Middleman::Renderers::FAQML
  def registered(app)
    # FAQML is not included in the default gems,
    # but we'll support it if available.
    begin
      # Load gem
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
    end
  end
end
