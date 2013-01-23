
module ReleaseNotes
  class << self
    def registered(app)
      version = "1.2"
      paths = {
        'riak' => "/basho/riak/#{version}/RELEASE-NOTES.md"
      }
      for project, release_notes_path in paths
        puts "== #{project} release notes from #{release_notes_path}"
        header = <<-END
---
title: Riak Release Notes
project: riak
version: 1.0.0+
document: reference
toc: false
audience: beginner
keywords: [release-notes]
---
        END
        body = `curl "https://raw.github.com#{release_notes_path}" 2>/dev/null`
        File.open("./source/languages/en/#{project}/references/Release-Notes.md", 'w') do |file|
          file << header
          file << body
        end
      end
    end
  end
end

::Middleman::Extensions.register(:release_notes, ReleaseNotes)
