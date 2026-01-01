# frozen_string_literal: true

require_relative "lib/forthic/version"

Gem::Specification.new do |spec|
  spec.name = "forthic"
  spec.version = Forthic::VERSION
  spec.authors = ["Rino Jose"]
  spec.email = ["rjose@forthix.com"]

  spec.summary = "[ARCHIVED] A Forthic interpreter that runs within Ruby - See https://github.com/forthix/forthic-rb"
  spec.description = "[ARCHIVED] This package provides a Forthic interpreter that allows you to execute Forthic code within your Ruby projects. Forthic is a stack-based programming language inspired by Forth. This gem is from an archived repository. Please see https://github.com/forthix/forthic-rb for the new official repository."
  spec.homepage = "https://github.com/forthix/forthic-rb"
  spec.required_ruby_version = ">= 3.1.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/forthix/forthic-rb"
  # spec.metadata["changelog_uri"] = "TODO: Put your gem's CHANGELOG.md URL here."

  spec.post_install_message = <<~MSG
    ⚠️  NOTICE: This gem is from an archived repository.

    The Forthic project has moved to: https://github.com/forthix/forthic-rb
    Please visit the new repository for updates and support.
  MSG

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  gemspec = File.basename(__FILE__)
  spec.files = IO.popen(%w[git ls-files -z], chdir: __dir__, err: IO::NULL) do |ls|
    ls.readlines("\x0", chomp: true).reject do |f|
      (f == gemspec) ||
        f.start_with?(*%w[bin/ test/ spec/ features/ .git appveyor Gemfile])
    end
  end
  spec.bindir = "exe"
  spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  # Uncomment to register a new dependency of your gem
  # spec.add_dependency "example-gem", "~> 1.0"

  # For more information and examples about making a new gem, check out our
  # guide at: https://bundler.io/guides/creating_gem.html
end
