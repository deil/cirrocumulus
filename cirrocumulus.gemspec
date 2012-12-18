# Generated by jeweler
# DO NOT EDIT THIS FILE DIRECTLY
# Instead, edit Jeweler::Tasks in Rakefile, and run 'rake gemspec'
# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "cirrocumulus"
  s.version = "0.9.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Anton Kosyakin"]
  s.date = "2012-12-18"
  s.description = "Engine for building your own agents, providing you base functionality for loading ontologies, communicating with other agents and parsing FIPA-ACL messages"
  s.email = "deil@mneko.net"
  s.extra_rdoc_files = [
    "LICENSE.txt",
    "README.rdoc"
  ]
  s.files = [
    "lib/cirrocumulus.rb",
    "lib/cirrocumulus/agents/message.rb",
    "lib/cirrocumulus/channels.rb",
    "lib/cirrocumulus/channels/jabber.rb",
    "lib/cirrocumulus/environment.rb",
    "lib/cirrocumulus/facts.rb",
    "lib/cirrocumulus/identifier.rb",
    "lib/cirrocumulus/logger.rb",
    "lib/cirrocumulus/ontology.rb",
    "lib/cirrocumulus/pattern_matching.rb",
    "lib/cirrocumulus/remote_console.rb",
    "lib/cirrocumulus/rule_queue.rb",
    "lib/cirrocumulus/rules/engine.rb",
    "lib/cirrocumulus/rules/fact.rb",
    "lib/cirrocumulus/rules/pattern_matcher.rb",
    "lib/cirrocumulus/rules/run_queue.rb",
    "lib/cirrocumulus/saga.rb",
    "lib/console.rb"
  ]
  s.homepage = "http://github.com/deil/cirrocumulus"
  s.licenses = ["GPL-2"]
  s.require_paths = ["lib"]
  s.required_ruby_version = Gem::Requirement.new(">= 1.9.2")
  s.rubygems_version = "1.8.24"
  s.summary = "Agent-based infrastructure management system"

  if s.respond_to? :specification_version then
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<log4r>, [">= 0"])
      s.add_runtime_dependency(%q<systemu>, [">= 0"])
      s.add_runtime_dependency(%q<xmpp4r>, [">= 0"])
      s.add_runtime_dependency(%q<xmpp4r-simple>, [">= 0"])
      s.add_runtime_dependency(%q<deil_sexpistol>, [">= 0"])
      s.add_runtime_dependency(%q<guid>, [">= 0"])
      s.add_development_dependency(%q<rdoc>, ["~> 3.12"])
      s.add_development_dependency(%q<bundler>, [">= 0"])
      s.add_development_dependency(%q<jeweler>, ["~> 1.8.3"])
    else
      s.add_dependency(%q<log4r>, [">= 0"])
      s.add_dependency(%q<systemu>, [">= 0"])
      s.add_dependency(%q<xmpp4r>, [">= 0"])
      s.add_dependency(%q<xmpp4r-simple>, [">= 0"])
      s.add_dependency(%q<deil_sexpistol>, [">= 0"])
      s.add_dependency(%q<guid>, [">= 0"])
      s.add_dependency(%q<rdoc>, ["~> 3.12"])
      s.add_dependency(%q<bundler>, [">= 0"])
      s.add_dependency(%q<jeweler>, ["~> 1.8.3"])
    end
  else
    s.add_dependency(%q<log4r>, [">= 0"])
    s.add_dependency(%q<systemu>, [">= 0"])
    s.add_dependency(%q<xmpp4r>, [">= 0"])
    s.add_dependency(%q<xmpp4r-simple>, [">= 0"])
    s.add_dependency(%q<deil_sexpistol>, [">= 0"])
    s.add_dependency(%q<guid>, [">= 0"])
    s.add_dependency(%q<rdoc>, ["~> 3.12"])
    s.add_dependency(%q<bundler>, [">= 0"])
    s.add_dependency(%q<jeweler>, ["~> 1.8.3"])
  end
end

