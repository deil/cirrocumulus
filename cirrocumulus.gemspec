# Generated by jeweler
# DO NOT EDIT THIS FILE DIRECTLY
# Instead, edit Jeweler::Tasks in Rakefile, and run 'rake gemspec'
# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "cirrocumulus"
  s.version = "0.5.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Anton Kosyakin"]
  s.date = "2011-12-01"
  s.description = "Engine for building your own agents, providing you base functionality for loading ontologies, communicating with other agents and parsing FIPA-ACL messages"
  s.email = "deil@mneko.net"
  s.extra_rdoc_files = [
    "LICENSE.txt",
    "README.rdoc"
  ]
  s.files = [
    ".document",
    "Gemfile",
    "LICENSE.txt",
    "README.rdoc",
    "Rakefile",
    "VERSION",
    "cirrocumulus.gemspec",
    "lib/cirrocumulus.rb",
    "lib/cirrocumulus/agent.rb",
    "lib/cirrocumulus/agent_wrapper.rb",
    "lib/cirrocumulus/engine.rb",
    "lib/cirrocumulus/kb.rb",
    "lib/cirrocumulus/logger.rb",
    "lib/cirrocumulus/ontology.rb",
    "lib/cirrocumulus/rule_engine.rb",
    "lib/cirrocumulus/rule_server.rb",
    "lib/cirrocumulus/rules/engine.rb",
    "lib/cirrocumulus/rules/run_queue.rb",
    "lib/cirrocumulus/saga.rb",
    "test/helper.rb",
    "test/test_cirrocumulus.rb"
  ]
  s.homepage = "http://github.com/deil/cirrocumulus"
  s.licenses = ["GPL-2"]
  s.require_paths = ["lib"]
  s.rubygems_version = "1.8.10"
  s.summary = "Agent-based infrastructure management system"

  if s.respond_to? :specification_version then
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<activesupport>, ["~> 2.3.11"])
      s.add_runtime_dependency(%q<log4r>, ["~> 1.1.9"])
      s.add_runtime_dependency(%q<systemu>, [">= 0"])
      s.add_runtime_dependency(%q<xmpp4r>, ["~> 0.5"])
      s.add_runtime_dependency(%q<xmpp4r-simple>, [">= 0"])
      s.add_runtime_dependency(%q<eventmachine>, [">= 0"])
      s.add_runtime_dependency(%q<deil_sexpistol>, [">= 0"])
      s.add_development_dependency(%q<bundler>, ["~> 1.0.0"])
      s.add_development_dependency(%q<jeweler>, ["~> 1.6.4"])
      s.add_development_dependency(%q<rcov>, [">= 0"])
    else
      s.add_dependency(%q<activesupport>, ["~> 2.3.11"])
      s.add_dependency(%q<log4r>, ["~> 1.1.9"])
      s.add_dependency(%q<systemu>, [">= 0"])
      s.add_dependency(%q<xmpp4r>, ["~> 0.5"])
      s.add_dependency(%q<xmpp4r-simple>, [">= 0"])
      s.add_dependency(%q<eventmachine>, [">= 0"])
      s.add_dependency(%q<deil_sexpistol>, [">= 0"])
      s.add_dependency(%q<bundler>, ["~> 1.0.0"])
      s.add_dependency(%q<jeweler>, ["~> 1.6.4"])
      s.add_dependency(%q<rcov>, [">= 0"])
    end
  else
    s.add_dependency(%q<activesupport>, ["~> 2.3.11"])
    s.add_dependency(%q<log4r>, ["~> 1.1.9"])
    s.add_dependency(%q<systemu>, [">= 0"])
    s.add_dependency(%q<xmpp4r>, ["~> 0.5"])
    s.add_dependency(%q<xmpp4r-simple>, [">= 0"])
    s.add_dependency(%q<eventmachine>, [">= 0"])
    s.add_dependency(%q<deil_sexpistol>, [">= 0"])
    s.add_dependency(%q<bundler>, ["~> 1.0.0"])
    s.add_dependency(%q<jeweler>, ["~> 1.6.4"])
    s.add_dependency(%q<rcov>, [">= 0"])
  end
end

