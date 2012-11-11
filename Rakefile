# encoding: utf-8

require 'rubygems'
require 'bundler'
begin
  Bundler.setup(:default, :development)
rescue Bundler::BundlerError => e
  $stderr.puts e.message
  $stderr.puts "Run `bundle install` to install missing gems"
  exit e.status_code
end
require 'rake'

require 'jeweler'
Jeweler::Tasks.new do |gem|
  # gem is a Gem::Specification... see http://docs.rubygems.org/read/chapter/20 for more options
  gem.name = "cirrocumulus"
  gem.homepage = "http://github.com/deil/cirrocumulus"
  gem.license = "GPL-2"
  gem.summary = "Agent-based infrastructure management system"
  gem.description = "Engine for building your own agents, providing you base functionality for loading ontologies, communicating with other agents and parsing FIPA-ACL messages"
  gem.email = "deil@mneko.net"
  gem.authors = ["Anton Kosyakin"]
  gem.required_ruby_version = '>= 1.9.2'
  gem.files = Dir.glob('lib/**/*.rb')
  #gem.files.exclude 'tmp'
end
Jeweler::RubygemsDotOrgTasks.new

require 'rdoc/task'
Rake::RDocTask.new do |rdoc|
  version = File.exist?('VERSION') ? File.read('VERSION') : ""

  rdoc.rdoc_dir = 'rdoc'
  rdoc.title = "cirrocumulus #{version}"
  rdoc.rdoc_files.include('README*')
  rdoc.rdoc_files.include('lib/**/*.rb')
end
