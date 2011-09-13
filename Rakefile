# 
# To change this template, choose Tools | Templates
# and open the template in the editor.
 

require 'rubygems'
require 'rake'
require 'rake/clean'
require 'rake/gempackagetask'
require 'rake/rdoctask'
require 'rake/testtask'

spec = Gem::Specification.new do |s|
  s.name = 'cirrocumulus'
  s.homepage = 'https://github.com/deil/cirrocumulus'
  s.version = '0.1.1'
  s.has_rdoc = false
  s.extra_rdoc_files = ['README.rdoc']
  s.summary = 'Agent-based infrastructure management system'
  s.description = 'Engine for agent-based infrastructure management system'
  s.author = 'Anton Kosyakin'
  s.email = 'deil@mneko.net'
  # s.executables = ['your_executable_here']
  s.files = %w(README.rdoc Rakefile) + Dir.glob("{bin,lib,spec}/**/*")
  s.require_path = "lib"
  s.bindir = "bin"
  s.license = ['GPL-2']
  s.add_dependency("activesupport", "~> 2.3.11")
  s.add_dependency("log4r", "~> 1.1.9")
  s.add_dependency("systemu")
  s.add_dependency("xmpp4r", "~> 0.5")
  s.add_dependency("xmpp4r-simple", "~> 0.8.8")
end

Rake::GemPackageTask.new(spec) do |p|
  p.gem_spec = spec
  p.need_tar = true
  p.need_zip = true
end

Rake::RDocTask.new do |rdoc|
  files =['README.rdoc', 'lib/**/*.rb']
  rdoc.rdoc_files.add(files)
  rdoc.main = "README" # page to start on
  rdoc.title = "cirrocumulus Docs"
  rdoc.rdoc_dir = 'doc/rdoc' # rdoc output folder
  rdoc.options << '--line-numbers'
end

#Rake::TestTask.new do |t|
#  t.test_files = FileList['test/**/*.rb']
#end
