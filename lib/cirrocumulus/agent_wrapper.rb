unless Kernel.respond_to?(:require_relative)
  module Kernel
    def require_relative(path)
      require File.join(File.dirname(caller[0]), path.to_str)
    end
  end
end

require 'yaml'
require 'cirrocumulus'
require 'cirrocumulus/logger'
require 'cirrocumulus/ontology'
require 'cirrocumulus/agent'

class String
  def underscore
    self.gsub(/::/, '/').
    gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2').
    gsub(/([a-z\d])([A-Z])/,'\1_\2').
    tr("-", "_").
    downcase
  end
end

ontologies_file_name = nil

ARGV.each_with_index do |arg, i|
  if arg == '-c'
    ontologies_file_name = ARGV[i + 1]
  end
end

if ontologies_file_name.nil?
  puts "Please supply config file name"
  exit(0)
end

puts "Loading configuration.."
agent_config = YAML.load_file(ontologies_file_name)
ontologies = agent_config['ontologies']
ontologies.each do |ontology_name|
  puts "Will load ontology %s" % ontology_name
  require File.join(AGENT_ROOT, 'ontologies', ontology_name.underscore)
end

=begin
kb_name = agent_config['kb']
kb = if kb_name
  puts "Will load knowledge base %s" % kb_name
  require File.join(AGENT_ROOT, 'ontologies/xen/', kb_name.underscore) # TODO
  eval("#{kb_name}.new()")
else
  Kb.new
end
=end

begin
  a = Agent::Base.new
  a.load_ontologies(agent_config['ontologies'])
  a.start
rescue Exception => e
  puts 'Got an error:'
  puts e
  puts e.backtrace
end

puts "\nBye-bye."
