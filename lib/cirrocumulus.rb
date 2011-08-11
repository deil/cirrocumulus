AGENT_ROOT = File.dirname(__FILE__)

require 'rubygems'
require 'cm/logger'
require 'cm/cirrocumulus'
require 'cm/master_agent'
require 'cm/kb'
require 'cm/ontology'
require 'yaml'

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
  return
end

puts "Loading configuration.."
agent_config = YAML.load_file(ontologies_file_name)
ontologies = agent_config['ontologies']
ontologies.each do |ontology_name|
  puts "Will load ontology %s" % ontology_name
  require File.join(AGENT_ROOT, 'ontologies', ontology_name.underscore)
end

cm = Cirrocumulus.new('master')
a = Agent::Base.new(cm)
a.load_ontologies(agent_config['ontologies'])
begin
  cm.run(a, Kb.new)
rescue Exception => e
  puts 'Got an error:'
  puts e
  puts e.backtrace
end

puts "\nBye-bye."
