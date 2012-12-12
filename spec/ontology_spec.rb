require 'bundler/setup'
require_relative '../lib/cirrocumulus'

class TestOntology < Ontology
end

describe Ontology do
  it 'each ontology instance with local identifier should be accessible' do
    ontology = TestOntology.new(LocalIdentifier.new('test'))
    (ontology == Ontology.query_ontology_instance(ontology.identifier)).should be_true
  end
  
  it 'ontology with another identifier should not be accessible' do
    ontology = TestOntology.new(RemoteIdentifier.new('test'))
    (ontology == Ontology.query_ontology_instance(ontology.identifier)).should be_false
  end
end
