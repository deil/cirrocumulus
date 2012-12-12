require_relative '../lib/cirrocumulus'

class TestOntology < Ontology
  ontology 'test_ontology'

  rule 'test_rule', [ [:shiny, :test, :fact] ] do |ontology, params|
  end
end

class OntologyWithDuplicateRules < Ontology
  ontology 'ontology_with_duplicate_rules'

  rule 'duplicate', [ [:shiny] ] do |ontology, params|
  end

  rule 'duplicate', [ [:shiny] ] do |ontology, params|
  end
end

class OntologyWithEmptyConditionsRule < Ontology
  ontology 'empty_conditions_rule'

  rule 'empty_conditions', [] do |ontology, params|
  end
end

describe Ontology do
  it 'each ontology instance with local identifier should be accessible' do
    ontology = TestOntology.new(LocalIdentifier.new('test'))
    (ontology == Ontology.query_ontology_instance(ontology.identifier)).should be_true
  end

  it 'should initialize empty facts database' do
    FactsDatabase.should_receive(:new)
    TestOntology.new(LocalIdentifier.new('test'))
  end

  it 'should ignore duplicate rules in some way' do
    ontology = OntologyWithDuplicateRules.new(Agent.local('duplicates'))
    ontology.class.current_ruleset.size.should == 1
  end

  it 'should ignore rules with empty conditions' do
    ontology = OntologyWithEmptyConditionsRule.new(Agent.local('empty_conditions'))
    ontology.class.current_ruleset.size.should == 0
  end
end
