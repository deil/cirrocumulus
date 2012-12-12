require_relative '../lib/cirrocumulus'

class TestOntology < Ontology
  ontology 'test_ontology'
end

describe 'Cirrocumulus environment' do
  it 'should restore all loaded ontologies states' do
    environment = Cirrocumulus::Environment.new('test')
    ontology = TestOntology.new(LocalIdentifier.new('test'))
    ontology.should_receive(:restore_state)

    environment.load_ontology(ontology)
    environment.run
  end
end
