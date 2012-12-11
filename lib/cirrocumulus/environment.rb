module Cirrocumulus
  class Environment
    attr_reader :name

    def initialize(name)
      @name = name
      @ontologies = []

      puts "Loading #{name} Cirrocumulus environment."
    end

    def load_ontology(ontology_instance)
      puts "Adding #{ontology_instance.name} ontology."
      @ontologies << ontology_instance
    end

    def run
      @ontologies.each {|ontology| ontology.restore_state }
      @ontologies.each {|ontology| ontology.run }
    end

    def join
      @ontologies.each {|ontology| ontology.join }
    end

    private



  end
end
