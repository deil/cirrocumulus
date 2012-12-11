module Cirrocumulus
  class Environment

    def self.current
      @@instance
    end

    attr_reader :name

    def initialize(name)
      puts "Loading #{name} Cirrocumulus environment."

      @name = name
      @ontologies = []
      @@instance = self
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

    @@instance = nil

  end
end
