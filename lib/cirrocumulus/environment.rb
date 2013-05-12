require 'log4r'

module Cirrocumulus
  #
  # Cirrocumulus environment. It is a container where all ontologies will be loaded.
  #
  class Environment

    def self.current
      @@instance
    end

    attr_reader :name

    def initialize(name)
      Log4r::Logger['agent'].info "Loading #{name} Cirrocumulus environment"

      @name = name
      @ontologies = []
      @@instance = self
    end

    #
    # Loads ontology instance into environment.
    #
    def load_ontology(ontology_instance)
      Log4r::Logger['agent'].info "Adding #{ontology_instance.name} ontology"
      @ontologies << ontology_instance
    end

    #
    # Runs the environment. It will also restore all loaded ontologies states.
    #
    def run
      @ontologies.each do |ontology|
        begin
          ontology.restore_state
        rescue Exception => ex
          Log4r::Logger['agent'].warn "ontology #{ontology.name} failed to restore it's state"
          Log4r::Logger['agent'].warn "with exception: #{ex.to_s}"
          Log4r::Logger['agent'].warn "at #{ex.backtrace.join("\n")}"
          Log4r::Logger['agent'].fatal 'will stop execution due to previous errors'
          return
        end
      end

      @ontologies.each {|ontology| ontology.run }
    end

    def join
      @ontologies.each {|ontology| ontology.join }
    end

    private

    @@instance = nil

  end
end
