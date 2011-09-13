module Ontology
  class Base
    attr_reader :name
    attr_reader :agent

    def initialize(name, agent)
      @name = name
      @agent = agent
    end

    def restore_state()
      puts "call to dummy Ontology::Base.restore_state()"
    end

    def tick()
    end

    def handle_message(message, kb)
      puts "call to dummy Ontology::Base.handle_message()"
    end

    protected

    def logger
      Log4r::Logger['agent']
    end
  end
end
