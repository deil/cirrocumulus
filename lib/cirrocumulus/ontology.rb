module Ontology
  class Base
    attr_reader :name
    attr_reader :agent

    def initialize(name, agent)
      @name = name
      @agent = agent
      @sagas = []
    end

    def restore_state()
      puts "call to dummy Ontology::Base.restore_state()"
    end

    def tick()
      @sagas.each do |saga|
        next if saga.is_finished?
        saga.timeout -= 1 if saga.timeout > 0
        saga.handle(nil) if saga.timeout == 0
      end

      handle_tick()
    end

    def handle_incoming_message(message, kb)
      was_processed = false
      @sagas.each do |saga|
        next if saga.is_finished?

        if saga.id == message.in_reply_to
          was_processed = true
          saga.handle(message)
        end
      end

      handle_message(message, kb) if !was_processed
    end

    protected

    def handle_tick()
    end

    def handle_message(message, kb)
      puts "call to dummy Ontology::Base.handle_message()"
    end

    def logger
      Log4r::Logger['agent']
    end
  end
end
