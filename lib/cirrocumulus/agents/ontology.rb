#
# Ontology. Here everything lives
#
module Cirrocumulus
	class Ontology
    attr_reader :name
    attr_reader :agent
    attr_reader :sagas

    def initialize(name, agent)
      @name = name
      @agent = agent
      @sagas = []
      @saga_idx = 0
    end

    # Restores saved state. Called once at initialization
    def restore_state()
      puts "call to dummy Ontology::Base.restore_state()"
    end

    def tick()
      time = Time.now.to_i

      @sagas.each do |saga|
        next if saga.is_finished?

        begin
          saga.handle(nil) if saga.timed_out?(time)
        rescue Exception => e
          Log4r::Logger['agent'].warn "Got exception while ticking saga: %s\n%s" % [e.to_s, e.backtrace.to_s]
        end
      end

      handle_tick()
    end

    def handle_incoming_message(message, kb)
      was_processed = false
      @sagas.each do |saga|
        next if saga.is_finished?

        if [message.in_reply_to, message.conversation_id].include?(saga.id)
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

    def create_saga(saga_class)
      @saga_idx += 1
      saga = saga_class.new(saga_class.to_s + '-' + @saga_idx.to_s, self)
      @sagas << saga
      saga
    end

    def logger
      Log4r::Logger['agent']
    end
  end
end
