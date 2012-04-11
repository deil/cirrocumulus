require 'thread'
require 'systemu'
require_relative 'jabber_bus'

module Agent
  class AgentInfo
    attr_accessor :identifier
    attr_accessor :default_ontology
    attr_accessor :last_seen_at

    def initialize
      @identifier = @default_ontology = @last_seen_at = nil
    end
  end

  class NetworkMap

    INVALIDATE_PERIOD = 600*2

    attr_reader :agents
    attr_accessor :version
    attr_reader :valid

    def initialize(agent)
      Log4r::Logger['agent'].debug "initializing empty network map"
      @agent = agent
      @agents = []
      @version = 0
      @valid = 0
    end

    def tick(cm)
      @valid -= 1
      if @valid <= 0
        Log4r::Logger['agent'].debug "invalidating network map"
        msg = Cirrocumulus::Message.new(nil, 'request', [:update, [:map, @version.to_s]])
        msg.ontology = 'cirrocumulus-map'
        cm.send(msg)
        @valid = INVALIDATE_PERIOD
      end
    end

    def handle_message(message, cm)
      if message.ontology == 'cirrocumulus-map'
        return if !message.receiver.blank? && (message.receiver != @agent.identifier)
        #p message

        if message.act == 'inform'
          if message.content.first == :"="
            ontology = message.content[2].first
            agent = @agents.find {|a| a.identifier == message.sender}
            if agent
              agent.default_ontology = ontology
              agent.last_seen_at = Time.now.to_i
              @version = Time.now.to_i
              Log4r::Logger['agent'].info "got neighbour details: #{agent.inspect}"
            end
          else
            agent_info = message.content
            agent = AgentInfo.new
            agent_info.each do |param|
              if param.first == :identifier
                agent.identifier = param.second
              elsif param.first == :default_ontology
                agent.default_ontology = param.second
              elsif param.first == :last_seen_at
                agent.last_seen_at = param.second.to_i
              end
            end

            my_agent = @agents.find {|a| a.identifier == agent.identifier}
            if my_agent.nil?
              my_agent = agent
              @agents << agent
            else
              my_agent.last_seen_at = [my_agent.last_seen_at, agent.last_seen_at].max
              my_agent.default_ontology = agent.default_ontology || my_agent.default_ontology
            end

            Log4r::Logger['agent'].info "neighbour updated: #{my_agent.inspect}"
            @version = Time.now.to_i
          end
        elsif message.act == 'request'
          map_request = message.content
          foreign_map = map_request.second
          foreign_map_version = foreign_map.second.to_i
          if @version >= foreign_map_version
            @agents.each do |agent|
              next if agent.default_ontology.blank?
              msg = Cirrocumulus::Message.new(nil, 'inform', [
                  [:identifier, agent.identifier],
                  [:default_ontology, agent.default_ontology],
                  [:last_seen_at, agent.last_seen_at.to_s]
              ])
              msg.receiver = message.sender
              msg.ontology = 'cirrocumulus-map'
              cm.send(msg)
            end
          end

          process_agent(message, cm)
        elsif message.act == 'query-ref'
          #p message.content
          if message.content.first == :default_ontology
            msg = Cirrocumulus::Message.new(nil, 'inform', [:'=', [:default_ontology], [@agent.default_ontology]])
            msg.receiver = message.sender
            msg.ontology = 'cirrocumulus-map'
            cm.send(msg)
          end
        end
      else
        process_agent(message, cm)
      end
    end

    private

    def process_agent(message, cm)
      agent_id = message.sender
      return if agent_id == @agent.identifier

      agent = @agents.find {|a| a.identifier == agent_id}
      if agent
        agent.last_seen_at = Time.now.to_i
        @version = Time.now.to_i
      else
        agent = AgentInfo.new
        agent.identifier = agent_id
        agent.last_seen_at = Time.now.to_i
        Log4r::Logger['agent'].info "discovered neighbour: #{agent.identifier}"
        @agents << agent
        @version = Time.now.to_i

        msg = Cirrocumulus::Message.new(nil, 'query-ref', [:default_ontology])
        msg.receiver = agent.identifier
        msg.ontology = 'cirrocumulus-map'
        cm.send(msg)
      end
    end
  end

  class Base
    attr_reader :identifier # identifier of running agent
    attr_reader :network_map

    def initialize(suffix = 'master')
      Log4r::Logger['agent'].info('Hello world!')

      @ontologies = []
      @message_queues = {}
      @worker_threads = {}
      #@network_map = NetworkMap.new(self)

      @suffix = suffix
      @identifier = nil
      @bus = nil
      @incoming_queue = Queue.new
    end

    # Gets current platform (linux, freebsd, etc)
    def platform
      if RUBY_PLATFORM =~ /freebsd/
        return 'freebsd'
      elsif RUBY_PLATFORM =~ /linux/
        return 'linux'
      end

      return 'unknown'
    end

    # Loads ontologies into agent
    def load_ontologies(ontologies_list)
      ontologies_list.each do |ontology_name|
        ontology = eval("#{ontology_name}.new(self)")
        @ontologies << ontology
        @message_queues[ontology] = Queue.new
      end
    end

    def default_ontology
      self.ontologies.size == 1 ? self.ontologies.first.name : nil
    end

    # Returns true if this agents supports requested ontology
    def handles_ontology?(ontology_name)
      self.ontologies.any? {|ontology| ontology.name == ontology_name}
    end

    # Asks each loaded ontology to restore its state
    def restore_state()
      Log4r::Logger['agent'].info 'Restoring previous state..'

      threads = []
      self.ontologies.each do |ontology|
        threads << Thread.new do
          begin
            ontology.restore_state()
          rescue Exception => e
            Log4r::Logger['agent'].warn "failed to restore state for ontology %s" % ontology.name
            Log4r::Logger['agent'].warn "%s: %s" % [e.to_s, e.backtrace.to_s]
          end
        end
      end

      threads.each {|thrd| thrd.join}
      Log4r::Logger['agent'].info 'State restored successfully!'
    end

    # Just sends an ACL message to bus
    def send_message(message)
      @bus.send_message(message)
    end

    # Replies to specified message.
    # Sends a reply to bus in reply to original message using sender, reply_with and conversation_in
    def reply_to_message(reply, message)
      reply.receiver = message.sender
      reply.in_reply_to = message.reply_with
      reply.conversation_id = message.conversation_id
      @bus.send_message(reply)
    end

    # Start the agent!
    def start()
      suffix = default_ontology ? default_ontology.gsub('cirrocumulus-', '') : @suffix
      _, hostname = systemu('hostname')
      hostname.strip!
      @identifier = '%s-%s' % [hostname, suffix]

      @bus = JabberBus.new(@identifier)
      @bus.start(self)

      start_workers
      restore_state
      main_loop
    end

    def process_incoming_message(message)
      @incoming_queue << message
    end

    protected

    attr_reader :ontologies

    def start_workers()
      self.ontologies.each do |ontology|
        @worker_threads[ontology] = Thread.new do
          Log4r::Logger['agent'].info 'Starting worker for ontology %s' % [ontology.name]
          ontology_loop(ontology, @message_queues[ontology])
        end
      end
    end

    def ontology_loop(ontology, queue)
      while true do
        message = queue.pop(true) rescue nil

        if message
          begin
            ontology.handle_incoming_message(message, nil)
          rescue Exception => e
            Log4r::Logger['agent'].warn "failed to handle incoming message for ontology %s: %s" % [ontology.name, e.to_s]
            Log4r::Logger['agent'].warn e.backtrace.to_s
          end
        else
          ontology.tick()
          sleep 0.2
        end
      end
    end

    def main_loop
      while true
        message = @incoming_queue.pop(true) rescue nil

        if message
          next if !message.receiver.blank? && message.receiver != self.identifier
          next if (message.sender == self.identifier) && !(message.receiver == self.identifier)

          ontologies.each do |ontology|
            if message.ontology == ontology.name || ontology.sagas.any? {|saga| [message.in_reply_to, message.conversation_id].include?(saga.id) }
              @message_queues[ontology] << message
              break
            end
          end
        else # sleep if no messages available
          sleep 0.2
        end
      end
    end
  end
end
