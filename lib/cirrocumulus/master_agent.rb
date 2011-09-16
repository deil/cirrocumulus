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
    attr_reader :identifier
    attr_reader :network_map

    def initialize(cm)
      Log4r::Logger['agent'].info('Initializing new agent')

      @cm = cm
      @identifier = cm.jid
      @ontologies = []
      @network_map = NetworkMap.new(self)
    end

    def load_ontologies(ontologies_list)
      ontologies_list.each do |ontology_name|
        ontology = eval("#{ontology_name}.new(self)")
        self.ontologies << ontology
      end
    end

    def default_ontology
      self.ontologies.size == 1 ? self.ontologies.first.name : nil
    end

    def handles_ontology?(ontology_name)
      self.ontologies.any? {|ontology| ontology.name == ontology_name}
    end

    def restore_state()
      self.ontologies.each do |ontology|
        begin
          ontology.restore_state()
        rescue Exception => e
          Log4r::Logger['agent'].warn "failed to restore state for ontology %s" % ontology.name
          Log4r::Logger['agent'].warn e.backtrace.to_s
        end
      end
    end

    def send_message(message)
      @cm.send(message)
    end

    def tick()
      @network_map.tick(@cm)

      self.ontologies.each {|ontology| ontology.tick() }
    end

    def handle_message(message, kb)
      @network_map.handle_message(message, @cm)
      self.ontologies.each {|ontology| ontology.handle_incoming_message(message, kb) if message.ontology == ontology.name}
    rescue Exception => e
      Log4r::Logger['agent'].warn "failed to handle incoming message: %s" % e.to_s
      puts e.backtrace.to_s
    end

    protected

    attr_reader :cm
    attr_reader :ontologies

  end
end
