require_relative 'pattern_matching'
require_relative 'rule_queue'
require_relative 'saga'

class RuleDescription
	attr_reader :name
	attr_reader :conditions
	attr_reader :options
	attr_reader :code

	def initialize(name, conditions, options, code)
	  @name = name
	  @conditions = conditions
	  @options = options
	  @code = code
  end

  def ==(other)
    name == other.name
  end
end

class Ontology
	class << self
		@@inproc_agents = {}
		@@loaded_rules = {}
		@@ontology_names = {}

		def register_ontology_instance(instance)
			@@inproc_agents[instance.identifier] = instance
		end

		def list_ontology_instances
			@@inproc_agents.each_key.map {|key| key.to_s}
		end

		def query_ontology_instance(identifier)
			@@inproc_agents.each_key do |key|
				return @@inproc_agents[key] if key == identifier
			end

			nil
    end

    def assert(identifier, data)
      instance = query_ontology_instance(identifier)
      instance.assert(data) if instance
    end

    def retract(identifier, data)
      instance = query_ontology_instance(identifier)
      instance.retract(data) if instance
    end

    def dump_kb(identifier)
      instance = query_ontology_instance(identifier)
      return instance.nil? ? [] : instance.dump_kb()
    end

    def dump_sagas(identifier)
      instance = query_ontology_instance(identifier)
      return instance.nil? ? [] : instance.dump_sagas()
    end

		def current_ruleset()
			return @@loaded_rules[name] ||= []
		end

		def enable_console
			proxy = RemoteConsole.new
			DRb.start_service('druby://0.0.0.0:8112', proxy)
		end

		def ontology(ontology_name)
			@@ontology_names[name] = ontology_name
		end

		def rule(name, predicate, options = {}, &block)
      return if predicate.empty?
      return if current_ruleset.count {|rule| rule.name == name} > 0

			current_ruleset << RuleDescription.new(name, predicate, options, block)
		end
	end

	attr_reader :identifier

	#
	# Infrastructure code
	#
  def initialize(identifier)
    @identifier = identifier
    @facts = FactsDatabase.new()
    @last_saga_id = 0
    @sagas = []

    self.class.register_ontology_instance(self)
    @mutex = Mutex.new
    @rule_queue = RuleQueue.new
  end

  def name
    @@ontology_names[self.class.name]
  end

	def run()
		self.running = true

		@thread = Thread.new(self) do |ontology|
			while self.running do
				ontology.timeout_facts
        @rule_queue.run_queued_rules
				sleep 0.1
			end
		end
	end

	def join
		self.running = false
		@thread.join()
	end

	def assert(fact, options = {})
		@mutex.synchronize do
			assert_nb(fact, options)
	  end
	end

	def retract(fact)
		@mutex.synchronize do
			retract_nb(fact)
		end
	end

	def replace(pattern, values, options = {})
   @mutex.synchronize do
	   matcher = PatternMatcher.new(@facts.enumerate())
     data = matcher.match(pattern)

     if data.empty?
       new_fact = pattern.clone

       pattern.each_with_index do |item,i|
         if item.is_a?(Symbol) && item.to_s.upcase == item.to_s
           new_fact[i] = values.is_a?(Hash) ? values[item] : values
         end
       end

       assert_nb(new_fact, options, false)
     else
       data.each do |match_data|
         old_fact = pattern.clone
         new_fact = pattern.clone
         pattern.each_with_index do |item,i|
           if match_data.include? item
             old_fact[i] = match_data[item]
             new_fact[i] = values.is_a?(Hash) ? values[item] : values
           end
         end

         facts_are_same = true
         old_fact.each_with_index do |item, idx|
           new_item = new_fact[idx]
           facts_are_same = false if new_item != item
         end

         unless facts_are_same
           debug "replace #{pattern.inspect} for #{values.inspect}"

           retract_nb(old_fact, true)
           assert_nb(new_fact, {}, false)
         end
       end
     end
   end
 end

  def dump_kb()
    @facts.enumerate.map {|fact| fact.data.to_s}
  end

  def dump_sagas()
    @sagas.map {|saga| saga.to_s}
  end

	#
	# Inter-agent communications
	#

  def create_saga(saga_class)
    @last_saga_id += 1
    saga = saga_class.new(saga_class.saga + '-' + @last_saga_id.to_s, self)
    @sagas << saga

    saga
  end

  def reply(options)
    if options.has_key?(:conversation_id)
      {:conversation_id => options[:conversation_id]}
    elsif options.has_key?(:reply_with)
      {:in_reply_to => options[:reply_with]}
    else
      {}
    end
  end

  #
  # Inform another agent about a fact. Normally, it will be added to it's KB.
  #
	def inform(agent, fact, options = {})
		puts "%25s | inform %s about %s %s" % [identifier, agent, Sexpistol.new.to_sexp(fact), print_message_options(options)]

    channel = ChannelFactory.retrieve(identifier, agent)
    channel.inform(identifier, fact, options) if channel
	end

	def inform_and_wait(agent, fact, options = {})

	end

  #
  # Send request to another agent.
  #
	def request(agent, contents, options = {})
		puts "%25s | %s -> %s" % [identifier.to_s, Sexpistol.new.to_sexp(contents), agent.to_s]

		channel = ChannelFactory.retrieve(identifier, agent)
		channel.request(identifier, contents) if channel
	end

	def request_and_wait(agent, contents, options = {})

	end

	def query(agent, expression, options = {})
    puts "%25s | query %s about %s %s" % [identifier, agent, Sexpistol.new.to_sexp(expression), print_message_options(options)]

    channel = ChannelFactory.retrieve(identifier, agent)
    channel.query(identifier, expression, options) if channel
	end

	def query_and_wait(agent, smth, options = {})
	end

  #
  # Send 'query-if' to another agent. Normally, it will reply if the expression is true or false.
  #
	def query_if(agent, fact, options = {})
	end

	def query_if_and_wait(agent, fact, options = {})
  end

  #
  # Custom code to restore previous state. Called at startup.
  #
  def restore_state; end

  #
  # Handles incoming fact. By default, just adds this fact to KB or redirects its processing to corresponding saga
  #
  def handle_inform(sender, proposition, options = {})
    puts "%25s | received %s from %s %s" % [identifier, Sexpistol.new.to_sexp(proposition), sender, print_message_options(options)]

    if options.has_key?(:conversation_id) || options.has_key?(:in_reply_to)
      saga_id = options[:conversation_id] || options[:in_reply_to]
      saga = @sagas.find {|saga| saga.id == saga_id}
      saga.handle_reply(sender, proposition, :action => :inform) if saga
    else
      assert proposition, :origin => sender
    end
  end

  #
  # Abstract method to handle requests to this ontology.
  #
	def handle_request(sender, contents, options = {}); end

  def handle_query(sender, expression, options = {})
    puts "%25s | %s queries %s %s" % [identifier, sender, Sexpistol.new.to_sexp(expression), print_message_options(options)]
  end

  #
  # Handles query-if to ontology. By default, it lookups the fact in KB and replies to the sender.
  #
  def handle_query_if(sender, proposition, options = {})
    puts "%25s | %s queries if %s %s" % [identifier, sender, Sexpistol.new.to_sexp(proposition), print_message_options(options)]
  end

	protected

	attr_reader :facts
	attr_accessor :running

	def assert_nb(fact, options = {}, silent = false)
		silent = options unless options.is_a?(Hash)
    options = {} unless options.is_a?(Hash)

		debug("assert #{fact}")
		@facts.add(fact, options)
		process_rules() unless silent
	end

	def retract_nb(fact, options = {}, silent = false)
		silent = options unless options.is_a?(Hash)
    options = {} unless options.is_a?(Hash)

		debug("retract #{fact}")
		@facts.remove(fact)
		process_rules() unless silent
	end

	def timeout_facts()
		@mutex.synchronize do
      to_retract = []
      @facts.enumerate().each {|fact| to_retract << fact if fact.timed_out? }
      to_retract.each {|fact| retract_nb(fact.data, true) }
      process_rules() unless to_retract.empty?
    end
	end

	def process_rules()
		matcher = PatternMatcher.new(@facts.enumerate)

		self.class.current_ruleset.each do |rule|
      binded_params = matcher.matches?(rule)
      next if binded_params.nil? || binded_params.empty?

      binded_params.each {|params| execute_rule(params)}
		end
	end

	def execute_rule(match_data)
    @rule_queue.push(match_data)
	end

  def print_message_options(options = {})
    return if options.empty?

    "[%s]" % options.map {|k,v| "%s=%s" % [k, v]}.join(',')
  end

	def debug(msg)
		puts "[DBG] %s" % msg
	end

end
