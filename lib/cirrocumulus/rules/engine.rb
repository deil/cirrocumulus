require 'thread'

module RuleEngine
  # Rule representation.
  # After processing a ruleset, all rules are wrapped with this object.
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
  end

  # Core class for Cirrocumulus Rule Engine.
  # All the magic about asserting/retracting facts and pattern-matching is performed here.
  class Base
    # DSL-method for defining rules.
    def self.rule(name, facts, options = {}, &block)
      current_ruleset << RuleDescription.new(name, facts, options, block)
    end

    def initialize
      @queue = RunQueue.new(self)
      @facts = []
      @fact_times = {}
      @mutex = Mutex.new
    end

    def dump_kb()
      log "Dumping current knowledge base:\n"

      @facts.each_with_index do |fact,i|
        log "%d) %s (at %s)" % [i, fact.inspect, @fact_times[fact]]
      end

      log "Empty" if @facts.empty?
    end

    def dump_ruleset()
      log "Dumping current ruleset:\n"
      self.class.current_ruleset.each_with_index do |rule,i|
        log "%d) %s" % [i, rule[:name]]
      end

      log "Empty ruleset" if self.class.current_ruleset.empty?
    end

    # Asserts new fact. If 'silent' is set to true, does not perform any associated rules
    #
    # * *Returns* :
    #   - nothing
    def assert(fact, silent = false)
      @mutex.synchronize do
        assert_nonblocking(fact, silent)
      end
    end

    # Retracts an existing fact. If 'silent' is set to true, does not perform any associated rules
    #
    # * *Returns* :
    #   - nothing
    def retract(fact, silent = false)
      @mutex.synchronize do
        retract_nonblocking(fact, silent)
      end
    end

    # Replaces fact value
    #
    # * *Returns* :
    #   - nothing
    def replace(pattern, values)
      @mutex.synchronize do
        data = match(pattern)
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
            log "replace #{pattern.inspect} for #{values.inspect}"

            retract_nonblocking(old_fact, true)
            assert_nonblocking(new_fact)
          end
        end
      end
    end

    def query(fact)
      @facts = [] if @facts.nil?
      return @facts.include? fact
    end

    def match(pattern)
      res = []
      match_pattern(pattern).each do |fact|
        res << bind_parameters(pattern, fact, {})
      end

      res
    end

    # Starts this rule engine instance.
    def start()
      @worker_thread = Thread.new do
        while true do
          @queue.run_queued_rules()
          sleep 0.1
        end
      end
    end

    # Executes all associated with current KB rules. Normally, this shouldn't be called by the programmer.
    def execute()
      process()
    end

    protected

    @@loaded_rules = {}

    def self.current_ruleset()
      return @@loaded_rules[self.name] ||= []
    end

    def assert_nonblocking(fact, silent = false)
      return if @facts.include? fact

      time = DateTime.now
      log "assert: %s" % fact.inspect

      @facts << fact
      @fact_times[fact] = time

      process() if !silent
    end

    def retract_nonblocking(fact, silent = false)
      if @facts.delete(fact)
        @fact_times.delete(fact)
        log "retract: #{fact.inspect}"
        process() if !silent
      else
        #puts "fact #{fact.inspect} not found"
      end
    end

    def matches?(rule)
      trace "Processing rule '#{rule.name}' (#{rule.conditions.size} condition(s)):"

      pattern_candidates = []
      rule.conditions.each do |pattern|
        pattern_candidates << match_pattern(pattern)
      end

      return nil if !pattern_candidates.all? {|c| c.size > 0}

      trace "Result: rule '#{rule.name}' has candidates for each pattern"

      match_parameters(rule, pattern_candidates)
    end

    def match_pattern(pattern)
      trace "=> attempting to match pattern #{pattern.inspect}"
      fact_matches = true
      candidates = []

      @facts.each do |fact|
        next if fact.size != pattern.size
        fact_matches = true

        pattern.each_with_index do |el,i|
          if el.is_a?(Symbol) && el.to_s.upcase == el.to_s # parameter
          else
            fact_matches = false if el != fact[i]
          end
        end

        candidates << fact if fact_matches
      end

      trace "=> found #{candidates.size} candidate(s)" if candidates.size > 0
      candidates
    end

    def match_parameters(rule, candidates)
      trace "Attempting to match parameters:"

      result = []
      attempt = []
      while (attempt = generate_combination(rule, candidates, attempt)) != [] do
        bindings = test_combination(rule, candidates, attempt)
        if bindings
          trace "Found: %s" % bindings.inspect
          result << bindings
        end
      end

      result
    end

    def test_combination(rule, candidates, attempt)
      facts = []
      attempt.each_with_index {|a,i| facts << candidates[i][a]}
      trace "=> testing combination #{attempt.inspect}: %s" % facts.inspect

      binded_params = {}
      pattern_params = {}
      facts.each_with_index do |fact,i|
        pattern_params = bind_parameters(rule.conditions[i], fact, binded_params)
        if pattern_params.nil? # failure, parameters mismatch
          return nil
        else
          binded_params.merge!(pattern_params)
        end
      end

      binded_params
    end

    def bind_parameters(pattern, fact, current_bindings)
      result = {}

      pattern.each_with_index do |p,i|
        if p.is_a?(Symbol) && p.to_s.upcase == p.to_s
          return nil if current_bindings.has_key?(p) && current_bindings[p] != fact[i]
          result[p] = fact[i]
        end
      end

      result
    end

    def generate_combination(rule, candidates, attempt)
      next_attempt = []

      if attempt == []
        rule.conditions.each {|pattern| next_attempt << 0}
      else
        next_attempt = increment_attempt(attempt, rule.conditions.size - 1, candidates.map {|c| c.size})
      end

      next_attempt
    end

    def increment_attempt(attempt, idx, limits)
      return [] if idx < 0

      if attempt[idx] < limits[idx] - 1
        attempt[idx] += 1
      else
        i = idx
        while i < limits.size do
          attempt[i] = 0
          i += 1
        end

        return increment_attempt(attempt, idx-1, limits)
      end

      attempt
    end

    def pattern_matches?(fact, pattern, current_params = {})
      return nil if fact.size != pattern.size

      puts "DEBUG: testing pattern %s against fact %s" % [pattern.inspect, fact.inspect]
      puts "DEBUG: current parameters binding is %s" % current_params.inspect

      binded_params = {}

      pattern.each_with_index do |el,i|
        if el.is_a?(Symbol) && el.to_s.upcase == el.to_s
          puts "DEBUG: need to bind parameter %s" % el.to_s
          if current_params && current_params.has_key?(el)
            current_value = current_params[el]
            return nil if fact[i] != current_value
          else
            binded_params[el] = fact[i]
          end
        else
          return nil if el != fact[i]
        end
      end

      puts "DEBUG: match! binding parameters: %s" % binded_params.inspect
      binded_params
    end

    def execute_rule(rule, params)
      @queue.enqueue(rule, params)
    end

    def process()
      self.class.current_ruleset.each do |rule|
        binded_params = matches?(rule)
        next if binded_params.nil?
        binded_params.each {|params| execute_rule(rule, params)}
      end
    end

    def log(msg)
      Log4r::Logger['kb'].info(msg)
    rescue
      puts "[INFO] %s" % msg
    end

    def debug(msg)
      Log4r::Logger['kb'].debug(msg)
    rescue
      puts "[DEBUG] %s" % msg
    end

    def trace(msg)
      return
      Log4r::Logger['kb_trace'].debug(msg)
    rescue
      puts "[TRACE] %s" % msg
    end
  end

end
