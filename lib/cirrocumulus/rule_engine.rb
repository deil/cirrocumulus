module RuleEngine
  class Base
    def self.rule(name, facts, &block)
      current_ruleset << {:name => name, :facts => facts, :body => block}
    end
    
    def dump_kb()
      log "Dumping current knowledge base:\n"
      @facts.each_with_index do |fact,i|
        log "%d) %s" % [i, fact.inspect]
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

    def assert(fact, silent = false)
      @facts = [] if @facts.nil?
      return if @facts.include? fact

      log "assert: #{fact.inspect}"

      @facts << fact
      process() if !silent
    end

    def retract(fact, silent = false)
      @facts = [] if @facts.nil?
      if @facts.delete(fact)
        log "retract: #{fact.inspect}"
        process() if !silent
      else
        #puts "fact #{fact.inspect} not found"
      end
    end
    
    def replace(pattern, values)
      log "replace: #{pattern.inspect} => #{values.inspect}"
      
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
        
        retract(old_fact, true)
        assert(new_fact)
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
    
    def execute()
      process()
    end

    protected

    @@loaded_rules = {}
    
    def self.current_ruleset()
      return @@loaded_rules[self.name] ||= []
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

    def matches?(rule)
      debug "Processing rule '#{rule[:name]}' (#{rule[:facts].size} condition(s))"

      pattern_candidates = []
      rule[:facts].each do |pattern|
        pattern_candidates << match_pattern(pattern)
      end
      
      return nil if !pattern_candidates.all? {|c| c.size > 0}

      debug "Rule '#{rule[:name]}' has candidates for each pattern"
      
      match_parameters(rule, pattern_candidates)
    end
    
    def match_pattern(pattern)
      debug "Attempting to match pattern #{pattern.inspect}"
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
      
      debug "Found #{candidates.size} candidate(s)"
      candidates
    end
    
    def match_parameters(rule, candidates)
      debug "Attempting to match parameters"
      
      result = []
      attempt = []
      while (attempt = generate_combination(rule, candidates, attempt)) != [] do
        bindings = test_combination(rule, candidates, attempt)
        if bindings
          debug "Matched! %s" % bindings.inspect
          result << bindings
        end
      end
      
      result
    end
    
    def test_combination(rule, candidates, attempt)
      facts = []
      attempt.each_with_index {|a,i| facts << candidates[i][a]}
      debug "Testing combination #{attempt.inspect}: %s" % facts.inspect

      binded_params = {}
      pattern_params = {}
      facts.each_with_index do |fact,i|
        pattern_params = bind_parameters(rule[:facts][i], fact, binded_params)
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
        rule[:facts].each {|pattern| next_attempt << 0}
      else
        next_attempt = increment_attempt(attempt, rule[:facts].size - 1, candidates.map {|c| c.size})
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
      debug "executing rule '#{rule[:name]}'"
      rule[:body].call(self, params)
    end
    
    def process()
      self.class.current_ruleset.each do |rule|
        binded_params = matches?(rule)
        next if binded_params.nil?
        binded_params.each {|params| execute_rule(rule, params)}
      end
    end
    
  end
 
end
