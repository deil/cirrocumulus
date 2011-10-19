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

    def assert(fact)
      log "assert: #{fact.inspect}"

      @facts = [] if @facts.nil?
      @facts << fact
      process()
    end

    def retract(fact)
      @facts = [] if @facts.nil?
      if @facts.delete(fact)
        process()
      else
        puts "fact #{fact.inspect} not found"
      end
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
      matched_patterns = 0
      rule_params = {}
      rule[:facts].each do |pattern|
        @facts.each do |fact|
          binded_params = pattern_matches?(fact, pattern, rule_params)
          if binded_params
            rule_params.merge!(binded_params)
            matched_patterns += 1
          end
        end
      end
      
      return nil if matched_patterns < rule[:facts].size

      rule_params
    end
    
    def pattern_matches?(fact, pattern, current_params = {})
      return nil if fact.size != pattern.size

      #puts "DEBUG: testing pattern %s against fact %s" % [pattern.inspect, fact.inspect]
      #puts "DEBUG: current parameters binding is %s" % current_params.inspect
      
      binded_params = {}
      
      pattern.each_with_index do |el,i|
        if el.is_a?(Symbol) && el.to_s.upcase == el.to_s
          #puts "DEBUG: need to bind parameter %s" % el.to_s
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
      
      #puts "DEBUG: match! binding parameters: %s" % binded_params.inspect 
      binded_params
    end

    def execute(rule, params)
      debug "executing rule '#{rule[:name]}'"
      rule[:body].call(self, params)
    end

    def process()
      self.class.current_ruleset.each do |rule|
        binded_params = matches?(rule)
        execute(rule, binded_params) unless binded_params.nil?
      end
    end

  end
end
