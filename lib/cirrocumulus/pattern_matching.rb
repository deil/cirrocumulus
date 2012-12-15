class MatchResult
	def initialize(rule)
	  @rule = rule
	  @matched_facts = []
	  @parameters = nil
	end

	attr_reader :rule
	attr_reader :matched_facts
	attr_accessor :parameters

  def ==(other)
    rule == other.rule && matched_facts == other.matched_facts
  end
end

class PatternMatcher
	def initialize(facts)
		@facts = facts
	end

	def match(pattern)
		res = []
		find_matches_for_condition(pattern).each do |fact|
			res << bind_parameters(pattern, fact.data, {})
		end

		res
	end

	def matches?(rule)
		trace "Processing rule '#{rule.name}' (#{rule.conditions.size} condition(s)):"

		pattern_candidates = []
		rule.conditions.each do |pattern|
      pattern_candidates << find_matches_for_condition(pattern)
		end

		return nil if !pattern_candidates.all? {|c| c.size > 0}

		intersect_matches_for_each_condition(rule, pattern_candidates)
	end

  def find_matches_for_condition(pattern)
    trace "=> attempting to match pattern #{pattern.inspect}"
    fact_matches = true
    candidates = []

	 @facts.each do |fact|
	   next if fact.data.size != pattern.size
	   fact_matches = true

	   pattern.each_with_index do |el,i|
	     if el.is_a?(Symbol) && el.to_s.upcase == el.to_s # parameter
	     else
	       fact_matches = false if el != fact.data[i]
	     end
	   end

      candidates << fact if fact_matches
    end

		trace "=> candidates: #{candidates.size}" if candidates.size > 0
    candidates
  end

	def intersect_matches_for_each_condition(rule, candidates)
	 result = []
	 attempt = []
	 while (attempt = generate_combination(rule, candidates, attempt)) != [] do
	   bindings = test_condition_parameters_combination(rule, candidates, attempt)
	   if bindings
	     match_data = MatchResult.new(rule)
	     attempt.each_with_index {|a,i| match_data.matched_facts << candidates[i][a]}
	     match_data.parameters = bindings
	     result << match_data
	   end
	 end

	 result
	end

	def test_condition_parameters_combination(rule, candidates, attempt)
	 facts = []
	 attempt.each_with_index {|a,i| facts << candidates[i][a].data}

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

	 binded_params = {}

	 pattern.each_with_index do |el,i|
	   if el.is_a?(Symbol) && el.to_s.upcase == el.to_s
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

	 binded_params
	end

	private

	def trace(msg)
		puts "[TRACE] %s" % msg if false
	end

	def debug(msg)
		puts "[DBG] %s" % msg if false
	end

end
