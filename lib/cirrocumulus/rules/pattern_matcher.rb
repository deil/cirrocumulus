module Cirrocumulus
  module Ruler
    # The Matcher.
    # Matches and compares facts with each other. Searches and bind variables.
    class PatternMatcher
      def match(pattern, fact)
        return false if fact.data.size != pattern.size
        fact_matches = true

        pattern.each_with_index do |el,i|
          next if el.is_a?(Symbol) && el.to_s.upcase == el.to_s
          fact_matches = false if el != fact.data[i]
        end

        fact_matches
      end
    end
  end
end
