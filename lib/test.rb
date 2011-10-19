require 'cirrocumulus/rule_engine.rb'

class Test < RuleEngine::Base
  rule 'convert', [[:temperature, :x, 'F']] do |engine|
    engine.retract([:temperature, :x, 'F'])
    engine.assert([:temperature, :y, 'C'])
  end
end

r = Test.new
r.assert([:temperature, :x, 'F'])
