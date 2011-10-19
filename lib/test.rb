require 'cirrocumulus/rule_engine.rb'

class Test < RuleEngine::Base
  rule 'convert', [[:temperature, :X, 'F']] do |engine, params|
    x = params[:X].to_i
    engine.retract([:temperature, x, 'F'])
    y = 5*(x - 32)/9
    engine.assert([:temperature, y, 'C'])
  end
  
  rule 'monitor_md', [[:virtual_disk, :X, :active], [:mdraid, :X, :failed]] do |engine, params|
    # md devices is failed, but virtual disk should be up
    p params
    puts "virtual disk #{params[:X]} should be up, but corresponding md devices is failed!"
  end
end

class Test2 < RuleEngine::Base
end

e = Test.new
e1 = Test2.new
e.assert [:virtual_disk, 666, :active]
e.assert [:mdraid, 666, :failed]
e.dump_ruleset
e.dump_kb
e.retract [:mdraid, 666, :failed]
e.dump_kb
