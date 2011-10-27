require 'cirrocumulus/rule_engine.rb'
require 'cirrocumulus/rule_server.rb'

class Test < RuleEngine::Base
  rule 'convert', [[:temperature, :X, 'F']] do |engine, params|
    puts "qqq"
    x = params[:X].to_i
    engine.retract([:temperature, x, 'F'])
    y = 5*(x - 32)/9
    engine.assert([:temperature, y, 'C'])
  end
  
  rule 'guest_powered_off', [[:guest, :X, :powered_off]] do |engine, params|
    puts "guest_powered_off"
  end
  
  rule 'monitor_md', [[:virtual_disk, :X, :active], [:mdraid, :X, :failed]] do |engine, params|
    # md devices is failed, but virtual disk should be up
    p params
    puts "virtual disk #{params[:X]} should be up, but corresponding md devices is failed!"
  end
end

#RuleEngine::Server.run()

e = Test.new
e.assert [:guest, "233bed174ab0802fd908f981d64d185b", :powered_off]
e.assert [:guest, "233bed174ab0802fd908f981d64d185b", :running]
e.assert [:guest, "233bed174ab0802fd908f981d64d185b", :state, :powered_on]
e.replace [:guest, "233bed174ab0802fd908f981d64d185b", :state, :STATE], :powered_off

p e.match [:guest, "233bed174ab0802fd908f981d64d185b", :running]
gets
exit(0)
e.assert [:virtual_disk, 163, :active]
#e.assert [:virtual_disk, 139, :active]
#e.assert [:virtual_disk, 145, :active]
#e.assert [:virtual_disk, 146, :active]
#e.assert [:virtual_disk, 149, :active]
e.assert [:virtual_disk, 153, :active]
#e.assert [:virtual_disk, 154, :active]
#e.assert [:virtual_disk, 156, :active]
e.assert [:virtual_disk, 158, :active]
#e.assert [:virtual_disk, 137, :active]
#e.assert [:virtual_disk, 135, :active]
#e.assert [:virtual_disk, 159, :active]
#e.assert [:virtual_disk, 103, :active]
#e.assert [:virtual_disk, 102, :active]
#e.assert [:virtual_disk, 20, :active]
#e.assert [:virtual_disk, 2, :active]
#e.assert [:virtual_disk, 777, :active]
#e.assert [:virtual_disk, 90, :active]
e.assert [:mdraid, 153, :failed], true
e.assert [:mdraid, 158, :failed], true
e.execute()
gets
