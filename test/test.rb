require 'bundler/setup'
require 'active_support'
require 'cirrocumulus/rule_engine'

class Test < RuleEngine::Base
  rule 'convert', [[:temperature, :X, 'F']] do |engine, params|
    puts "qqq"
    x = params[:X].to_i
    engine.retract([:temperature, x, 'F'])
    y = 5*(x - 32)/9
    engine.assert([:temperature, y, 'C'])
  end

  rule 'test', [ [:a, :B, :c], [:c, :B, :a]], :for => 5.seconds do |engine, params|
    p "test"
    p params
  end
=begin
  rule 'guest_powered_off', [[:guest, :X, :powered_off]], :for => 5.seconds do |engine, params|
    puts "guest_powered_off"
  end
  
  rule 'monitor_md', [[:virtual_disk, :X, :active], [:mdraid, :X, :failed]] do |engine, params|
    # md devices is failed, but virtual disk should be up
    p params
    puts "virtual disk #{params[:X]} should be up, but corresponding md devices is failed!"
  end
=end
end

#RuleEngine::Server.run()

e = Test.new

#e.assert [:i, :will, :expire, :soon], :expires => 10.seconds
#e.replace [:guest, :X, :powered_off], "q1w2", :expires => 2.seconds
e.assert [:a, 'b', :c]
e.assert [:a, 'haha', :c], :expires => 3.seconds
e.assert [:c, 'b', :a], :expires => 6.seconds

e.start()

while true
  sleep 1
end

exit 0

e.assert [:guest, "233bed174ab0802fd908f981d64d185b", :powered_off]
e.assert [:guest, "233bed174ab0802fd908f981d64d185b", :running]
e.assert [:guest, "233bed174ab0802fd908f981d64d185b", :state, :powered_on]
e.replace [:guest, "233bed174ab0802fd908f981d64d185b", :state, :STATE], :powered_off

p e.match [:guest, "233bed174ab0802fd908f981d64d185b", :running]
#while true do
  e.tick()
#  sleep 1
#end

gets
e.dump_kb()
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
