require 'rubygems'
require 'erlectricity'

receive do |f|
    prev = 0
    while true do
	processes = `ps aux | wc -l`
	if processes != prev
	    f.send!([:result, "Total processes: %s" % [processes]])
	end
	
	prev = processes
	sleep(0.1)
    end
end

#    f.when([:echo, String]) do |text|
#	f.send!([:result, "Yoo said: #{text}"])
#	f.receive_loop
#    end
#
#    uptime = %x("uptime")
#    f.send!([:result, uptime])
#    sleep(1)
#    f.receive_loop
