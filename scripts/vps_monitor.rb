require 'rubygems'
require 'erlectricity'

stop = false

receive do |f|
    Thread.new do
	f.send!([:result, "started external script"])
	prev = 0
	while not stop
	    processes = `ps aux | wc -l`
	    if processes != prev
		f.send!([:result, "Total processes: %s" % [processes]])
	    end

	    prev = processes
	    sleep(0.1)
	end
	
	f.send!([:result, "stopped external script"])
    end

    f.when([:echo, String]) do |text|
	f.send!([:result, "Yoo said: #{text}"])
	f.receive_loop
    end
end

stop = true