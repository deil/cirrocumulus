require 'rubygems'
require 'erlectricity'

SLEEP_TIMEOUT = 3
stop = false

receive do |f|
    Thread.new do
	f.send!([:result, "started external script"])
	vps_time = {}
	vps_cpu = {}
	
	while not stop
	    xm_out = `xm list`.split("\n")
	    xm_out.each_with_index do |xm, i|
		if i > 0
		    begin
			vps = xm.split(" ")[0]
			time = xm.split(" ")[5].to_f
			if not vps_time[vps].nil? and vps != "Domain-0"
			    diff = time - vps_time[vps]
			    cpu_usage = (100 * (diff / (SLEEP_TIMEOUT * 4).to_f)).to_i
			    f.send!([:result, "VPS #{vps}: #{cpu_usage}%"]) unless vps_cpu[vps].nil? or vps_cpu[vps] == cpu_usage
			    vps_cpu[vps] = cpu_usage
			end
			vps_time[vps] = time
		    rescue Exception => ex
			f.send!([:result, ex.to_s])
		    end
		end
	    end

	    sleep(SLEEP_TIMEOUT)
	end
	
	f.send!([:result, "stopped external script"])
    end

    f.when([:echo, String]) do |text|
	f.send!([:result, "Yoo said: #{text}"])
	f.receive_loop
    end
end

stop = true