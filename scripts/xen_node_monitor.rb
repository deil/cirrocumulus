require 'rubygems'
require 'erlectricity'
require '../scripts/vu.rb'

SLEEP_TIMEOUT = 10
stop = false
thread = nil

running_vus = []

receive do |f|
  thread = Thread.new do
    #f.send!([:result, "started external script1"])
    #vps_time = {}
    #vps_cpu = {}
	
    #f.send!([:result, "qweqwe"])

    while not stop
	current_vus = VirtualUnit.list_running()
	current_vus.each do |domain|
	    if !running_vus.include? domain
		f.send!([:assert, ["vu_running", domain]])
	    end
	end
    
	running_vus.each do |domain|
	    if !current_vus.include? domain
		f.send!([:retract, ["vu_running", domain]])
	    end
	end
    
	running_vus = current_vus
    
=begin
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
=end
      #f.send!([:result, "qweqwe"])
=begin
			if counter == 1
				f.send!([:assert, [:aoe_up, "111", "1"]])
				f.send!([:assert, [:aoe_up, "111", "2"]])
				f.send!([:assert, [:raid_active, "111", "2"]])
			elsif counter == 2
				f.send!([:assert, [:aoe_down, "111", "1"]])
			elsif counter == 3
				f.send!([:assert, [:aoe_up, "111", "1"]])
			end
			
			counter += 1
=end
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
thread.terminate!
