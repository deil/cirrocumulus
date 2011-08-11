require 'systemu'

class HDD
  def self.list()
    status, out = systemu('ls /dev/sd*')
    volumes = out.split("\n")
    disks = []
    volumes.each do |vol|
	if vol =~ /sd\w$/
	  disks << vol[/sd\w$/]
	end
    end
    
    disks
  end
  
  attr_reader :device
  
  def initialize(device)
    @device = device
  end
  
  def sn
    status, out = systemu("smartctl -i /dev/#{@device} | grep \"Serial Number\"")
    values = out.split(" ")
    values[2]
  end
  
  def capacity
    status, out = systemu("smartctl -i /dev/#{@device} | grep \"User Capacity\"")
    values = out.split(" ")
    cap = values[2].gsub(',', '').to_i / (1024*1024*1024)
  end
  
  def temperature
    get_int_value('Temperature')
  end
  
  def power_on_hours
    get_int_value('Hours')
  end
  
  def uncorrectable_sectors
    get_int_value('Uncorrectable')
  end
  
  def reallocated_sectors
    get_int_value('Reallocated_Sector')
  end
  
  def run_short_test()
    systemu("smartctl -t short /dev/#{@device}")
  end
  
  def test_result
    status, out = systemu("smartctl -l selftest /dev/#{@device} | grep \"\\# 1\"")
    return :unknown if out.empty?
    return :ok if out.include? "without error"
    :error
  end
  
  def health
    return :replace if test_result == :error || uncorrectable_sectors > 0 || power_on_hours > 2*365*24
    return :warning if reallocated_sectors > 0 || power_on_hours > 365*24 || test_result == :unknown
    return :ok
  end
  
  private
  
  def get_int_value(key)
    status, out = systemu("smartctl -A /dev/#{@device} | grep #{key}")
    cols = out.split(" ")
    cols.last.to_i
  end
  
end