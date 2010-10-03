#!/usr/bin/ruby

require 'rubygems'
require 'systemu'

class VirtualUnit
    def self.list_running
	list = []
	
	status, out, err = systemu "xm list"
	domains = out.split("\n")
	domains.each_with_index do |domain, i|
    	    next if i == 0
	    info = domain.split(" ")
	    domain_name = info.first
    	    domain_id = info[1].to_i
    	    if domain_id > 0
		list << domain_name
	    end
	end
	
	list
    end
end
