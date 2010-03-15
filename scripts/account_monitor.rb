#!/usr/bin/ruby

require 'rubygems'
require 'erlectricity'
require '/opt/cirrocumulus/scripts/db.rb'

SLEEP_TIMEOUT = 3
stop = false
thread = nil

receive do |f|
    thread = Thread.new do
	f.send!([:result, "started account monitor"])
	accounts = Account.all(:conditions => {:is_active => true})

	while not stop
	    new_accounts = Account.all(:conditions => {:is_active => true})

	    new_accounts.each do |acc|
		if not accounts.include?(acc)
		    f.send!([:result, "new account: #{acc.uid} @ #{acc.created_at}"])
		end
	    end

	    accounts = new_accounts
	    sleep(SLEEP_TIMEOUT)
	end
	
	f.send!([:result, "stopped account monitor"])
    end
end

stop = true
thread.terminate!
