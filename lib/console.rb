require 'drb'
require 'rubygems'
require 'highline/import'

class CmConsole
	def initialize
		@cm = DRbObject.new_with_uri('druby://127.0.0.1:8112')
	end

	def list_inproc_agents
		@cm.list_inproc_agents()
	rescue DRb::DRbError => ex
		[]
	end
end

DRb.start_service
cm = CmConsole.new

loop do
  cmd = ask('cirrocumulus> ') {|q| q.readline = true}

  case cmd
	  when 'quit'
		  break

	  when 'list'
		  puts "Inproc agent instances:"
		  inproc = cm.list_inproc_agents
			inproc.each do |identifier|
					puts "* %s" % identifier
			end

    else
		  puts "Unknown command: %s" % cmd
  end
end
