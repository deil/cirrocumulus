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

  def assert(identifier, data)
    @cm.assert(identifier, data)
  rescue DRb::DRbError => ex
  end

  def retract(identifier, data)
    @cm.retract(identifier, data)
  rescue DRb::DRbError => ex
  end

  def dump_kb(identifier)
    @cm.dump_kb(identifier)
  rescue DRb::DRbError => ex
    []
  end

  def dump_sagas(identifier)
    @cm.dump_sagas(identifier)
  rescue DRb::DRbError => ex
    []
  end
end

DRb.start_service
cm = CmConsole.new

loop do
  line = ask('cirrocumulus> ') {|q| q.readline = true}
  args = line.split(" ")
  cmd = args.first

  case cmd
	  when 'quit'
		  break

	  when 'list'
		  puts "Inproc agent instances:"
		  inproc = cm.list_inproc_agents
			inproc.each do |identifier|
					puts "* %s" % identifier
      end

    when 'assert'
      agent = args[1].to_s
      2.times { args.delete_at(0) }
      data = args.join(' ')
      puts "assert %s to %s" % [data, agent]
      cm.assert(agent, data)

    when 'retract'
      agent = args[1].to_s
      2.times { args.delete_at(0) }
      data = args.join(' ')
      puts "retract %s from %s" % [data, agent]
      cm.retract(agent, data)

    when 'kb'
      agent = args[1].to_s
      puts "dumping KB of %s:" % agent
      facts = cm.dump_kb(agent)
      facts.each do |fact|
        puts "* %s" % fact
      end

      puts "KB is yet empty." if facts.empty?

    when 'sagas'
      agent = args[1].to_s
      puts "dumping sagas of %s:" % agent
      sagas = cm.dump_sagas(agent)
      sagas.each do |saga|
        puts "* %s" % saga
      end

      puts "No active sagas right now." if sagas.empty?


    else
		  puts "Unknown command: %s" % cmd
  end
end
