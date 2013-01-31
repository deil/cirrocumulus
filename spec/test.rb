require 'bundler/setup'
require_relative '../lib/cirrocumulus'
require_relative '../lib/cirrocumulus/remote_console'
require_relative '../lib/cirrocumulus/rules/run_queue'
require_relative '../lib/cirrocumulus/rules/engine'

r = RuleEngine::Base.new
r.assert [:guest, "123", :cpu_time, 123.45]
p r.query([:guest, "123", :cpu_time, 123.45])
p r.query([:guest, "123", :cpu_time, :CPU_TIME])
p r.query([:guest1, :GUEST_ID, :cpu_time, :CPU_TIME])

class Saga1 < Saga
  saga 'start_vds'

  def start(arg)
    @arg = arg

    query(Agent.remote('deuterium-xen'), [:free_memory])
  end

  def dump_parameters
    "VDS=%s" % [@arg]
  end

  def handle_reply(sender, contents, options = {})
    p sender
    p contents

    case @state
      when STATE_START
        puts "STATE_START"
    end
  end
end

class HypervisorOntology < Ontology
	ontology 'xen'

	rule 'test1', [ [:spec] ] do |ontology, params|
  end

  trigger 'test2', :for => [:spec] do |ontology|
    puts "assert"
  end

  trigger 'test3', :for => [:spec], :on => :retract do |ontology|
    puts "retract"
  end

  def handle_query(sender, expression, options = {})
    if expression == [:free_memory]
      inform(sender, [[:free_memory], 1024], reply(options))
    else
      super(sender, expression, options)
    end
  end

	def handle_request(sender, contents, options = {})
		puts "%25s | %s requests %s" % [identifier.to_s, sender.to_s, Sexpistol.new.to_sexp(contents)]
	end
end

class NetworkMonitoringOntology < Ontology
	ontology 'network_monitor'

	rule 'test', [ [:start] ] do |ontology, params|
		puts "hello world"
		ontology.query Agent.remote('deuterium-xen'), [:free_memory], :reply_with => '1'
		ontology.request LocalIdentifier.new('xen'), [:greet], :reply_with => 'greeting_test'
	end

	rule 'host_is_down', [ [:HOST, :ping, 0] ] do |ontology, params|
    saga = ontology.create_saga(Saga1)
    saga.start('66ecb')
	end

	def handle_request(sender, contents, options = {})
		puts "%25s | %s requests %s" % [identifier, sender, Sexpistol.new.to_sexp(contents)]
	end
end

Ontology.enable_console()

cc = Cirrocumulus::Environment.new('deuterium')

agent = NetworkMonitoringOntology.new(Agent.network('network_monitor'))
cc.load_ontology(agent)

agent.assert([:start])

agent2 = HypervisorOntology.new(Agent.network('xen'))
cc.load_ontology(agent2)

cc.run

agent.assert ['gw.mneko.net', :ping, 1]
sleep 2
agent.replace ['gw.mneko.net', :ping, :STATE], 0

puts "\nPress any key.."
gets

cc.join
