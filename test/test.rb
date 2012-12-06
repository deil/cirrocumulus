require 'thread'
require 'rubygems'
require 'sexpistol'
require_relative '../lib/cirrocumulus/identifier'
require_relative '../lib/cirrocumulus/channels'
require_relative '../lib/cirrocumulus/network_channel'
require_relative '../lib/cirrocumulus/facts'
require_relative '../lib/cirrocumulus/ontology'

class Saga1 < Saga
  saga 'start_vds'

  def start(arg)
    @arg = arg

    query(LocalIdentifier.new('xen'), [:free_memory])
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

	rule 'test1', [ [:test] ] do |ontology, params|
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
		ontology.query Agent.all('xen'), [:free_memory], :reply_with => '1'
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

agent = NetworkMonitoringOntology.new('network_monitor')
agent.assert([:start])
agent.run()

agent2 = HypervisorOntology.new('xen')
agent2.run()

agent.assert ['gw.mneko.net', :ping, 1]
sleep 2
agent.replace ['gw.mneko.net', :ping, :STATE], 0

puts "\nPress any key.."
gets

agent.join
agent2.join
