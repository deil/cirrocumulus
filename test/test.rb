require 'thread'
require 'rubygems'
require 'sexpistol'
require_relative '../lib/cirrocumulus/identifier'
require_relative '../lib/cirrocumulus/channels'
require_relative '../lib/cirrocumulus/facts'
require_relative '../lib/cirrocumulus/ontology'

class HypervisorOntology < Ontology
	ontology 'xen'

	rule 'test1', [] do |ontology, params|
		puts "heh.."
	end

	def handle_request(sender, contents)
		puts "%s | %s: %s" % [identifier.to_s, sender.to_s, Sexpistol.new.to_sexp(contents)]
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
		puts "host_is_down: %s" % params[:HOST]
		ontology.request LocalIdentifier.new('xen'), [:test]
	end

	def handle_request(sender, contents)
		puts "%s | %s: %s" % [identifier.to_s, sender.to_s, Sexpistol.new.to_sexp(contents)]
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

puts "Press any key.."
gets

agent.join
agent2.join
