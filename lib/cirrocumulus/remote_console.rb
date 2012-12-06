require 'drb'
require 'sexpistol'
require_relative 'identifier'

class RemoteConsole
	def initialize

	end

	def list_inproc_agents
		Ontology.list_ontology_instances()
  end

  def assert(instance, data)
    Ontology.assert(LocalIdentifier.new(instance), Sexpistol.new.parse_string(data)[0])
  end

  def retract(instance, data)
    Ontology.retract(LocalIdentifier.new(instance), Sexpistol.new.parse_string(data)[0])
  end

  def dump_kb(instance)
    Ontology.dump_kb(LocalIdentifier.new(instance))
  end

  def dump_sagas(instance)
    Ontology.dump_sagas(LocalIdentifier.new(instance))
  end
end
