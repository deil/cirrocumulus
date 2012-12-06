class AbstractChannel
  def inform(sender, proposition); end
	def request(sender, action, options = {}); end
  def query(sender, expression, options = {}); end
  def query_if(sender, proposition, options = {}); end
end

class ThreadChannel < AbstractChannel
	def initialize(ontology)
		@ontology = ontology
  end

  def inform(sender, proposition, options = {})
    @ontology.handle_inform(sender, proposition, options)
  end

	def request(sender, action, options = {})
		@ontology.handle_request(sender, action, options)
  end

  def query(sender, expression, options = {})
    @ontology.handle_query(sender, expression, options)
  end

  def query_if(sender, proposition, options = {})
    @ontology.handle_query_if(sender, proposition, options)
  end
end

class ChannelFactory
	def self.retrieve(agent)
		if agent.is_a?(LocalIdentifier)
			ontology_instance = Ontology.query_ontology_instance(agent)

			if ontology_instance
				return ThreadChannel.new(Ontology.query_ontology_instance(agent))
			else
				puts "[WARN] Thread-local ontology not found for identifier=%s" % agent.to_s
			end
		end

		nil
	end
end
