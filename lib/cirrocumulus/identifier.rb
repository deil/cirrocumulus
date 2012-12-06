class Agent
	def self.all(ontology_name = nil)
		if ontology_name == nil
			Broadcast.new
		else
			Autodiscover.new(ontology_name)
		end
	end

	def self.remote(agent_identifier)
		self.new(agent_identifier)
	end

	def initialize(agent_identifier)
		@agent_identifier = agent_identifier
	end

	def to_s
		@agent_identifier
	end
end

class Autodiscover
	def initialize(ontology_name)
		@ontology_name = ontology_name
	end

	def to_s
		"*-%s" % @ontology_name
	end
end

class Broadcast
	def to_s
		"(broadcast)"
	end
end

class LocalIdentifier
	def initialize(ontology_name)
		@ontology_name = ontology_name
	end

	def ==(other)
		return false if other.nil?
		return false if !other.is_a?(LocalIdentifier)

		return to_s == other.to_s
	end

	def hash
		return to_s.hash
	end

	def eql?(other)
		return false if other.nil?
		return false if !other.is_a?(LocalIdentifier)

		return to_s.eql?(other)
	end

	def to_s
		"local-%s" % @ontology_name
	end
end
