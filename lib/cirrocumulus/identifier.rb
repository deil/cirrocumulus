class Agent
  def self.local(instance_name)
    LocalIdentifier.new(instance_name)
  end
  
  def self.jabber(ontology_name)
    JabberIdentifier.new(ontology_name)
  end
  
	def self.all(ontology_name = nil)
		if ontology_name == nil
			Broadcast.new
		else
			Autodiscover.new(ontology_name)
		end
	end

	def self.remote(agent_identifier)
		RemoteIdentifier.new(agent_identifier)
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

class RemoteIdentifier
  def initialize(remote_instance_name)
    @remote_instance_name = remote_instance_name
  end
  
  def ==(other)
    return false if other.nil? || !other.is_a?(RemoteIdentifier)
    
    return to_s == other.to_s
  end
  
  def hash
    to_s.hash
  end
  
  def eql?(other)
    self == other
  end
  
  def to_s
    @remote_instance_name
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
    self == other
  end

  def to_s
    "local-%s" % @ontology_name
  end
end
