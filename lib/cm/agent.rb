class Agent
  attr_reader :default_ontology

  def tick()
  end

  def handles_ontology?(ontology)
    return @default_ontology == ontology
  end
  
  def self.get_node(sender)
    sender.split('-').first
  end
  
  def self.get_subsystem(sender)
    sender.split('-').second
  end
end