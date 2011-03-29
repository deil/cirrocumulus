class Agent
  attr_reader :default_ontology
  
  def initialize()
    @sagas = []
    @saga_idx = 0
  end

  def tick()
    @sagas.each do |saga|
      next if saga.is_finished?
      saga.timeout -= 1 if saga.timeout > 0
      saga.handle(nil) if saga.timeout == 0
    end
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