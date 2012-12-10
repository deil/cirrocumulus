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

class NetworkChannel < AbstractChannel
  def initialize(local_instance, remote_identifier)
    @remote_identifier = remote_identifier
    @client = JabberChannel.query_client(local_instance.to_s)
  end

  def inform(sender, proposition, options = {})
    msg = [
      :inform,
      [:receiver,
        [:agent_identifier, :name, @remote_identifier.to_s]
      ],
      [:content, proposition]
    ]
    
    msg << [:reply_with, options[:reply_with]] if options.has_key?(:reply_with)
    msg << [:in_reply_to, options[:in_reply_to]] if options.has_key?(:in_reply_to)
    msg << [:conversation_id, options[:conversation_id]] if options.has_key?(:conversation_id)
    
    @client.queue(Sexpistol.new.to_sexp(msg))
  end

  def request(sender, action, options = {})
    msg = [
      :request,
      [:receiver,
        [:agent_identifier, :name, @remote_identifier.to_s]
      ],
      [:content, action]
    ]
    
    msg << [:reply_with, options[:reply_with]] if options.has_key?(:reply_with)
    msg << [:in_reply_to, options[:in_reply_to]] if options.has_key?(:in_reply_to)
    msg << [:conversation_id, options[:conversation_id]] if options.has_key?(:conversation_id)
    
    @client.queue(Sexpistol.new.to_sexp(msg))
  end

  def query(sender, expression, options = {})
    msg = [
      :query,
      [:receiver,
        [:agent_identifier, :name, @remote_identifier.to_s]
      ],
      [:content, expression]
    ]
    
    msg << [:reply_with, options[:reply_with]] if options.has_key?(:reply_with)
    msg << [:in_reply_to, options[:in_reply_to]] if options.has_key?(:in_reply_to)
    msg << [:conversation_id, options[:conversation_id]] if options.has_key?(:conversation_id)
    
    @client.queue(Sexpistol.new.to_sexp(msg))
  end

  def query_if(sender, proposition, options = {})
    msg = [
      :query_if,
      [:receiver,
        [:agent_identifier, :name, @remote_identifier.to_s]
      ],
      [:content, proposition]
    ]
    
    msg << [:reply_with, options[:reply_with]] if options.has_key?(:reply_with)
    msg << [:in_reply_to, options[:in_reply_to]] if options.has_key?(:in_reply_to)
    msg << [:conversation_id, options[:conversation_id]] if options.has_key?(:conversation_id)
    
    @client.queue(Sexpistol.new.to_sexp(msg))
  end
  
end

class ChannelFactory
  def self.retrieve(instance, agent)
    if agent.is_a?(LocalIdentifier)
      ontology_instance = Ontology.query_ontology_instance(agent)

      if ontology_instance
        return ThreadChannel.new(Ontology.query_ontology_instance(agent))
      else
        puts "[WARN] Thread-local ontology not found for identifier=%s" % agent.to_s
      end
    elsif agent.is_a?(RemoteIdentifier)
      return NetworkChannel.new(instance, agent.to_s)
    end

    nil
  end
end
