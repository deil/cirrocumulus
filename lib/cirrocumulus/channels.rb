require_relative 'channels/jabber'

#
# Communication channel between agents. This is an abstract class.
#
class AbstractChannel
  #
  # Informs the other side that given proposition is true.
  #
  def inform(sender, proposition, options = {}); end

  #
  # Request the other side to perform an action
  #
  def request(sender, action, options = {}); end

  #
  # Query the other side about expression value.
  #
  def query(sender, expression, options = {}); end

  #
  # Query the other side if given proposition is true.
  #
  def query_if(sender, proposition, options = {}); end

  #
  # Agree to perform an action
  #
  def agree(sender, action, options = {}); end

  #
  # Refuse to perform an action, because of reason.
  #
  def refuse(sender, action, reason, options = {}); end

  #
  # Attempted to perform an action, but failed with given reason.
  #
  def failure(sender, action, reason, options = {}); end
end

#
# Communication channel between different threads on local machine.
#
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

  def agree(sender, action, options = {})
    @ontology.handle_agree(sender, action, options)
  end

  def refuse(sender, action, reason, options = {})
    @ontology.handle_refuse(sender, action, reason, options)
  end

  def failure(sender, action, reason, options = {})
    @ontology.handle_failure(sender, action, reason, options)
  end
end

#
# Communication channel over network.
#
class NetworkChannel < AbstractChannel
  def initialize(client, remote_jid)
    @remote_jid = remote_jid
    @client = client
    @serializer = Sexpistol.new
  end

  def inform(sender, proposition, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :inform, proposition, options)))
  end

  def request(sender, action, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :request, action, options)))
  end

  def query(sender, expression, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :query, expression, options)))
  end

  def query_if(sender, proposition, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :query_if, proposition, options)))
  end

  def agree(sender, action, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :agree, action, options)))
  end

  def refuse(sender, action, reason, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :refuse, [action, reason], options)))
  end

  def failure(sender, action, reason, options = {})
    client.queue(serializer.to_sexp(build_message(remote_jid, :failure, [action, reason], options)))
  end

  private

  attr_reader :client
  attr_reader :remote_jid
  attr_reader :serializer

  def build_message(receiver_name, act, content, options)
    msg = [
      act,
      [:receiver,
        [:agent_identifier, :name, receiver_name]
      ],
      [:content, content]
    ]

    msg << [:reply_with, options[:reply_with]] if options.has_key?(:reply_with)
    msg << [:in_reply_to, options[:in_reply_to]] if options.has_key?(:in_reply_to)
    msg << [:conversation_id, options[:conversation_id]] if options.has_key?(:conversation_id)

    msg
  end

end

#
# Factory to retrieve proper communication channel for two agents.
#
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
      jabber_client = JabberChannel.query_client(instance.to_s)

      if jabber_client
        return NetworkChannel.new(jabber_client, agent.to_s)
      else
        puts "[WARN] No active Jabber clients."
      end
    end

    puts "[WARN] No suitable channel found for #{agent.to_s} (#{agent.class.name})"
    nil
  end
end
