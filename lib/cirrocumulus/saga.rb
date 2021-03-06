#
# Saga. Implements long-running workflows
#
class Saga
  class << self
    @@saga_names = {}

    def saga(saga_name = nil)
      return @@saga_names[name] if saga_name.nil?
      @@saga_names[name] = saga_name
    end
  end

  STATE_ERROR = -1
  STATE_START = 0
  STATE_FINISHED = 255

  attr_reader :id

  def initialize(id, ontology)
    @id = id
    @ontology = ontology
    @state = STATE_START
    @started_at = Time.now
    @timeout_at = nil
  end

  def is_finished?
    @state == STATE_ERROR || @state == STATE_FINISHED
  end

  def tick
    return if @timeout_at.nil? || @timeout_at > Time.now

    @timeout_at = nil
    handle_reply(nil, nil, nil)
  end

  def handle_reply(sender, contents, options = {}); end

  def dump_parameters
    ""
  end

  def to_s
    "%s type=%s, started at %s, state=%d, params: %s" % [@id, @@saga_names[self.class.name], @started_at, @state, dump_parameters]
  end

  protected

  #
  # Inter-agent communications with context of this saga.
  #

  def inform(agent, proposition, options = {})
    @ontology.inform agent, proposition, options.merge(:conversation_id => self.id)
  end

  def agree(agent, action, options = {})
    @ontology.agree agent, action, options.merge(:conversation_id => self.id)
  end

  def refuse(agent, action, reason, options = {})
    @ontology.refuse agent, action, reason, options.merge(:conversation_id => self.id)
  end

  def failure(agent, action, reason = true)
    @ontology.failure agent, action, reason, options.merge(:conversation_id => self.id)
  end

  def request(agent, action, options = {})
    @ontology.request agent, action, options.merge(:conversation_id => self.id)
  end

  def query(agent, expression, options = {})
    @ontology.query agent, expression, options.merge(:reply_with => self.id)
  end

  def query_if(agent, proposition, options = {})
    @ontology.query_if agent, proposition, options.merge(:reply_with => self.id)
  end

  def finish()
    change_state(STATE_FINISHED)
  end

  def error()
    change_state(STATE_ERROR)
  end

  def change_state(new_state)
    @state = new_state
  end

  def timeout(secs)
    @timeout_at = Time.now + secs.seconds
  end

end
