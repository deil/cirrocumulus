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
  end

  def is_finished?
    @state == STATE_ERROR || @state == STATE_FINISHED
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
  def inform(agent, proposition)
    @ontology.inform agent, proposition, :conversation_id => self.id
  end

  def request(agent, action)
    @ontology.request agent, action, :conversation_id => self.id
  end

  def query(agent, expression)
    @ontology.query agent, expression, :reply_with => self.id
  end

  def query_if(agent, proposition)
    @ontology.query_if agent, proposition, :reply_with => self.id
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

end
