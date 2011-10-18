class Saga
  attr_reader :id
  attr_reader :context
  attr_reader :finished
  attr_accessor :timeout

  DEFAULT_TIMEOUT = 15
  LONG_TIMEOUT = 60
  
  STATE_ERROR = -1
  STATE_START = 0
  STATE_FINISHED = 255

  def initialize(id, ontology)
    @id = id
    @ontology = ontology
    @finished = false
    @timeout = -1
    @state = STATE_START
  end

  def is_finished?
    @finished || @state == STATE_ERROR
  end

  protected

  def clear_timeout()
    @timeout = -1
  end

  def set_timeout(secs)
    Log4r::Logger['agent'].debug "[#{id}] waiting for #{secs} second(s)" if secs > 1
    @timeout = secs*2
  end
  
  def change_state(new_state)
    Log4r::Logger['agent'].debug "[#{id}] switching state from #{@state} to #{new_state}"
    @state = new_state
  end
  
  def finish()
    clear_timeout()
    change_state(STATE_FINISHED)
    @finished = true
    Log4r::Logger['agent'].info "[#{id}] finished"
  end
  
  def error()
    change_state(STATE_ERROR)
  end
end
