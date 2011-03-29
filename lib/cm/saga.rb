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

  def initialize(id, cm, agent)
    @agent = agent
    @cm = cm
    @id = id
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
    Log4r::Logger['agent'].debug "waiting for #{secs} second(s) [#{id}]" if secs > 1
    @timeout = secs*2
  end
  
  def change_state(new_state)
    Log4r::Logger['agent'].debug "switching state from #{@state} to #{new_state} [#{id}]"
    @state = new_state
  end
  
  def finish()
    clear_timeout()
    change_state(STATE_FINISHED)
    @finished = true
    Log4r::Logger['agent'].info "finished [#{id}]"
  end
  
  def error()
    change_state(STATE_ERROR)
  end
end
