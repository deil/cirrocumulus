class VpsRestartSaga < Saga
  attr_reader :vps_id

  STATE_CHECK_RUNNING = 1
  STATE_STOP_DOMU = 3

  def start(vps_id, context)
    @context = context
    @vps_id = vps_id
    Log4r::Logger['agent'].info "attempting to restart VPS id=#{@vps_id}"

    @selected_node = nil
    @vps = VpsConfiguration.find(@vps_id)
    check_running()
  end

  def handle(message)
    case @state
      when STATE_CHECK_RUNNING
        if message.nil?
          notify_not_running()
          finish()
        else
          status = message.content.first
          #if status == :not
            #notify_not_running()
            #finish()
          if status == :running
            @selected_node = message.sender
            restart_domu()
            notify_finished()
            finish()
          #else
            #error()
          end
        end
    end
  end

  private

  def check_running()
    message = Cirrocumulus::Message.new(nil, 'query-if', Sexpistol.new.to_sexp([:running, [:domu, @vps.vps_id]]))
    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
    set_timeout(10)
    change_state(STATE_CHECK_RUNNING)
  end

  def restart_domu()
    message = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp(
    [
      :restart,
      [:domu,
        [:name, @vps.vps_id],
      ]
    ]))

    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    message.receiver = @selected_node
    @cm.send(message)
  end

  def notify_not_running()
    msg = Cirrocumulus::Message.new(nil, 'refuse', Sexpistol.new.to_sexp([requested_action, [:not_running]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end

  def notify_finished()
    msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([requested_action, [:finished]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  
  def requested_action()
    [:restart_vps, [:id, @vps.id]]
  end
end
