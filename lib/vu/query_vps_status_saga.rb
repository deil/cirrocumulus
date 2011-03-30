class QueryVpsStatusSaga < Saga
  attr_reader :vps_id

  STATE_CHECK_RUNNING = 1

  def start(vps_id, context)
    @context = context
    @vps_id = vps_id
    Log4r::Logger['agent'].info "query VPS state id=#{@vps_id} [#{id}]"

    @vps = VpsConfiguration.find(@vps_id) if VpsConfiguration.exists? @vps_id
    check_running()
  end

  def handle(message)
    if @state == STATE_CHECK_RUNNING
      if message.nil? # timeout
        msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([:"=", [:vps_state, [:id, @vps_id]], [:not_running]]))
        msg.ontology = 'cirrocumulus-vps'
        msg.receiver = @receiver
        @cm.send(msg)
        update_vps_state(false, nil)
        finish()
      else
        if message.content.first == :running
          domU = message.content.second
          if domU.second == @vps.vps_id
            msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([:"=", [:vps_state, [:id, @vps_id]], [:running, message.sender]]))
            msg.ontology = 'cirrocumulus-vps'
            msg.receiver = context.sender
            msg.in_reply_to = context.reply_with
            @cm.send(msg)
            update_vps_state(true, message.sender)
            finish()
          end
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
    @state = STATE_CHECK_RUNNING
    set_timeout(DEFAULT_TIMEOUT)
  end

  def update_vps_state(is_running, node)
    state = VpsState.find_by_vps_configuration_id(@vps.id)
    state = VpsState.create(:vps_configuration_id => @vps.id, :vnc => false, :maintenance => false, :running => is_running, :running_on => node) if state.nil?
    state.running_on = node
    state.running = is_running
    state.save
  end
end
