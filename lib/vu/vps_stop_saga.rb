class VpsStopSaga < Saga
  attr_reader :vps_id

  STATE_CHECK_RUNNING = 1
  STATE_STOP_DOMU = 2
  STATE_WAITING_FOR_REPLY = 3
  STATE_STOPPING_RAID = 4

  def start(vps_id, context)
    @context = context
    @vps_id = vps_id
    Log4r::Logger['agent'].info "attempting to stop VPS id=#{@vps_id}"

    @selected_node = nil
    @vps = VpsConfiguration.find(@vps_id)
    check_running()
  end

  def handle(message = nil)
    case @state
      when STATE_CHECK_RUNNING
        if message.nil?
          notify_not_running()
          finish()
        else
          status = message.content.first
          if status == :running
            @selected_node = message.sender
            stop_domu()
            change_state(STATE_WAITING_FOR_REPLY)
            set_timeout(DEFAULT_TIMEOUT)
          end
        end
        
      when STATE_WAITING_FOR_REPLY
        if message.nil?
          Log4r::Logger['agent'].warn "#{@selected_node} didn't confirm domU stop, failure [#{id}]"
          notify_failure(:node_not_responsing)
          error()
        else
         if message.act == 'inform'
           Log4r::Logger['agent'].info "VPS successfully stopped [#{id}]"
           update_vps_state()
           stop_vps_raids()
           change_state(STATE_STOPPING_RAID)
           set_timeout(DEFAULT_TIMEOUT)
         elsif message.act == 'failure'
           Log4r::Logger['agent'].warn "failed to stop domU [#{id}]"
           notify_failure(:unknown_error)
           error()
         end
        end
        
      when STATE_STOPPING_RAID
        if message.nil?
          if @queued_raids.all? {|r| r[:stopped]}
            Log4r::Logger['agent'].info "all RAID disks are successfully stopped [#{id}]"
          else
            Log4r::Logger['agent'].warn "#{@selected_node} didn't confirm RAID stop, but VPS successully stopped [#{id}]"
          end
          
          notify_finished()
          finish()
        else
          if message.act == 'inform'
            action = message.content.first
            status = message.content.second # :finished
            obj = action.second.first
            disk_number = action.second.second.to_i
            raid = @queued_raids.find {|r| r[:disk_number] == disk_number}
            raid[:stopped] = true
            Log4r::Logger['agent'].debug "/dev/md#{raid[:disk_number]} stopped [#{id}]"
            
            if @queued_raids.all? {|r| r[:stopped]}
              clear_timeout()
              set_timeout(1)
            end
          elsif message.act == 'failure'
            action = message.content
            obj = action.second.first
            disk_number = action.second.second.to_i
            raid = @queued_raids.find {|r| r[:disk_number] == disk_number}
            raid[:stopped] = false
            Log4r::Logger['agent'].warn "/dev/md#{raid[:disk_number]} failed to stop [#{id}]"
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

  def stop_domu()
    message = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp(
    [
      :stop,
      [:domu,
        [:name, @vps.vps_id],
      ]
    ]))

    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    message.receiver = @selected_node
    @cm.send(message)
  end
  
  def stop_vps_raids()
    @queued_raids = []
    @vps.storage_disks.each do |disk|
      msg = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([
        :stop, [:raid, disk.disk_number]
      ]))
      msg.ontology = 'cirrocumulus-xen'
      msg.receiver = @selected_node
      msg.reply_with = @id
      @cm.send(msg)
      @queued_raids << {:disk_number => disk.disk_number, :stopped => false}
    end
  end
  
  def notify_failure(reason)
    msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([[:stop_vps, [:id, @vps.id]], [reason]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  

  def notify_not_running()
    msg = Cirrocumulus::Message.new(nil, 'refuse', Sexpistol.new.to_sexp([[:stop_vps, [:id, @vps.id]], [:not_running]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end

  def notify_finished()
    msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([[:stop_vps, [:id, @vps.id]], [:finished]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  
  def update_vps_state()
    state = VpsState.find_by_vps_configuration_id(@vps.id)
    state = VpsState.create(:vps_configuration_id => @vps.id, :vnc => false, :maintenance => false, :running => false, :running_on => @selected_node) if state.nil?
    state.running = false
    state.save
  end
end
