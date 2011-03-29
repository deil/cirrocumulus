class DiskAttachSaga < Saga
  attr_reader :vps_id
  attr_reader :disk_number
  attr_reader :block_device

  STATE_SEARCHING_NODE = 1
  STATE_STOPPING_ACTIVE_RAID = 2
  STATE_ASSEMBLING_RAID = 3
  STATE_WAITING_FOR_REPLY = 4

  def start(vps_id, disk_number, block_device, context)
    @context = context
    @vps_id = vps_id
    @disk_number = disk_number
    @block_device = block_device
    
    vps = VpsConfiguration.find(@vps_id)
    @vps_uid = vps.vps_id
    @selected_node = nil
    Log4r::Logger['agent'].info "attaching disk=#{disk_number} to VPS=#{@vps_id} as #{@block_device}"
    handle(nil)
  end

  def handle(message)
    case @state
      when STATE_START
        search_node()
        change_state(STATE_SEARCHING_NODE)
        set_timeout(DEFAULT_TIMEOUT)
        
      when STATE_SEARCHING_NODE
        if message.nil? # timeout
          if @selected_node
            stop_active_raid()
            start_raid()
            change_state(STATE_ASSEMBLING_RAID)
            set_timeout(DEFAULT_TIMEOUT)
          else
            Log4r::Logger['agent'].warn "VPS=#{@vps_id} is not running. Stop [#{id}]"
            notify_failure(:not_running)
            error()
          end
        else
         if message.act == 'inform' && message.content == [:running, [:domu, @vps_uid]]
           @selected_node = message.sender
           Log4r::Logger['agent'].info "VPS=#{@vps_id} is running on #{@selected_node} [#{id}]"
           set_timeout(1)
         end
        end

      when STATE_ASSEMBLING_RAID
        if message.nil? # timeout
          Log4r::Logger['agent'].warn "Unable to assemble RAID. Node is not responding. Stop [#{id}]"
          notify_failure(:node_not_responding)
          error()
        else
          if message.act == 'inform'
            Log4r::Logger['agent'].debug "node replied, attaching disk [#{id}]"
            attach_disk()
            change_state(STATE_WAITING_FOR_REPLY)
            set_timeout(DEFAULT_TIMEOUT)
          elsif message.act == 'failure'
            Log4r::Logger['agent'].warn "failed to assemble RAID on #{@selected_node}, stop. [#{id}]"
            notify_failure(:unknown_error)
            error()
          end
        end

      when STATE_WAITING_FOR_REPLY
        if message.nil?
          finish()
          notify_finished()
        elsif message.act == 'failure'
          notify_failure(:unknown_reason)
          error()
        elsif message.act == 'inform'
          finish()
          notify_finished()
        end
      
    else
      Log4r::Logger['agent'].warn "unhandled message:"
      p message
    end
  end

  private
  
  def search_node()
    Log4r::Logger['agent'].debug "searching where VPS=#{@vps_id} is running [#{id}]"
    message = Cirrocumulus::Message.new(nil, 'query-if', Sexpistol.new.to_sexp([:running, [:domu, @vps_uid]]))
    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end
  
  def stop_active_raid()
    message = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([:stop, [:raid, @disk_number]]))
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end
  
  def start_raid()
    msg = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([:start, [:raid, @disk_number]]))
    msg.ontology = 'cirrocumulus-xen'
    msg.receiver = @selected_node
    msg.reply_with = "#{id}"
    @cm.send(msg)
  end
  
  def attach_disk()
    msg = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([
      :attach_disk,
      [
        [:domu, @vps_uid], [:disk_number, @disk_number], [:block_device, 'xvdb']
      ]
    ]))
    msg.ontology = 'cirrocumulus-xen'
    msg.receiver = @selected_node
    msg.reply_with = "#{id}"
    @cm.send(msg)
  end

  def notify_failure(reason)
    msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([
          [
            :attach_disk, [:vps_id, @vps_id], [:disk_number, @disk_number], [:block_device, @block_device]
          ],
          [reason]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end

  def notify_finished()
    msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([
          [
            :attach_disk, [:vps_id, @vps_id], [:disk_number, @disk_number], [:block_device, @block_device]
          ],
          [:finished]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
end
