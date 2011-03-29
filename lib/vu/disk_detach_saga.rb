class DiskDetachSaga < Saga
  attr_reader :vps_id
  attr_reader :block_device

  STATE_SEARCHING_NODE = 1
  STATE_WAITING_FOR_REPLY = 2
  STATE_STOPPING_RAID = 3

  def start(vps_id, disk_number, block_device, context)
    @context = context
    @vps_id = vps_id
    @disk_number = disk_number
    @block_device = block_device

    vps = VpsConfiguration.find(@vps_id)
    @vps_uid = vps.vps_id
    @selected_node = nil
    Log4r::Logger['agent'].info "detaching #{@block_device} from VPS=#{@vps_id}"
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
            detach_disk()
            change_state(STATE_WAITING_FOR_REPLY)
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
           clear_timeout()
           handle(nil)
         end
        end

      when STATE_WAITING_FOR_REPLY
        if message.nil?
          Log4r::Logger['agent'].warn "node didn't reply, stop [#{id}]"
          notify_failure(:node_not_responding)
          error()
        elsif message.act == 'failure'
          notify_failure(:unknown_reason)
          error()
        elsif message.act == 'inform'
          stop_detached_raid()
          change_state(STATE_STOPPING_RAID)
          set_timeout(DEFAULT_TIMEOUT)
        end

    when STATE_STOPPING_RAID
      if message.nil?
        Log4r::Logger['agent'].warn "#{@selected_node} hasn't confirmed (stop (raid #{@disk_number})), but disk is detached [#{id}]"
      elsif message.act == 'inform'
        Log4r::Logger['agent'].info "RAID stopped successfully [#{id}]"
      elsif message.act == 'failure'
        Log4r::Logger['agent'].warn "error while stopping RAID, but disk is detached [#{id}]"
      end

      finish()
      notify_finished()

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

  def detach_disk()
    msg = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([
      :detach_disk,
      [
        [:domu, @vps_uid], [:block_device, @block_device]
      ]
    ]))
    msg.ontology = 'cirrocumulus-xen'
    msg.receiver = @selected_node
    msg.reply_with = "#{id}"
    @cm.send(msg)
  end
  
  def stop_detached_raid()
    message = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([:stop, [:raid, @disk_number]]))
    message.receiver = @selected_node
    message.reply_with = id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end

  def notify_failure(reason)
    msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([
          [
            :detach_disk, [:vps_id, @vps_id], [:block_device, @block_device]
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
            :detach_disk, [:vps_id, @vps_id], [:block_device, @block_device]
          ],
          [:finished]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
end
