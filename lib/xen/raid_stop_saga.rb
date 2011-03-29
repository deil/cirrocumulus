class RaidStopSaga < Saga

  def start(disk_number, context)
    @context = context
    @disk_number = disk_number
    Log4r::Logger['agent'].info "stoping /dev/md#{disk_number} [#{id}]"
    handle()
  end
  
  def handle(message = nil)
    case @state
      when STATE_START
        raid_state = Raid::check_raid(@disk_number)
        if raid_state == :active || raid_state == :failed
          result = Raid::stop_raid(@disk_number)
          if result
            notify_finished()
            finish()
          else
            Log4r::Logger['agent'].warn "failed to stop array /dev/md#{@disk_number} [#{id}]"
            notify_failure(:unknown_error)
            error()
          end
        elsif raid_state == :stopped
          Log4r::Logger['agent'].debug "RAID array is already stoped, finished [#{id}]"
          notify_finished()
          finish()
        end
    end
  end
  
  private
  
  def notify_finished()
    msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([
              [:stop, [:raid, @disk_number]], [:finished]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  
  def notify_failure(err)
    msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([
              [:stop, [:raid, @disk_number]], [err]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
end