class RaidAssembleSaga < Saga

  STATE_CHECKING_EXPORTS = 1

  def start(disk_number, context)
    @context = context
    @disk_number = disk_number
    Log4r::Logger['agent'].info "assembling /dev/md#{disk_number} [#{id}]"
    handle()
  end
  
  def handle(message = nil)
    case @state
      when STATE_START
        raid_state = Raid::check_raid(@disk_number)
        if raid_state == :active
          Log4r::Logger['agent'].debug "RAID array is already active, finished [#{id}]"
          notify_finished()
          finish()
        elsif raid_state == :stopped
          change_state(STATE_CHECKING_EXPORTS)
          set_timeout(1)
        else
          Log4r::Logger['agent'].warn "/dev/md#{disk_number} exists, but is in unknown state [#{id}]"
          notify_failure(:raid_active_unknown_state)
          error()
        end
        
      when STATE_CHECKING_EXPORTS
        exports = Raid::check_aoe(@disk_number)
        if exports.empty?
          Log4r::Logger['agent'].warn "no visible AoE exports for disk #{@disk_number} [#{id}]"
          notify_failure(:no_aoe_exports)
          error()
        else
          result = Raid::assemble_raid(@disk_number, exports)
          if result
            notify_finished()
            finish()
          else
            Log4r::Logger['agent'].warn "failed to assemble /dev/md#{@disk_number} [#{id}]"
            notify_failure(:unknown_error)
            error()
          end
        end
    end
  end
  
  private
  
  def check_if_already_assembled()
    Raid::check_raid(@disk_number)
  end

  def notify_finished()
    msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([
              [:start, [:raid, @disk_number]], [:finished]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  
  def notify_failure(err)
    msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([
              [:start, [:raid, @disk_number]], [err]
        ]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
end
