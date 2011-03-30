class PingVpsesSaga < Saga

  STATE_WAITING_FOR_REPLY = 1

  def start()
    Log4r::Logger['agent'].info "pinging all VPSes [#{id}]"
    
    @vpses = VpsConfiguration.all(:conditions => {:is_active => true})
    @vps_statuses = @vpses.map {|v| {:vps_id => v.id, :vps_uid => v.vps_id, :statuses => []} }
    @vpses.each {|v| check_running(v.vps_id)}
    change_state(STATE_WAITING_FOR_REPLY)
    set_timeout(60)
  end

  def handle(message)
    case @state 
      when STATE_WAITING_FOR_REPLY
        if !message.nil?
          info = nil
          is_running = false
          
          if message.content.first == :running
            info = message.content.second
            is_running = true
          elsif message.content.first == :not
            info = message.content.second.second
            is_running = false
          end
          
          domU = info.second
          node = message.sender
          @vps_statuses.each {|v|
            next if v[:vps_uid] != domU
            v[:statuses] << {:is_running => is_running, :node => node}

            if is_running
              #Log4r::Logger['agent'].info "VPS #{v[:vps_id]} is running on #{node} [#{id}]"
              update_vps_state(v[:vps_id], true, message.sender)
            end
          }
        else
          @vps_statuses.each {|v|
            if v[:statuses].all? {|s| s[:is_running] != true}
              Log4r::Logger['agent'].info "VPS #{v[:vps_id]} is not running [#{id}]"
              update_vps_state(v[:vps_id], false, nil)
            end
          }
          
          finish()
        end
    end
  end

  private

  def check_running(vps_id)
    message = Cirrocumulus::Message.new(nil, 'query-if', Sexpistol.new.to_sexp([:running, [:domu, vps_id]]))
    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end

  def update_vps_state(vps_id, is_running, node)
    state = VpsState.find_by_vps_configuration_id(vps_id)
    state = VpsState.create(:vps_configuration_id => vps_id, :vnc => false, :maintenance => false, :running => is_running, :running_on => node) if state.nil?
    state.running_on = node
    state.running = is_running
    state.save
  end
end
