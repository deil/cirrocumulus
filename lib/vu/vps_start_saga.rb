class VpsStartSaga < Saga
  attr_reader :vps_id

  STATE_CHECKING_IF_ALREADY_RUNNING = 1
  STATE_SEARCHING_SUITABLE_NODE = 2
  STATE_ASSEMBLING_RAID = 3
  STATE_STARTING_DOMU = 4

  def start(vps_id, context)
    @context = context
    @vps_id = vps_id
    @nodes = []
    @selected_node = nil
    @raid_devices = []
    @current_raid_device = nil
    @vps = VpsConfiguration.find(@vps_id)

    handle(nil)
  end

  def handle(message)
    case @state
      when STATE_START
        check_already_running()
        change_state(STATE_CHECKING_IF_ALREADY_RUNNING)
        set_timeout(DEFAULT_TIMEOUT)
        
      when STATE_CHECKING_IF_ALREADY_RUNNING
        if message.nil? # no nodes replied, nowhere to go!
          find_suitable_node()
          change_state(STATE_SEARCHING_SUITABLE_NODE)
          set_timeout(DEFAULT_TIMEOUT)
          #error()
        else
          status = message.content.first
          
          #if status == :not
            #find_suitable_node()
            #change_state(STATE_SEARCHING_SUITABLE_NODE)
            #set_timeout(DEFAULT_TIMEOUT)
          if status == :running
            already_running(message.content)
            finish()
          #else # unknown reply, WTF
            #error()
          end
        end

      when STATE_SEARCHING_SUITABLE_NODE
        if message.nil?
          select_node()
        else
          store_node_info(message)
        end

      when STATE_ASSEMBLING_RAID
        if message.nil?
          Log4r::Logger['agent'].error "no reply from node while assembling /dev/md#{@current_raid_device[:disk_number]} [#{id}]"
          error()
        else
          if message.sender == @selected_node[:name]
            if message.act == 'inform' # add more checks!!!
              @current_raid_device[:status] = :success
              Log4r::Logger['agent'].debug "[#{id}] RAID device /dev/md#{@current_raid_device[:disk_number]} was assembled successfully"
              @current_raid_device = assemble_next_raid(@current_raid_device)
              if @current_raid_device.nil?
                Log4r::Logger['agent'].debug "[#{id}] all RAID devices are assembled, starting domU"
                change_state(STATE_STARTING_DOMU)
                start_domu_on_selected_node()
                set_timeout(LONG_TIMEOUT)
              end
            elsif message.act == 'failure' || message.act == 'refuse'
              Log4r::Logger['agent'].warn "failed to assemble /dev/md#{@current_raid_device[:disk_number]} [#{id}]"
              @current_raid_device[:status] = :failed
            end
          end
        end
        
      when STATE_STARTING_DOMU
        if message.nil?
          update_vps_state()
          notify_finished()
          finish()
        else
          if message.sender == @selected_node[:name]
            if message.act == 'failure' || message.act == 'refuse'
              process_failure()
            else # add extra check
              update_vps_state()
              notify_finished()
              finish()
            end
          end
        end
    end
  end

  private

  def check_already_running()
    message = Cirrocumulus::Message.new(nil, 'query-if', Sexpistol.new.to_sexp([:running, [:domu, @vps.vps_id]]))
    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end

  def already_running(content)
    Log4r::Logger['agent'].info "VPS is already running [#{id}]"
    notify_already_running()
  end

  def find_suitable_node()
    Log4r::Logger['agent'].debug "[#{id}] querying available nodes"
    message = Cirrocumulus::Message.new(nil, 'query-ref', Sexpistol.new.to_sexp([:free_memory]))
    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end

  def store_node_info(message)
    node = {:name => message.sender, :ram => message.content[2].first, :attempt_failed => false}
    Log4r::Logger['agent'].debug "[#{id}] found node #{node[:name]} with #{node[:ram]}Mb of free RAM"
    @nodes << node
    @nodes = @nodes.sort_by {|n| n[:ram]}.reverse
  end
  
  def find_next_node()
    #puts @nodes.inspect
    node = @nodes.find {|n| n[:ram] >= @vps.current.ram && n[:attempt_failed] == false}
  end

  def select_node()
    clear_timeout()

    Log4r::Logger['agent'].debug "[#{id}] got information about #{@nodes.size} node(s)"
    @selected_node = find_next_node()
    
    if @selected_node
      Log4r::Logger['agent'].info "[#{id}] using #{@selected_node[:name]} to start VPS #{@vps.id}"
      Log4r::Logger['agent'].debug "[#{id}] assembling RAID devices"
      @raid_devices = @vps.storage_disks.map {|d| {:disk_number => d.disk_number, :status => :idle}}
      change_state(STATE_ASSEMBLING_RAID)
      assemble_raids_on_node()
    else
      Log4r::Logger['agent'].warn "[#{id}] we're f#cked up: nowhere to run this VPS"
      notify_failure()
      error()
    end
  end
  
  def assemble_raids_on_node()
    @current_raid_device = @raid_devices.first
    @current_raid_device[:status] = :requested
    start_raid(@selected_node[:name], @current_raid_device[:disk_number])
    set_timeout(DEFAULT_TIMEOUT)
  end
  
  def assemble_next_raid(current_raid)
    device = @raid_devices.find {|d| d[:status] == :idle}
    if device
      start_raid(@selected_node[:name], device[:disk_number])
      set_timeout(DEFAULT_TIMEOUT)
      return device
    end
    
    nil
  end

  def start_raid(node, disk_number)
    msg = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp([:start, [:raid, disk_number]]))
    msg.ontology = 'cirrocumulus-xen'
    msg.receiver = node
    msg.reply_with = @id
    @cm.send(msg)
  end

  def start_domu_on_selected_node()
    disks = StorageDisksVpsConfiguration.find_all_by_vps_configuration_id(@vps.id).map {|disk|
      [disk.block_device, disk.storage_disk.disk_number]
    }
    
    message = Cirrocumulus::Message.new(nil, 'request', Sexpistol.new.to_sexp(
    [
      :start,
      [:domu,
        [:id, @vps.id],
        [:name, @vps.vps_id],
        [:mem, @vps.current.ram],
        [:vcpus, @vps.current.vcpus],
        [:cpu_weight, @vps.current.cpu_weight],
        [:cpu_cap, @vps.current.cpu_cap],
        [:disks, disks]
      ]
    ]))

    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    message.receiver = @selected_node[:name]
    @cm.send(message)
  end

  def notify_already_running()
    msg = Cirrocumulus::Message.new(nil, 'refuse', Sexpistol.new.to_sexp([[:start_vps, [:id, @vps.id]], [:already_running]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  
  def process_failure()
    @selected_node[:attempt_failed] = true
    Log4r::Logger['agent'].warn "[#{id}] failed to start VPS=#{@vps.id} on #{@selected_node[:name]}"
    select_node()
  end

  def notify_failure()
    msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([[:start_vps, [:id, @vps.id]], [:unknown_reason]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end

  def notify_finished()
    msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([[:start_vps, [:id, @vps.id]], [:finished]]))
    msg.ontology = @agent.default_ontology
    msg.receiver = @context.sender
    msg.in_reply_to = @context.reply_with
    @cm.send(msg)
  end
  
  def update_vps_state()
    state = VpsState.find_by_vps_configuration_id(@vps.id)
    state = VpsState.create(:vps_configuration_id => @vps.id, :vnc => false, :maintenance => false, :running => true, :running_on => @selected_node[:name]) if state.nil?
    state.running_on = @selected_node[:name]
    state.running = true
    state.save
  end
end
