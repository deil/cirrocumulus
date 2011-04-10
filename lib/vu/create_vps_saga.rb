class CreateVpsSaga < Saga

  STATE_SEARCHING_SUITABLE_NODE = 1
  STATE_CREATING_RAID = 2

  def start(vps_id, name, mem, distro, disk_number, disk_size, context)
    @context = context
    @vps = {
        :id => vps_id, :uid => name, :ram => mem,
        :disk => [{:disk_number => disk_number, :size => disk_size}],
        :distro => distro
    }

    Log4r::Logger['agent'].info "[#{id}] creating vps ID=#{vps_id} NAME=#{name} MEM=#{mem}Mb DISK=#{disk_size}Gb"
    handle()
  end

  def handle(message = nil)
    case @state
      when STATE_START
        @nodes = []
        find_suitable_node()
        change_state(STATE_SEARCHING_SUITABLE_NODE)
        set_timeout(DEFAULT_TIMEOUT)

      when STATE_SEARCHING_SUITABLE_NODE
        if message.nil? # timeout
          @selected_node = select_next_node()
          if @selected_node
            Log4r::Logger['agent'].info "[#{id}] using node #{@selected_node[:name]}"
            Log4r::Logger['agent'].debug "[#{id}] creating RAID device /dev/md#{@vps[:disk].first[:disk_number]}"
            create_raid(@vps[:disk].first[:disk_number], @vps[:disk].first[:size])
            change_state(STATE_CREATING_RAID)
            set_timeout(LONG_TIMEOUT)
          else
            Log4r::Logger['agent'].error "[#{id}] FATAL: no available nodes (or every node failed)"
            error()
          end
        elsif message.act == 'inform'
          store_node_info(message)
        end

      when STATE_CREATING_RAID
        if message.nil? # timeout
          finish()
        elsif message.sender == @selected_node[:name]
          if message.act == 'failure' || message.act == 'refuse'
            Log4r::Logger['agent'].warn "[#{id}] FATAL: failed to create RAID array"
            notify_failure()
            error()
          end
        end
    end
  end

  private

  def find_suitable_node()
    Log4r::Logger['agent'].debug "[#{id}] querying available nodes"

    message = Cirrocumulus::Message.new(nil, 'query-ref', [:free_memory])
    message.reply_with = @id
    message.ontology = 'cirrocumulus-xen'
    @cm.send(message)
  end

  def store_node_info(message)
    node = {:name => message.sender, :ram => message.content[2].first.to_i, :attempt_failed => false}
    Log4r::Logger['agent'].debug "[#{id}] found node #{node[:name]} with #{node[:ram]}Mb of free RAM"
    @nodes << node
    @nodes = @nodes.sort_by {|n| n[:ram]}.reverse
  end

  def select_next_node
    @nodes.find {|node| node[:ram] > @vps[:ram] && node[:attempt_failed] == false}
  end

  def create_raid(disk_number, size)
    msg = Cirrocumulus::Message.new(nil, 'request', [:create, [:raid, [:disk_number, disk_number], [:size, size]]])
    msg.ontology = 'cirrocumulus-xen'
    msg.reply_with = id
    @cm.send(msg)
  end

  def notify_failure()
    msg = Cirrocumulus::Message.new(nil, 'failure', [[:create, [:vps, [:id, @vps[:id]]]], [:unknown_reason]])
    msg.ontology = 'cirrocumulus-vps'
    msg.in_reply_to = @context.reply_with
    msg.receiver = @context.sender
    @cm.send(msg)
  end

end
