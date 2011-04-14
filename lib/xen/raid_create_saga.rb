class RaidCreateSaga < Saga

  STATE_CREATING_DISKS = 1
  STATE_ADDING_EXPORTS = 2
  STATE_CREATING_RAID = 3

  def start(disk_number, size, context)
    @context = context
    @disk_number = disk_number
    @size = size
    Log4r::Logger['agent'].info "[#{id}] creating new RAID device: /dev/md#{disk_number}"
    handle()
  end

  def handle(message = nil)
    case @state
      when STATE_START
        @results = []
        storages.each do |storage|
          @results << {:storage => storage, :status => :requested}
          create_disk(@disk_number, @size, storage)
        end
        change_state(STATE_CREATING_DISKS)
        set_timeout(LONG_TIMEOUT)

      when STATE_CREATING_DISKS
        if message.nil? # timeout
          p @results
          Log4r::Logger['agent'].warn "[#{id}] FATAL: timeout while creating volumes"
          notify_failure()
          error()
        else
          storage = @results.find {|s| s[:storage] == message.sender}
          if storage
            if message.act == "inform"
              storage[:status] = :success
            elsif message.act == "failure" || message.act == "refuse"
              storage[:status] = :failed
              Log4r::Logger['agent'].warn "[#{id}] FATAL: failed to create volume vd#{@disk_number} on #{message.sender}"
              notify_failure(:volume_create_failed)
              error()
            end

            if @results.all? {|r| r[:status] == :success }
              Log4r::Logger['agent'].info "[#{id}] all volumes created"
              @results = []
              storages.each do |storage|
                @results << {:storage => storage, :status => :requested}
                add_export(@disk_number, storage)
              end
              change_state(STATE_ADDING_EXPORTS)
              set_timeout(LONG_TIMEOUT)
            end
          end
        end

      when STATE_ADDING_EXPORTS
        if message.nil?
          notify_failure()
          error()
        else
          storage = @results.find {|s| s[:storage] == message.sender}
          if storage
            if message.act == 'inform'
              storage[:status] = :success
            elsif message.failed?
              storage[:status] = :failed
              Log4r::Logger['agent'].warn "[#{id}] FATAL: failed to add export for vd#{@disk_number} on #{message.sender}"
              notify_failure(:add_export_failed)
              error()
            end

            if @results.all? {|r| r[:status] == :success }
              Log4r::Logger['agent'].info "[#{id}] exports added successfully"
              change_state(STATE_CREATING_RAID)
              set_timeout(1)
            end
          end
        end

      when STATE_CREATING_RAID
        exports = Raid::check_aoe(@disk_number)
        if exports.size == storages.size
          if Raid::create_raid(@disk_number, exports)
            Log4r::Logger['agent'].info "[#{id}] array /dev/md#{@disk_number} created successfully"
            notify_finish()
            finish()
          else
            Log4r::Logger['agent'].warn "[#{id}] FATAL: failed to create RAID array /dev/md#{@disk_number}"
            notify_failure(:mdadm_failed)
            error()
          end
        else
          Log4r::Logger['agent'].warn "[#{id}] only this exports are visible: #{exports}, but expected count is #{storages.size}"
          notify_failure(:incorrect_aoe_exports)
          error()
        end
    end
  end

  private

  def storages
    @agent.network_map.agents.find_all {|a| a.default_ontology == 'cirrocumulus-storage'}.map {|s| s.identifier }
  end

  def create_disk(disk_number, size, storage)
    Log4r::Logger['agent'].debug "[#{id}] creating volume on #{storage}"
    msg = Cirrocumulus::Message.new(nil, 'request', [:create, [:volume, [:disk_number, disk_number], [:size, size]]])
    msg.ontology = 'cirrocumulus-storage'
    msg.reply_with = @id
    msg.receiver = storage
    @cm.send(msg)
  end

  def add_export(disk_number, storage)
    storage =~ /c001s(\d)/
    slot = $1.to_i #+ 1
    Log4r::Logger['agent'].debug "[#{id}] adding export on #{storage} as e#{disk_number}.#{slot}"
    msg = Cirrocumulus::Message.new(nil, 'request', [:create, [:export, [:disk_number, disk_number], [:slot, slot]]])
    msg.ontology = 'cirrocumulus-storage'
    msg.reply_with = @id
    msg.receiver = storage
    @cm.send(msg)
  end

  def notify_failure(reason = :unknown_reason)
    msg = Cirrocumulus::Message.new(nil, 'failure', [[:create, [:raid, [:disk_number, @disk_number], [:size, @size]]], [reason]])
    msg.ontology = 'cirrocumulus-xen'
    msg.in_reply_to = @context.reply_with
    msg.receiver = @context.sender
    @cm.send(msg)
  end

  def notify_finish()
    msg = Cirrocumulus::Message.new(nil, 'inform', [[:create, [:raid, [:disk_number, @disk_number], [:size, @size]]], [:finished]])
    msg.ontology = 'cirrocumulus-xen'
    msg.in_reply_to = @context.reply_with
    msg.receiver = @context.sender
    @cm.send(msg)
  end
end
