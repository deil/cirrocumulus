class RaidCreateSaga < Saga

  STATE_CREATING_DISKS = 1

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
        @@storages.each do |storage|
          @results << {:storage => storage, :status => :requested}
          create_disk(@disk_number, @size, storage)
        end
        change_state(STATE_CREATING_DISKS)
        set_timeout(LONG_TIMEOUT)

      when STATE_CREATING_DISKS
        if message.nil? # timeout
          p @results
          finish()
        else
          storage = @results.find {|s| s[:storage] == message.sender}
          if storage
            if message.act == "inform"
              storage[:status] = :success
            elsif message.act == "failure" || message.act == "refuse"
              storage[:status] = :failed
              Log4r::Logger['agent'].warn "[#{id}] FATAL: failed to create volume vd#{@disk_number} on #{message.sender}"
              notify_failure()
              error()
            end
          end
        end
    end
  end

  private

  @@storages = ['c001s1-storage', 'c001s2-storage']

  def create_disk(disk_number, size, storage)
    Log4r::Logger['agent'].debug "[#{id}] creating volume on #{storage}"
    msg = Cirrocumulus::Message.new(nil, 'request', [:create, [:raid, [:disk_number, disk_number], [:size, size]]])
    msg.ontology = 'cirrocumulus-storage'
    msg.reply_with = @id
    msg.receiver = storage
    @cm.send(msg)
  end

  def notify_failure()
    msg = Cirrocumulus::Message.new(nil, 'failure', [[:create, [:raid, [:disk_number, @disk_number], [:size, @size]]], [:unknown_reason]])
    msg.ontology = 'cirrocumulus-xen'
    msg.in_reply_to = @context.reply_with
    msg.receiver = @context.sender
    @cm.send(msg)
  end
end
