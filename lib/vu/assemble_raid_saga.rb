class AssembleRaidSaga < Saga

  STATE_CHECKING_ALREADY_RUNNING = 1
  STATE_CHECKING_EXPORTS = 2
  STATE_WAITING_FOR_REPLY = 3

  def start(node, disk_number, context)
    @context = context
    @selected_node = node
    @disk_number = disk_number

    Log4r::Logger['agent'].info "assembling RAID #{@disk_number} on #{@selected_node} [#{id}]"
    handle(nil)
  end

  def handle(message)
    
  end
end
