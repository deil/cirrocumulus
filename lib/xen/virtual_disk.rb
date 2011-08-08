class VirtualDisk
  def self.check_state(disk_number)
    _, res = systemu "cat /proc/mdstat | grep md#{disk_number}"
    lines = res.split("\n")
    return :disconnected if lines.blank?

    line = lines.first
    return line.split(" ")[2] == "active" ? :connected : :failed
  end
end
