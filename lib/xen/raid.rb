class Raid
  def self.check_aoe(disk_id)
    exports = []
    _, res = systemu "aoe-stat"
    lines = res.split("\n")
    lines.each do |line|
      l = line.split(" ")
      if l.first =~ /e#{disk_id}\.\d/
        exports << l.first if l[4] == 'up'
      end
    end

    exports
  end
 
  def self.check_raid(disk_id)
    _, res = systemu "cat /proc/mdstat | grep md#{disk_id}"
    lines = res.split("\n")
    return :stopped if lines.blank?

    line = lines.first
    return line.split(" ")[2] == "active" ? :active : :failed
  end
  
  def self.stop_raid(disk_id)
    cmd = "mdadm -S /dev/md#{disk_id}"
    puts cmd
    _, out, err = systemu(cmd)
    puts out
    puts err
    return err.blank? || err.include?("stopped ")
  end
  
  def self.assemble_raid(disk_id, exports)
    devices = exports_to_aoe_devices(exports)
    cmd = "mdadm --assemble /dev/md#{disk_id} " + devices.join(' ') + " --run"
    puts cmd
    _, out, err = systemu(cmd)
    puts out
    puts err
    err.blank? || err.include?("has been started")
  end

  def self.create_raid(disk_number, exports)
    devices = exports_to_aoe_devices(exports)
    devices << "missing" if exports.size < 2
    cmd = "mdadm --create /dev/md#{disk_number} --force --run --level=1 --raid-devices=2 -binternal --bitmap-chunk=1024 --metadata=1.2 " + devices.join(' ')
    Log4r::Logger['os'].info(cmd)
    _, out, err = systemu(cmd)
    Log4r::Logger['os'].debug("stdout: " + out)
    Log4r::Logger['os'].debug("stderr: " + err)

    err =~ /array \/dev\/md#{disk_number} started/
  end

  private

  def self.exports_to_aoe_devices(exports)
    exports.map {|e| '/dev/etherd/' + e}
  end
end
